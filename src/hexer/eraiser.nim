#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Implements the core of exception handling.
- We transform `result = x` to `result = (Success, x)`, likewise `return`.
- We translate `proc p(params): T {.raises.}` to `proc p(params): (ErrorCode, T)`.
- We transform `let/var local: T = rcall(args)` to `let/var local: (ErrorCode, T) = rcall(args);
  if local[0] != Success: raise (local[0], result)`
  and other usages of `local` to `local[1]`.
- We transform other `rcalls` to `let tmp = rcall(args); if tmp[0] != Success: raise (tmp[0], result)`
- We transform `raise e` to `raise (e, result)`.
- Destroyer's job is to replicate `finally` sections and destructors for every side exit like `raise`,
  `return` and `break`.
- nifcgen's job is to translate `raise` either to a `goto errHandler` or to a `return`, depending on
  whether the `raise` is inside a `try` block or not.

There is a classic phase ordering problem here: We want to introduce `raise` statements before destructor
injections so that resource cleanup is correctly done. But we want to introduce the tuples later so that
we don't end up producing lots of destructors for `(ErrorCode, T)` tuples which only delegate to the `T`
anyway and need to be inlined and removed. So instead of producing `if e != Success: raise (e, result)` we
produce `let tmp = call(); if failed(tmp): raise tmp` and introduce the tuples later.

We also produce the required temporaries (as cursors so that we don't get even more copies).

]##

import std / [sets, tables, hashes, assertions, syncio]

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typeprops, reporters]
import ".." / models / tags
import duplifier, passes
include ".." / nimony / nif_annotations

type
  Context = object
    ptrSize, tmpCounter: int
    typeCache: TypeCache
    hoisted: TokenBuf
      ## Statements that must run *before* the statement currently being
      ## emitted: the `canRaise` temp of a raising call plus its
      ## `if failed(tmp): raise tmp` check. Emitting them here — instead of
      ## wrapping them into an `(expr (stmts ...) tmp)` in expression position —
      ## is what keeps the eraiser's output statement-based, so no follow-up
      ## `xelim` run is needed to flatten it again (see `doc/final_ir.md`).

proc hoistTail(c: var Context; dest: var TokenBuf; pos: int) =
  ## Move `dest[pos ..< ^0]` — a sequence of complete statements — in front of
  ## the statement currently being translated.
  if dest.len <= pos: return
  var tail = cursorAt(dest, pos)
  while tail.hasMore:
    takeTree c.hoisted, tail
  endRead tail
  dest.shrink pos

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
    {.ensuresNif: addedAny(dest).}

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  var c2 = Context(ptrSize: c.ptrSize, typeCache: move(c.typeCache),
                   hoisted: createTokenBuf(16))

  copyInto(dest, n):
    let isConcrete = c2.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c2.typeCache.registerLocal(symId, r.kind, r.params)
      c2.typeCache.openScope()
      tr c2, dest, n
      c2.typeCache.closeScope()
    else:
      takeTree dest, n
  c.typeCache = move(c2.typeCache)

proc addRaiseStmt(dest: var TokenBuf; target: SymId; info: NifLineInfo) =
  copyIntoKind dest, IfS, info:
    copyIntoKind dest, ElifU, info:
      copyIntoKind dest, FailedX, info:
        dest.addSymUse target, info
      copyIntoKind dest, StmtsS, info:
        copyIntoKind dest, RaiseS, info:
          dest.addSymUse target, info

proc collectTupleLocals(n: var Cursor; hasRaisesPragma: var bool; res: var HashSet[SymId]) =
  case n.kind
  of TagLit:
    if n.exprKind == FailedX and n.childCursor.kind == Symbol:
      res.incl n.childCursor.symId
      n.into:
        while n.hasMore: collectTupleLocals(n, hasRaisesPragma, res)
    elif n.pragmaKind == RaisesP:
      hasRaisesPragma = true
      n.into:
        while n.hasMore: collectTupleLocals(n, hasRaisesPragma, res)
    elif n.stmtKind == ResultS and n.childCursor.kind == SymbolDef:
      if hasRaisesPragma:
        res.incl n.childCursor.symId
      n.into:
        while n.hasMore: collectTupleLocals(n, hasRaisesPragma, res)
    elif n.symKind in RoutineKinds:
      # do not descend into nested routines
      skip n
    else:
      n.into:
        while n.hasMore: collectTupleLocals(n, hasRaisesPragma, res)
  else:
    inc n

proc localsThatBecomeTuples*(n: Cursor): HashSet[SymId] =
  # n must be a routine body!
  result = initHashSet[SymId]()
  var n = n
  var hasRaisesPragma = false
  collectTupleLocals(n, hasRaisesPragma, result)

proc callCanRaise*(typeCache: var TypeCache; n: Cursor): bool =
  var fnType = skipProcTypeToParams(getType(typeCache, n.childCursor))
  if fnType.tagEnum != ParamsTagId:
    raiseAssert "BUG eraiser callCanRaise: callee type not params at " & infoToStr(n.info) &
         ": " & toString(getType(typeCache, n.childCursor), false)
  skip fnType # params
  skip fnType # return type
  # now pragmas follow:
  result = hasPragma(fnType, RaisesP)

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; inhibit: bool) =
  let head = n.load()
  let info = n.info
  let callStart = n # skip `(call)`
  n = sub(n)
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))
  if fnType.tagEnum != ParamsTagId:
    raiseAssert "BUG eraiser trCall: callee type not params at " & infoToStr(info) &
         ": " & toString(getType(c.typeCache, n), false)
  skip fnType # params
  let retType = fnType
  skip fnType # return type
  # now pragmas follow:
  let canRaise = hasPragma(fnType, RaisesP)
  if canRaise and not inhibit:
    let isVoid = retType.isDotToken or retType.typeKind == VoidT
    let hoistPos = dest.len
    # A void raising call is already in statement position, so its temp and
    # check stay put, wrapped in a `(stmts ...)`. A value-returning one is an
    # operand: its statements are hoisted in front of the enclosing statement
    # and only the temp's name is left behind.
    if isVoid:
      dest.addParLe(StmtsS, info)
    block:
      let symId = pool.syms.getOrIncl("`canRaise." & $c.tmpCounter)
      inc c.tmpCounter
      # The temp holds the call's RETURN VALUE — an owned, freshly
      # constructed value — so it must be an owning local. As a CursorS the
      # duplifier could only `=dup` out of it (a cursor is a borrow) and the
      # destroyer never released it: every `dest = raisingCall()` leaked the
      # entire returned value once per call.
      copyIntoKind dest, VarS, info:
        addSymDef dest, symId, info
        dest.addEmpty2 info # export marker, pragma
        copyTree dest, retType
        # value is the call expression:
        dest.addParLe(head.tagId, info)
        while n.hasMore:
          tr c, dest, n
        dest.addParRi(n.endInfo)
        n = callStart; skip n
      addRaiseStmt(dest, symId, info)
      if isVoid:
        dest.addParRi()
      else:
        hoistTail(c, dest, hoistPos)
        dest.addSymUse symId, info
  else:
    dest.addParLe(head.tagId, info)
    while n.hasMore:
      tr c, dest, n
    dest.addParRi(n.endInfo)
    n = callStart; skip n

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    let target = n.symId
    c.typeCache.takeLocalHeader(dest, n, kind)
    let cr = n.exprKind in CallKinds and callCanRaise(c.typeCache, n)
    if cr:
      trCall c, dest, n, true
    else:
      tr c, dest, n
  if cr:
    addRaiseStmt(dest, target, n.endInfo)

proc trAssign(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    tr c, dest, n # left hand side
    tr c, dest, n # right hand side

proc trStmtList(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## The statement-insertion point: whatever `trCall` collected in
  ## `c.hoisted` while translating one statement goes in front of it.
  ## An enclosing statement's own hoists are parked across the descent.
  ## Both `(stmts ...)` and `(scope ...)` are statement lists, so both must
  ## land the hoists here: a `scope` that only recursed would push its
  ## children's temps out to the enclosing `stmts`, in front of the very
  ## locals they read ("could not find symbol").
  var outerHoisted = createTokenBuf(16)
  swap(outerHoisted, c.hoisted)
  copyInto dest, n:
    while n.hasMore:
      let stmtStart = dest.len
      tr c, dest, n
      if c.hoisted.len > 0:
        # `stmtStart` is past every still-open tag, so the splice cannot
        # invalidate an enclosing scope's bookkeeping.
        dest.insert(c.hoisted, stmtStart)
        c.hoisted.shrink 0
  swap(c.hoisted, outerHoisted)

proc trScope(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  trStmtList c, dest, n
  c.typeCache.closeScope()

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol, SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StrLit, UnknownToken, DotToken, EofToken:
    takeTree dest, n
  of TagLit:
    let ek = n.exprKind
    case ek
    of CallKinds:
      trCall c, dest, n, false
    of TypeofX:
      takeTree dest, n
    else:
      case n.stmtKind
      of AsgnS:
        trAssign c, dest, n
      of ProcS, FuncS, MethodS, ConverterS:
        trProcDecl c, dest, n
      of LocalDecls:
        trLocal c, dest, n
      of ScopeS:
        trScope c, dest, n
      of MacroS, TemplateS, TypeS:
        takeTree dest, n
      of StmtsS:
        trStmtList c, dest, n
      of CallS, CmdS, IteratorS, BlockS, EmitS, IfS, WhenS, BreakS,
         ContinueS, ForS, WhileS, CoroforS, CaseS, RetS, YldS,
         PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS, ImportasS,
         FromimportS, ImportexceptS, ExportS, ExportexceptS, CommentS,
         DiscardS, TryS, RaiseS, UnpackdeclS, AssumeS, AssertS,
         CallstrlitS, InfixS, PrefixS, HcallS, StaticstmtS, BindS,
         MixinS, UsingS, AsmS, DeferS, LabS, JmpS, NoStmt:
        # generic container: copy the head and recurse into the children
        copyInto dest, n:
          while n.hasMore:
            tr c, dest, n
  else:
    raiseAssert "BUG: unexpected ParRi in eraiser.tr" # classic ParRi only

proc injectRaisingCalls*(pass: var Pass; ptrSize: int) =
  var n = pass.n  # Extract cursor locally
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache(),
                  hoisted: createTokenBuf(16))
  c.typeCache.openScope()
  tr(c, pass.dest, n)  # Write to pass.dest
  c.typeCache.closeScope()

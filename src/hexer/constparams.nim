#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[

Implements "pass by const" by introducing more hidden pointers.
We do this right before inlining and before codegen as it interacts with the
codegen's `maybeByConstRef` logic — and, unlike the raise lowering that used to
share this file, it cannot move ahead of `cps`: the state procs and init
wrappers `cps` GENERATES have const-ref params of their own and need the same
derefs as everyone else's. See `eraiser.nim`, which absorbed the half that left.

]##

import std / [sets, tables, hashes, assertions, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typeprops, builtintypes]
import ".." / models / tags
import duplifier, passes
include ".." / nimony / nif_annotations

type
  Context = object
    constRefParams: HashSet[SymId]
    ptrSize, tmpCounter: int
    typeCache: TypeCache
    sizeofCache: SizeofCache  ## shared size-by-symbol memoization
    needsXelim: bool
    keepOverflowFlag: bool

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
    {.ensuresNif: addedAny(dest).}

proc passByConstRef(typ, pragmas: Cursor; ptrSize: int;
                    cache: var SizeofCache): bool =
  result = sizeof.passByConstRef(typ, pragmas, ptrSize, cache) or
           typeprops.isInheritable(typ, false)

proc passByConstRef(c: var Context; typ, pragmas: Cursor): bool =
  result = passByConstRef(typ, pragmas, c.ptrSize, c.sizeofCache)

type
  ArgRole* = enum
    argPlain        ## nothing to do here beyond walking into it
    argConstRef     ## has to arrive as an address
    argCompileTime  ## `typedesc`/`static`: a value with no runtime existence

proc nextArgRole*(fnType: var Cursor; ptrSize: int; cache: var SizeofCache): ArgRole =
  ## Classify the next actual against the formal parameter list, advancing
  ## `fnType` past the formal it consumed.
  ##
  ## Answers "how does this actual reach the callee?" — and `argConstRef` is
  ## the interesting one: the callee gets an ADDRESS, so the actual needs
  ## storage to have an address of.
  ##
  ## Two walkers need this and must agree exactly, because both walk formals
  ## and actuals in step: `trCall` below, and `coro_transform`'s lifetime
  ## extension, which asks the same question a pass earlier to find out what a
  ## coroutine has to keep in its frame. A rule one of them has and the other
  ## lacks does not fail — it silently pairs an argument with the wrong
  ## parameter from that point on. So the rules live here once: a `varargs`
  ## formal serves every remaining actual and must not be advanced past; a
  ## closure's environment actual has no formal at all; and the const-ref
  ## question is asked before the compile-time one. What the two walkers *do*
  ## with a role is where they are allowed to differ.
  if not fnType.hasMore: return argPlain
  assert fnType.isTagLit
  let previousFormalParam = fnType
  let param = takeLocal(fnType, SkipFinalParRi)
  let pk = param.typ.typeKind
  if pk in {MutT, OutT, LentT}:
    result = argPlain
  elif pk == VarargsT:
    fnType = previousFormalParam
    result = argPlain
  elif passByConstRef(param.typ, param.pragmas, ptrSize, cache):
    result = argConstRef
  elif pk in {TypedescT, StaticT}:
    result = argCompileTime
  else:
    result = argPlain

proc rememberConstRefParams(c: var Context; params: Cursor) =
  if not params.isTagLit: return
  var n = params
  n = sub(n) # skips (params; bounds the walk under vpr
  while n.hasMore:
    let r = takeLocal(n, SkipFinalParRi)
    if r.name.kind == SymbolDef and passByConstRef(c, r.typ, r.pragmas):
      c.constRefParams.incl r.name.symId

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  var c2 = Context(ptrSize: c.ptrSize, typeCache: move(c.typeCache),
    sizeofCache: move(c.sizeofCache), needsXelim: c.needsXelim)

  copyInto(dest, n):
    let isConcrete = c2.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c2.typeCache.registerLocal(symId, r.kind, decl)
      c2.typeCache.openScope()
      rememberConstRefParams c2, r.params
      let info = n.info
      copyIntoKind dest, StmtsS, info:
        if n.stmtKind == StmtsS:
          n.into:
            while n.hasMore:
              tr c2, dest, n
        else:
          tr c2, dest, n
      c2.typeCache.closeScope()
    else:
      takeTree dest, n
  c.typeCache = move(c2.typeCache)
  c.sizeofCache = move(c2.sizeofCache)
  c.needsXelim = c2.needsXelim

proc yieldsAddress(n: Cursor): bool =
  ## Does this expression already EVALUATE to an address? Not the same question
  ## as "is its head an `addr`": `coro_transform`'s lifetime extension hands us
  ## `(expr (stmts (var tmp ...)) (haddr tmp))`, whose value is an address even
  ## though the tree it sits in is an `expr`.
  var n = n
  while n.exprKind == ExprX:
    inc n
    while not isLastSon(n): skip n
  result = n.exprKind in {AddrX, HaddrX}

proc trConstRef(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  if yieldsAddress(n):
    # Already carries an address: this call is inside a coroutine, and
    # `coro_transform` gave the argument storage in the FRAME rather than let
    # us put it on a state proc's stack, which dies at the next suspension.
    tr c, dest, n
  elif constructsValue(n):
    # We cannot take the address of a literal so we have to copy it to a
    # temporary first:
    let argType = getType(c.typeCache, n)
    c.needsXelim = true
    copyIntoKind dest, ExprX, info:
      copyIntoKind dest, StmtsS, info:
        let symId = pool.syms.getOrIncl("`constRefTemp." & $c.tmpCounter)
        inc c.tmpCounter
        copyIntoKind dest, VarS, info:
          addSymDef dest, symId, info
          dest.addEmpty2 info # export marker, pragma
          copyTree dest, argType
          # value:
          tr c, dest, n
      copyIntoKind dest, HaddrX, info:
        dest.addSymUse symId, info
  else:
    copyIntoKind dest, HaddrX, info:
      tr c, dest, n

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var fnType = skipProcTypeToParams(getType(c.typeCache, n.childCursor))
  assert fnType.tagEnum == ParamsTagId
  dest.addParLe(n.cursorTagId, n.info)
  n.into: # skip `(call)`
    tr c, dest, n # handle `fn`

    fnType = sub(fnType) # peek only, never left
    while n.hasMore:
      case nextArgRole(fnType, c.ptrSize, c.sizeofCache)
      of argPlain: tr c, dest, n
      of argConstRef: trConstRef c, dest, n
      of argCompileTime: skip n  # a compile-time value produces no code
    dest.addParRi(n.endInfo)

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

proc trScope(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  dest.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      tr c, dest, n
  dest.addParRi()
  c.typeCache.closeScope()

proc trPragmaBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  n.into: # pragmax
    let pragmasStart = n # pragmas
    n = sub(n)
    if n.pragmaKind == KeepOverflowFlagP:
      skip n # keepOverflowFlag
      n = pragmasStart; skip n # pragmas
      let oldKeepOverflowFlag = c.keepOverflowFlag
      c.keepOverflowFlag = true
      tr(c, dest, n)
      c.keepOverflowFlag = oldKeepOverflowFlag
    elif n.pragmaKind == CastP:
      skip n # cast pragma
      n = pragmasStart; skip n # pragmas
      tr(c, dest, n)
    else:
      bug "unknown pragma block: " & toString(n, false)

proc checkedArithOp(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.addParLe(ExprX, info)
  dest.addParLe(StmtsS, info)
  let typ = n.childCursor

  let target = pool.syms.getOrIncl("`constRefTemp." & $c.tmpCounter)
  inc c.tmpCounter
  copyIntoKind dest, VarS, info:
    addSymDef dest, target, info
    dest.addEmpty2 info # export marker, pragma
    copyTree dest, typ
    dest.addDotToken() # value
  dest.addParLe(globalTags.registerTag("keepovf"), info)
  dest.copyInto n:
    tr(c, dest, n) # type
    tr(c, dest, n) # operand A
    tr(c, dest, n) # operand B
  dest.addSymUse target, info
  dest.addParRi() # "keepovf"
  dest.addParRi() # stmts
  dest.addSymUse target, info
  dest.addParRi() # expr
  c.needsXelim = true

proc trObjConstr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  takeInto dest, n:
    takeTree dest, n # type
    while n.hasMore:
      if n.substructureKind == KvU:
        takeInto dest, n:
          takeTree dest, n # key
          tr c, dest, n
          if n.hasMore:
            # optional inheritance
            takeTree dest, n
      else:
        # V-Table:
        takeTree dest, n

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of Symbol:
    if c.constRefParams.contains(n.symId):
      copyIntoKind dest, DerefX, n.info:
        dest.addSubtree n
    else:
      dest.addSubtree n
    inc n
  of SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StrLit, UnknownToken, DotToken, EofToken:
    takeTree dest, n
  of TagLit:
    let ek = n.exprKind
    case ek
    of CallKinds:
      trCall c, dest, n
    of PragmaxX:
      trPragmaBlock c, dest, n
    of AddX, SubX, MulX, DivX, ModX:
      if c.keepOverflowFlag:
        checkedArithOp c, dest, n
      else:
        copyInto dest, n:
          while n.hasMore: tr c, dest, n
    of DotX:
      takeInto dest, n:
        tr c, dest, n
        while n.hasMore:
          dest.takeTree n
    of OconstrX:
      trObjConstr c, dest, n
    else:
      case n.stmtKind
      of ProcS, FuncS, MethodS, ConverterS:
        trProcDecl c, dest, n
      of LocalDecls:
        trLocal c, dest, n
      of ScopeS:
        trScope c, dest, n
      of MacroS, TemplateS, TypeS:
        takeTree dest, n
      of CallS, CmdS, IteratorS, BlockS, EmitS, AsgnS, IfS, WhenS, BreakS,
         ContinueS, ForS, WhileS, CoroforS, CaseS, RetS, RaiseS, TryS,
         YldS, StmtsS,
         PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS,
         ImportasS, FromimportS, ImportexceptS, ExportS, ExportexceptS,
         CommentS, DiscardS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
         InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
         AsmS, DeferS, LabS, JmpS, NoStmt:
        # generic container: copy the head and recurse into the children
        copyInto dest, n:
          while n.hasMore: tr c, dest, n
  else:
    raiseAssert "BUG: unexpected ParRi in constparams.tr" # classic ParRi only

proc injectConstParamDerefs*(pass: var Pass; ptrSize: int; needsXelim: var bool) =
  var n = pass.n  # Extract cursor locally
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache(pass.bits),
                  needsXelim: needsXelim)
  c.typeCache.openScope()
  tr(c, pass.dest, n)  # Write to pass.dest
  c.typeCache.closeScope()
  needsXelim = c.needsXelim

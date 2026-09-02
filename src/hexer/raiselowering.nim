#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

##[
Raise lowering: `.raises` becomes a success tuple.

Nimony's exceptions are checked returns. `eraiser` has already done the
control-flow half — every raising call is a temp plus a `(failed t)` check, and
every `raise` is a statement — and this pass does the value half: a raising
routine's `result` and the temps holding raising calls become
`(ErrorCode, T)` (or a bare `ErrorCode` when there is no value), `raise e`
becomes `raise (e, result)`, and a plain use of one of those locals projects
onto its value half.

**It runs before `cps`, and that is the whole point.** It used to live in
`constparams.nim` and run after — not by design, but because it was written
(#1031, April 2025) two months before there was a CPS pass (#1196) to run
before, and the pass that came later was simply spliced in ahead of it. The
cost showed up the moment a `.passive` routine also raised: `cps` lifts a local
that outlives a state into the coroutine frame, and after that neither the
declaration this pass retypes nor the uses it projects exist any more — the
declaration is an assignment and the uses are `(dot (deref this) fld)`. Every
value crossing that seam was wrong, and `cps` had grown its own copy of this
pass's knowledge to compensate.

Running here, the transform sees one ordinary routine body and `cps` sees a
coroutine that happens to return a tuple. Neither has to know about the other.

What stayed behind in `constparams.nim` is the pass-by-const-ref half, which
genuinely cannot move: it also has to see the state procs and init wrappers
`cps` GENERATES, whose const-ref params need the same derefs as everyone
else's.
]##

import std / [sets, tables, hashes, assertions, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, typeprops, builtintypes]
import ".." / models / tags
import eraiser, passes
include ".." / nimony / nif_annotations

type
  Context = object
    tupleVars: HashSet[SymId]
    exceptVars: seq[SymId]
    tmpCounter: int
    typeCache: TypeCache
    canRaise: bool
    nextRaiseIsSpecial: bool
    resultSym: SymId
    retType: Cursor

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
    {.ensuresNif: addedAny(dest).}

proc takeRaisingHeader(c: var Context; dest: var TokenBuf; decl: Cursor;
                       n: var Cursor): bool =
  ## `typenav.takeRoutineHeader`, plus the signature half of this pass: a
  ## `.raises` routine RETURNS its success tuple.
  ##
  ## The `(raises)` pragma stays: `lengcgen` reads it to decide that a raising
  ## `.noreturn` proc must NOT get C's `noreturn` attribute, because under goto
  ## exceptions such a proc does return — it hands back an error code — and
  ## telling gcc otherwise deletes the callers' error paths. What `lengcgen`
  ## no longer does from the pragma is rewrite THIS return type; it now does
  ## that for proctypes only.
  ##
  ## This used to happen in `lengcgen`, at the very end, which was fine while
  ## nothing between here and there had to know a raising routine's real shape.
  ## `cps` does: it builds a coroutine's frame and its result slot out of the
  ## return type, and it cannot wait for codegen to tell it what that is.
  result = true # assume concrete
  let sym = n.symId
  for i in 0..<BodyPos:
    if i == ParamsPos:
      c.typeCache.registerParams(sym, decl, n)
      takeTree dest, n
    elif i == TypevarsPos:
      result = n.substructureKind != TypevarsU
      takeTree dest, n
    elif i == ReturnTypePos:
      addLengReturnType(dest, n, asRoutine(decl, SkipExclBody).pragmas, n.info)
      skip n
    else:
      takeTree dest, n

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  var c2 = Context(typeCache: move(c.typeCache),
    resultSym: SymId(0), canRaise: hasPragma(r.pragmas, RaisesP),
    retType: r.retType)

  copyInto(dest, n):
    let isConcrete = takeRaisingHeader(c2, dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c2.typeCache.registerLocal(symId, r.kind, decl)
      c2.typeCache.openScope()
      c2.tupleVars = localsThatBecomeTuples(n)
      let info = n.info
      copyIntoKind dest, StmtsS, info:
        if n.stmtKind == StmtsS:
          n.into:
            while n.hasMore:
              tr c2, dest, n
        else:
          tr c2, dest, n
        if c2.canRaise and isVoidType(r.retType):
          copyIntoKind dest, RetS, info:
            dest.addSymUse pool.syms.getOrIncl(SuccessName), info
      c2.typeCache.closeScope()
    else:
      takeTree dest, n
  c.typeCache = move(c2.typeCache)

proc produceSuccessTuple(c: var Context; dest: var TokenBuf; typ: Cursor; info: NifLineInfo): bool =
  if isVoidType(typ):
    result = false
  else:
    dest.addParLe TupconstrX, info
    dest.addParLe TupleT, info
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
    dest.addSubtree typ
    dest.addParRi()
    dest.addSymUse pool.syms.getOrIncl(SuccessName), info
    result = true

proc produceRaiseTuple(c: var Context; dest: var TokenBuf; typ: Cursor; info: NifLineInfo) =
  if not isVoidType(c.retType):
    dest.addParLe TupconstrX, info
    dest.addParLe TupleT, info
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
    dest.addSubtree typ
    dest.addParRi()

proc finishRaiseTuple(c: var Context; dest: var TokenBuf; info: NifLineInfo) =
  if not isVoidType(c.retType):
    if c.resultSym != SymId(0):
      copyIntoKind dest, TupatX, info:
        dest.addSymUse c.resultSym, info
        dest.addIntLit 1, info
    dest.addParRi()

proc trRaise(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let isSpecial = c.nextRaiseIsSpecial
  c.nextRaiseIsSpecial = false
  # Bare `(raise .)` (re-raise) reaches us when derefs lowers a heap-based
  # exception's no-match fall-through. In a `.raises` context we propagate
  # the in-flight exception by signalling `Failure` to the caller; the caller
  # consults the threadvar `exc` for the actual value. Outside a raises proc
  # there is no error channel, so we degrade to a bare `(ret .)`.
  if n.childCursor.kind == DotToken:
    let info = n.info
    skip n # the whole bare `(raise .)`
    if c.canRaise:
      copyIntoKind dest, RaiseS, info:
        produceRaiseTuple(c, dest, c.retType, info)
        dest.addSymUse pool.syms.getOrIncl(FailureName), info
        finishRaiseTuple(c, dest, info)
    else:
      copyIntoKind dest, RetS, info:
        dest.addDotToken()
    return
  let localIsVoid = isVoidType(getType(c.typeCache, n.childCursor))
  if c.exceptVars.len > 0:
    # also bind the value to a potential `T as e` variable:
    let info = n.info
    copyIntoKind dest, AsgnS, info:
      dest.addSymUse c.exceptVars[^1], info
      if isSpecial and not localIsVoid:
        let x = n.childCursor
        assert x.kind == Symbol
        copyIntoKind dest, TupatX, info:
          dest.addSymUse x.symId, info
          dest.addIntLit 0, info
      else:
        dest.addSubtree n.childCursor

  copyInto dest, n:
    produceRaiseTuple c, dest, c.retType, n.info
    if n.kind == Symbol and localIsVoid:
      dest.addSymUse n.symId, n.info
      inc n
    elif isSpecial:
      let info = n.info
      copyIntoKind dest, TupatX, info:
        assert n.kind == Symbol
        dest.addSymUse n.symId, info
        inc n
        dest.addIntLit 0, info
    else:
      tr c, dest, n
    finishRaiseTuple c, dest, n.endInfo

proc trFailed(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `(failed t)` asks a raising call's temp for its error CODE. `takeTree`,
  ## not `tr`: `tr` would answer with the value projection an ordinary use of
  ## the same symbol gets.
  let info = n.info
  n.into:
    if isVoidType(getType(c.typeCache, n)):
      dest.takeTree n   # a void call's temp holds only the code
    else:
      copyIntoKind dest, TupatX, info:
        dest.takeTree n
        dest.addIntLit 0, info
  c.nextRaiseIsSpecial = true

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; targetExpectsTuple: bool) =
  var fnType = skipProcTypeToParams(getType(c.typeCache, n.childCursor))
  assert fnType.tagEnum == ParamsTagId
  var pragmas = fnType
  skip pragmas
  let retType = pragmas
  skip pragmas
  let canRaise = hasPragma(pragmas, RaisesP)
  var needsTuple = (not targetExpectsTuple and canRaise) or
                   (targetExpectsTuple and not canRaise)
  if needsTuple:
    needsTuple = produceSuccessTuple(c, dest, retType, n.info)

  dest.addParLe(n.cursorTagId, n.info)
  n.into: # skip `(call)`
    while n.hasMore: tr c, dest, n
    dest.addParRi(n.endInfo)
  if needsTuple:
    dest.addParRi() # TupconstrX

proc takeLocalHeader(c: var TypeCache; dest: var TokenBuf; n: var Cursor; kind: SymKind; isTuple: bool) =
  let name = n.symId
  takeTree dest, n # name
  takeTree dest, n # export marker
  takeTree dest, n # pragmas
  c.registerLocal(name, kind, n)
  if isVoidType(n) and isTuple:
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.info
    skip n
  else:
    if isTuple:
      dest.addParLe TupleT, n.info
      dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), n.info
    takeTree dest, n # type
    if isTuple:
      dest.addParRi()

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    let symId = n.symId
    var isTuple = c.tupleVars.contains(symId)
    # A void+raises call cursor: takeLocalHeader will change the void type to
    # ErrorCode (scalar, not a tuple). Remove from tupleVars so that *uses*
    # of this symbol are NOT transformed to (tupat sym 1) by tr().
    if isTuple:
      var peek = n
      skip peek # name
      skip peek # export marker
      skip peek # pragmas
      if isVoidType(peek):
        c.tupleVars.excl(symId)
    c.typeCache.takeLocalHeader(dest, n, kind, isTuple)
    if n.exprKind in CallKinds:
      trCall c, dest, n, isTuple
    else:
      tr(c, dest, n)

proc trResultDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  copyInto dest, n:
    c.resultSym = n.symId
    c.typeCache.takeLocalHeader(dest, n, ResultY, c.canRaise)
    tr(c, dest, n)
  # produce `result[0] = Success` statement for initialization:
  if c.canRaise:
    copyIntoKind dest, AsgnS, info:
      copyIntoKind dest, TupatX, info:
        dest.addSymUse c.resultSym, info
        dest.addIntLit 0, info
      dest.addSymUse pool.syms.getOrIncl(SuccessName), info

proc trRet(c: var Context; dest: var TokenBuf; n: var Cursor) =
  if c.canRaise:
    copyInto dest, n:
      if n.kind == DotToken:
        dest.addSymUse pool.syms.getOrIncl(SuccessName), n.info
        inc n
      else:
        let maybeClose = produceSuccessTuple(c, dest, c.retType, n.info)
        tr c, dest, n
        if maybeClose:
          dest.addParRi() # tuple constructor
  else:
    copyInto dest, n:
      tr c, dest, n

proc trScope(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  dest.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      tr c, dest, n
  dest.addParRi()
  c.typeCache.closeScope()

proc trPragmaBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  ## `(pragmax <pragmas> <body>)`: the pragmas are opaque here — the overflow
  ## and cast blocks are `constparams`' business — but the body is ordinary
  ## code and may raise.
  copyInto dest, n:
    dest.takeTree n # pragmas
    while n.hasMore: tr c, dest, n

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # We only deal with the data flow here.
  var nn = n.childCursor
  skip nn # stmts
  let oldLen = c.exceptVars.len
  if nn.substructureKind == ExceptU:
    inc nn
    if nn.stmtKind == LetS:
      copyInto dest, nn:
        let exc = nn.symId
        c.exceptVars.add exc
        c.typeCache.takeLocalHeader(dest, nn, LetY)
        assert nn.isDotToken
        dest.addSubtree nn
        inc nn

  dest.addParLe(n.cursorTagId, n.info)
  n.into:
    tr c, dest, n
    c.exceptVars.shrink oldLen
    while n.substructureKind == ExceptU:
      copyInto dest, n:
        if n.stmtKind == LetS:
          dest.addDotToken() # we moved the declaration before the try statement
          skip n
        else:
          dest.takeTree n
        tr c, dest, n
    if n.substructureKind == FinU:
      copyInto dest, n:
        tr c, dest, n
    dest.addParRi(n.endInfo)

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var nn = n.childCursor
  if nn.kind == Symbol and ((nn.symId == c.resultSym and c.canRaise) or c.tupleVars.contains(nn.symId)):
    let isResultSym = nn.symId == c.resultSym
    skip nn
    if nn.exprKind in CallKinds and callCanRaise(c.typeCache, nn):
      # nothing to do, both are in compatible tuple form:
      copyInto dest, n:
        dest.addSubtree n  # result
        inc n
        trCall c, dest, n, true
    else:
      copyInto dest, n:
        dest.addSubtree n  # result
        inc n
        let maybeClose: bool
        if isResultSym:
          maybeClose = produceSuccessTuple(c, dest, c.retType, n.info)
        else:
          maybeClose = produceSuccessTuple(c, dest, getType(c.typeCache, n), n.info)
        tr c, dest, n
        if maybeClose:
          dest.addParRi() # tuple constructor
  else:
    copyInto dest, n:
      tr c, dest, n
      tr c, dest, n

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
    if (n.symId == c.resultSym and c.canRaise) or c.tupleVars.contains(n.symId):
      let info = n.info
      copyIntoKind dest, TupatX, info:
        dest.addSymUse n.symId, info
        dest.addIntLit 1, info
    else:
      dest.addSubtree n
    inc n
  of SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StrLit, UnknownToken, DotToken, EofToken:
    takeTree dest, n
  of TagLit:
    let ek = n.exprKind
    case ek
    of CallKinds:
      trCall c, dest, n, false
    of PragmaxX:
      trPragmaBlock c, dest, n
    of DotX:
      takeInto dest, n:
        tr c, dest, n
        while n.hasMore:
          dest.takeTree n
    of OconstrX:
      trObjConstr c, dest, n
    of FailedX:
      trFailed c, dest, n
    else:
      case n.stmtKind
      of ProcS, FuncS, MethodS, ConverterS:
        trProcDecl c, dest, n
      of LocalDecls - {ResultS}:
        trLocal c, dest, n
      of ResultS:
        trResultDecl c, dest, n
      of ScopeS:
        trScope c, dest, n
      of AsgnS:
        trAsgn c, dest, n
      of RetS:
        trRet c, dest, n
      of RaiseS:
        trRaise c, dest, n
      of TryS:
        trTry c, dest, n
      of MacroS, TemplateS, TypeS:
        takeTree dest, n
      of CallS, CmdS, IteratorS, BlockS, EmitS, IfS, WhenS, BreakS,
         ContinueS, ForS, WhileS, CoroforS, CaseS, YldS, StmtsS,
         PragmasS, PragmaxS, InclS, ExclS, IncludeS, ImportS,
         ImportasS, FromimportS, ImportexceptS, ExportS, ExportexceptS,
         CommentS, DiscardS, UnpackdeclS, AssumeS, AssertS, CallstrlitS,
         InfixS, PrefixS, HcallS, StaticstmtS, BindS, MixinS, UsingS,
         AsmS, DeferS, LabS, JmpS, NoStmt:
        # generic container: copy the head and recurse into the children
        copyInto dest, n:
          while n.hasMore: tr c, dest, n
  else:
    raiseAssert "BUG: unexpected ParRi in raiselowering.tr" # classic ParRi only

proc lowerRaises*(pass: var Pass) =
  ## The whole module. Runs between the destroyer and `cps`; see the note at
  ## the top of this file for why that ordering is load-bearing.
  var n = pass.n
  var c = Context(typeCache: createTypeCache(pass.bits),
    tupleVars: localsThatBecomeTuples(n))
  c.retType = c.typeCache.builtins.voidType
  c.typeCache.openScope()
  tr(c, pass.dest, n)
  c.typeCache.closeScope()

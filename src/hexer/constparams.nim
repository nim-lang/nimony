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
codegen's `maybeByConstRef` logic.

We also now do part of the exception handling transformations here:
- Introduce tuples for the required variables.
- Translate `raise e` to `raise (e, result)`.

]##

import std / [sets, tables, hashes, assertions, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typeprops, builtintypes]
import ".." / models / tags
import duplifier, eraiser, passes
include ".." / nimony / nif_annotations

type
  Context = object
    constRefParams: HashSet[SymId]
    tupleVars: HashSet[SymId]
    exceptVars: seq[SymId]
    ptrSize, tmpCounter: int
    typeCache: TypeCache
    sizeofCache: SizeofCache  ## shared size-by-symbol memoization
    needsXelim: bool
    keepOverflowFlag: bool
    canRaise: bool
    nextRaiseIsSpecial: bool
    resultSym: SymId
    retType: Cursor

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)
    {.ensuresNif: addedAny(dest).}

proc passByConstRef(c: var Context; typ, pragmas: Cursor): bool =
  result = sizeof.passByConstRef(typ, pragmas, c.ptrSize, c.sizeofCache) or
           typeprops.isInheritable(typ, false)

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
    sizeofCache: move(c.sizeofCache), needsXelim: c.needsXelim,
    resultSym: SymId(0), canRaise: hasPragma(r.pragmas, RaisesP),
    retType: r.retType)

  copyInto(dest, n):
    let isConcrete = c2.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c2.typeCache.registerLocal(symId, r.kind, decl)
      c2.typeCache.openScope()
      rememberConstRefParams c2, r.params
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
  c.sizeofCache = move(c2.sizeofCache)
  c.needsXelim = c2.needsXelim

proc trConstRef(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  assert n.exprKind notin {AddrX, HaddrX}
  if constructsValue(n):
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

proc produceSuccessTuple(c: var Context; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo): bool =
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

proc produceRaiseTuple(c: var Context; dest: var TokenBuf; typ: Cursor; info: PackedLineInfo) =
  if not isVoidType(c.retType):
    dest.addParLe TupconstrX, info
    dest.addParLe TupleT, info
    dest.addSymUse pool.syms.getOrIncl(ErrorCodeName), info
    dest.addSubtree typ
    dest.addParRi()

proc finishRaiseTuple(c: var Context; dest: var TokenBuf; info: PackedLineInfo) =
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
  if n.firstSon.kind == DotToken:
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
  let localIsVoid = isVoidType(getType(c.typeCache, n.firstSon))
  if c.exceptVars.len > 0:
    # also bind the value to a potential `T as e` variable:
    let info = n.info
    copyIntoKind dest, AsgnS, info:
      dest.addSymUse c.exceptVars[^1], info
      if isSpecial and not localIsVoid:
        let x = n.firstSon
        assert x.kind == Symbol
        copyIntoKind dest, TupatX, info:
          dest.addSymUse x.symId, info
          dest.addIntLit 0, info
      else:
        dest.addSubtree n.firstSon

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
  let info = n.info
  n.into:
    let localIsVoid = isVoidType(getType(c.typeCache, n))
    if localIsVoid:
      dest.takeTree n
    else:
      assert n.kind == Symbol
      copyIntoKind dest, TupatX, info:
        dest.addSymUse n.symId, info
        inc n
        dest.addIntLit 0, info
  c.nextRaiseIsSpecial = true

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; targetExpectsTuple: bool) =
  var fnType = skipProcTypeToParams(getType(c.typeCache, n.firstSon))
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

  dest.addParLe(n.tag, n.info)
  n.into: # skip `(call)`
    tr c, dest, n # handle `fn`

    fnType = sub(fnType) # peek only, never left
    while n.hasMore:
      let previousFormalParam = fnType
      if not fnType.hasMore:
        tr c, dest, n # can happen for closure parameter
      else:
        assert fnType.isTagLit
        let param = takeLocal(fnType, SkipFinalParRi)
        let pk = param.typ.typeKind
        if pk in {MutT, OutT, LentT}:
          tr c, dest, n
        elif pk == VarargsT:
          # do not advance formal parameter:
          fnType = previousFormalParam
          tr c, dest, n
        elif passByConstRef(c, param.typ, param.pragmas):
          trConstRef c, dest, n
        elif pk in {TypedescT, StaticT}:
          # do not produce any code for this as it's a compile-time value:
          skip n
        else:
          tr c, dest, n
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
  dest.addParLe(n.tag, n.info)
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
  let typ = n.firstSon

  let target = pool.syms.getOrIncl("`constRefTemp." & $c.tmpCounter)
  inc c.tmpCounter
  copyIntoKind dest, VarS, info:
    addSymDef dest, target, info
    dest.addEmpty2 info # export marker, pragma
    copyTree dest, typ
    dest.addDotToken() # value
  dest.addParLe(pool.tags.getOrIncl("keepovf"), info)
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

proc trTry(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # We only deal with the data flow here.
  var nn = n.firstSon
  skip nn # stmts
  let oldLen = c.exceptVars.len
  if nn.substructureKind == ExceptU:
    inc nn
    if nn.stmtKind == LetS:
      copyInto dest, nn:
        let exc = nn.symId
        c.exceptVars.add exc
        c.typeCache.takeLocalHeader(dest, nn, LetY)
        assert nn.kind == DotToken
        dest.add nn
        inc nn

  dest.addParLe(n.tag, n.info)
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
  var nn = n.firstSon
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
    if c.constRefParams.contains(n.symId):
      copyIntoKind dest, DerefX, n.info:
        dest.addSubtree n
    elif (n.symId == c.resultSym and c.canRaise) or c.tupleVars.contains(n.symId):
      let info = n.info
      copyIntoKind dest, TupatX, info:
        dest.addSymUse n.symId, info
        dest.addIntLit 1, info
    else:
      dest.addSubtree n
    inc n
  of SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StrLitKind, UnknownTokenKind, DotToken, EofTokenKind:
    takeToken dest, n
  of OpenTagKind:
    let ek = n.exprKind
    case ek
    of CallKinds:
      trCall c, dest, n, false
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
         AsmS, DeferS, NoStmt:
        # generic container: copy the head and recurse into the children
        copyInto dest, n:
          while n.hasMore: tr c, dest, n
  else:
    raiseAssert "BUG: unexpected ParRi in constparams.tr" # classic ParRi only

proc injectConstParamDerefs*(pass: var Pass; ptrSize: int; needsXelim: var bool) =
  var n = pass.n  # Extract cursor locally
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache(), needsXelim: needsXelim,
    tupleVars: localsThatBecomeTuples(n))
  c.retType = c.typeCache.builtins.voidType
  c.typeCache.openScope()
  tr(c, pass.dest, n)  # Write to pass.dest
  c.typeCache.closeScope()
  needsXelim = c.needsXelim

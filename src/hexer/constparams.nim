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

import std / [sets, assertions]

include nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typeprops, builtintypes]
import ".." / models / tags
import duplifier, eraiser

type
  Context = object
    constRefParams: HashSet[SymId]
    tupleVars: HashSet[SymId]
    exceptVars: seq[SymId]
    ptrSize, tmpCounter: int
    typeCache: TypeCache
    needsXelim: bool
    keepOverflowFlag: bool
    canRaise: bool
    nextRaiseIsSpecial: bool
    resultSym: SymId
    retType: Cursor

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc passByConstRef(c: var Context; typ, pragmas: Cursor): bool =
  result = sizeof.passByConstRef(typ, pragmas, c.ptrSize) or typeprops.isInheritable(typ, false)

proc rememberConstRefParams(c: var Context; params: Cursor) =
  var n = params
  inc n # skips (params
  while n.kind != ParRi:
    let r = takeLocal(n, SkipFinalParRi)
    if r.name.kind == SymbolDef and passByConstRef(c, r.typ, r.pragmas):
      c.constRefParams.incl r.name.symId

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  var c2 = Context(ptrSize: c.ptrSize, typeCache: move(c.typeCache), needsXelim: c.needsXelim,
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
          inc n
          while n.kind != ParRi:
            tr c2, dest, n
          skipParRi n
        else:
          tr c2, dest, n
        if c2.canRaise and isVoidType(r.retType):
          copyIntoKind dest, RetS, info:
            dest.addSymUse pool.syms.getOrIncl(SuccessName), info
      c2.typeCache.closeScope()
    else:
      takeTree dest, n
  c.typeCache = move(c2.typeCache)
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
    finishRaiseTuple c, dest, n.info

proc trFailed(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  let localIsVoid = isVoidType(getType(c.typeCache, n))
  if localIsVoid:
    dest.takeTree n
  else:
    assert n.kind == Symbol
    copyIntoKind dest, TupatX, info:
      dest.addSymUse n.symId, info
      inc n
      dest.addIntLit 0, info
  skipParRi n
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

  dest.add n
  inc n # skip `(call)`
  tr c, dest, n # handle `fn`

  inc fnType
  while n.kind != ParRi:
    let previousFormalParam = fnType
    if fnType.kind == ParRi:
      tr c, dest, n # can happen for closure parameter
    else:
      assert fnType.kind == ParLe
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
  takeParRi dest, n
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
    let isTuple = c.tupleVars.contains(symId)
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
  dest.add n
  inc n
  while n.kind != ParRi:
    tr c, dest, n
  takeParRi dest, n
  c.typeCache.closeScope()

proc trPragmaBlock(c: var Context; dest: var TokenBuf; n: var Cursor) =
  inc n # pragmax
  inc n # pragmas
  if n.pragmaKind == KeepOverflowFlagP:
    skip n # keepOverflowFlag
    skipParRi n # pragmas
    let oldKeepOverflowFlag = c.keepOverflowFlag
    c.keepOverflowFlag = true
    tr(c, dest, n)
    c.keepOverflowFlag = oldKeepOverflowFlag
  else:
    bug "unknown pragma block: " & toString(n, false)
  skipParRi n # pragmax

proc checkedArithOp(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.add parLeToken(ExprX, info)
  dest.add parLeToken(StmtsS, info)
  let typ = n.firstSon

  let target = pool.syms.getOrIncl("`constRefTemp." & $c.tmpCounter)
  inc c.tmpCounter
  copyIntoKind dest, VarS, info:
    addSymDef dest, target, info
    dest.addEmpty2 info # export marker, pragma
    copyTree dest, typ
    dest.addDotToken() # value
  dest.add parLeToken(pool.tags.getOrIncl("keepovf"), info)
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

  dest.add n
  inc n
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
  takeParRi dest, n

proc trAsgn(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var nn = n.firstSon
  if nn.kind == Symbol and ((nn.symId == c.resultSym and c.canRaise) or c.tupleVars.contains(nn.symId)):
    let isResultSym = nn.symId == c.resultSym
    skip nn
    if nn.exprKind in CallKinds and callCanRaise(c.typeCache, nn):
      # nothing to do, both are in compatible tuple form:
      copyInto dest, n:
        dest.add n # result
        inc n
        trCall c, dest, n, true
    else:
      copyInto dest, n:
        dest.add n # result
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

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var nested = 0
  while true:
    case n.kind
    of Symbol:
      if c.constRefParams.contains(n.symId):
        copyIntoKind dest, DerefX, n.info:
          dest.add n
      elif (n.symId == c.resultSym and c.canRaise) or c.tupleVars.contains(n.symId):
        let info = n.info
        copyIntoKind dest, TupatX, info:
          dest.addSymUse n.symId, info
          dest.addIntLit 1, info
      else:
        dest.add n
      inc n
    of SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StringLit, UnknownToken, DotToken, EofToken:
      dest.add n
      inc n
    of ParLe:
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
          dest.add n
          inc n
          inc nested
      of FailedX:
        trFailed c, dest, n
      else:
        case n.stmtKind
        of ProcS, FuncS, MacroS, MethodS, ConverterS:
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
        of TemplateS, TypeS:
          takeTree dest, n
        else:
          dest.add n
          inc n
          inc nested
    of ParRi:
      dest.add n
      inc n
      dec nested
    if nested == 0: break

proc injectConstParamDerefs*(n: Cursor; ptrSize: int; needsXelim: var bool): TokenBuf =
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache(), needsXelim: needsXelim,
    tupleVars: localsThatBecomeTuples(n))
  c.retType = c.typeCache.builtins.voidType
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  tr(c, result, n)
  c.typeCache.closeScope()
  needsXelim = c.needsXelim

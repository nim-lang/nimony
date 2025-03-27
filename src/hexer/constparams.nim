#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

##[

Implements "pass by const" by introducing more hidden pointers.
We do this right before inlining and before codegen as it interacts with the
codegen's `maybeByConstRef` logic.

]##

import std / [sets, assertions]

include nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof]
import ".." / models / tags
import duplifier

type
  Context = object
    constRefParams: HashSet[SymId]
    ptrSize, tmpCounter: int
    typeCache: TypeCache
    needsXelim: bool
    keepOverflowFlag: bool

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc rememberConstRefParams(c: var Context; params: Cursor) =
  var n = params
  inc n # skips (params
  while n.kind != ParRi:
    let r = takeLocal(n, SkipFinalParRi)
    if r.name.kind == SymbolDef and passByConstRef(r.typ, r.pragmas, c.ptrSize):
      c.constRefParams.incl r.name.symId

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var r = asRoutine(n)
  var c2 = Context(ptrSize: c.ptrSize, typeCache: createTypeCache(), needsXelim: c.needsXelim)
  c2.typeCache.openScope()
  copyInto(dest, n):
    let isConcrete = c2.typeCache.takeRoutineHeader(dest, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, r.params)
      rememberConstRefParams c2, r.params
      tr c2, dest, n
    else:
      takeTree dest, n
  c2.typeCache.closeScope()
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

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  dest.add n
  inc n # skip `(call)`
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))
  takeTree dest, n # skip `fn`
  assert fnType.tagEnum == ParamsTagId
  inc fnType
  while n.kind != ParRi:
    let previousFormalParam = fnType
    assert fnType.kind == ParLe
    let param = takeLocal(fnType, SkipFinalParRi)
    let pk = param.typ.typeKind
    if pk in {MutT, OutT, LentT}:
      tr c, dest, n
    elif pk == VarargsT:
      # do not advance formal parameter:
      fnType = previousFormalParam
      tr c, dest, n
    elif passByConstRef(param.typ, param.pragmas, c.ptrSize):
      trConstRef c, dest, n
    elif pk in {TypedescT, StaticT}:
      # do not produce any code for this as it's a compile-time value:
      skip n
    else:
      tr c, dest, n
  takeParRi dest, n

proc trLocal(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let kind = n.symKind
  copyInto dest, n:
    c.typeCache.takeLocalHeader(dest, n, kind)
    tr(c, dest, n)

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
    raiseAssert "unknown pragma block: " & toString(n, false)
  skipParRi n # pragmax

proc checkedArithOp(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let before = dest.len
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

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var nested = 0
  while true:
    case n.kind
    of Symbol:
      if c.constRefParams.contains(n.symId):
        copyIntoKind dest, DerefX, n.info:
          dest.add n
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
        trCall c, dest, n
      of PragmaxX:
        trPragmaBlock c, dest, n
      of AddX, SubX, MulX, DivX, ModX:
        if c.keepOverflowFlag:
          checkedArithOp c, dest, n
        else:
          dest.add n
          inc n
          inc nested
      else:
        case n.stmtKind
        of ProcS, FuncS, MacroS, MethodS, ConverterS:
          trProcDecl c, dest, n
        of LocalDecls:
          trLocal c, dest, n
        of ScopeS:
          trScope c, dest, n
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
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache(), needsXelim: needsXelim)
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  tr(c, result, n)
  c.typeCache.closeScope()
  needsXelim = c.needsXelim

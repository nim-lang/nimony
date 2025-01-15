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
import duplifier

type
  Context = object
    constRefParams: HashSet[SymId]
    ptrSize, tmpCounter: int
    typeCache: TypeCache

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc rememberConstRefParams(c: var Context; params, pragmas: Cursor) =
  var n = params
  while n.kind != ParRi:
    let r = takeLocal(n, SkipFinalParRi)
    if r.name.kind == SymbolDef and passByConstRef(r.typ, pragmas, c.ptrSize):
      c.constRefParams.incl r.name.symId

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var r = takeRoutine(n, SkipFinalParRi)
  var c2 = Context(ptrSize: c.ptrSize, typeCache: createTypeCache())
  copyInto(dest, n):
    copyTree dest, r.name
    copyTree dest, r.exported
    copyTree dest, r.pattern
    copyTree dest, r.typeVars
    copyTree dest, r.params
    copyTree dest, r.pragmas
    copyTree dest, r.effects
    if r.body.stmtKind == StmtsS and not isGeneric(r):
      rememberConstRefParams c2, r.params, r.pragmas
      tr c2, dest, r.body
    else:
      copyTree dest, r.body

proc trConstRef(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  assert n.exprKind notin {AddrX, HaddrX}
  if constructsValue(n):
    # We cannot take the address of a literal so we have to copy it to a
    # temporary first:
    let argType = getType(c.typeCache, n)
    copyIntoKind dest, ExprX, info:
      copyIntoKind dest, StmtsS, info:
        let symId = pool.syms.getOrIncl("`constRefTemp" & $c.tmpCounter)
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
  var fnType = getType(c.typeCache, n)
  takeTree dest, n # skip `fn`
  assert fnType == "params"
  inc fnType
  while n.kind != ParRi:
    let previousFormalParam = fnType
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
    else:
      tr c, dest, n
  wantParRi dest, n

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
      if n.exprKind in CallKinds:
        trCall c, dest, n
      elif n.stmtKind in {ProcS, FuncS, MacroS, MethodS, ConverterS}:
        trProcDecl c, dest, n
      else:
        dest.add n
        inc n
        inc nested
    of ParRi:
      dest.add n
      inc n
      dec nested
    if nested == 0: break

proc injectConstParamDerefs*(n: Cursor; ptrSize: int): TokenBuf =
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache())
  result = createTokenBuf(300)
  var n = n
  tr(c, result, n)

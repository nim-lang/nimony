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

import std / [sets, assertions]

include nifprelude
import ".." / nimony / [nimony_model, decls, programs, typenav, sizeof, typeprops]
import ".." / models / tags
import duplifier

type
  Context = object
    ptrSize, tmpCounter: int
    typeCache: TypeCache
    needsXelim: bool

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  var c2 = Context(ptrSize: c.ptrSize, typeCache: move(c.typeCache), needsXelim: c.needsXelim)

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
  c.needsXelim = c2.needsXelim

proc addRaiseStmt(dest: var TokenBuf; target: SymId; info: PackedLineInfo) =
  copyIntoKind dest, IfS, info:
    copyIntoKind dest, ElifU, info:
      copyIntoKind dest, FailedX, info:
        dest.addSymUse target, info
      copyIntoKind dest, StmtsS, info:
        copyIntoKind dest, RaiseS, info:
          dest.addSymUse target, info

proc localsThatBecomeTuples*(n: Cursor): HashSet[SymId] =
  # n must be a routine!
  result = initHashSet[SymId]()
  var nested = 0
  var n = n
  var hasRaisesPragma = false
  while true:
    case n.kind
    of Symbol, SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StringLit, UnknownToken, DotToken, EofToken:
      inc n
    of ParLe:
      if n.exprKind == FailedX and n.firstSon.kind == Symbol:
        result.incl n.firstSon.symId
        inc nested
        inc n
      elif n.pragmaKind == RaisesP:
        hasRaisesPragma = true
        inc nested
        inc n
      elif n.stmtKind == ResultS and n.firstSon.kind == SymbolDef:
        if hasRaisesPragma:
          result.incl n.firstSon.symId
        inc nested
        inc n
      elif n.symKind in RoutineKinds:
        skip n
      else:
        inc nested
        inc n
    of ParRi:
      dec nested
      inc n
    if nested == 0: break

proc callCanRaise*(typeCache: var TypeCache; n: Cursor): bool =
  var fnType = skipProcTypeToParams(getType(typeCache, n.firstSon))
  assert fnType.tagEnum == ParamsTagId
  skip fnType # params
  skip fnType # return type
  # now pragmas follow:
  result = hasPragma(fnType, RaisesP)

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; inhibit: bool) =
  let head = n.load()
  let info = n.info
  inc n # skip `(call)`
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))
  assert fnType.tagEnum == ParamsTagId
  skip fnType # params
  let retType = fnType
  skip fnType # return type
  # now pragmas follow:
  let canRaise = hasPragma(fnType, RaisesP)
  if canRaise and not inhibit:
    c.needsXelim = true
    let isVoid = retType.kind == DotToken or retType.typeKind == VoidT
    if not isVoid:
       dest.addParLe(ExprX, info)
    copyIntoKind dest, StmtsS, info:
      let symId = pool.syms.getOrIncl("`canRaise." & $c.tmpCounter)
      inc c.tmpCounter
      copyIntoKind dest, CursorS, info:
        addSymDef dest, symId, info
        dest.addEmpty2 info # export marker, pragma
        copyTree dest, retType
        # value is the call expression:
        dest.add head
        while n.kind != ParRi:
          tr c, dest, n
        takeParRi dest, n
      addRaiseStmt(dest, symId, info)
    if not isVoid:
      dest.addSymUse symId, info
      dest.addParRi()
  else:
    dest.add head
    while n.kind != ParRi:
      tr c, dest, n
    takeParRi dest, n

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
    addRaiseStmt(dest, target, n.info)

proc trAssign(c: var Context; dest: var TokenBuf; n: var Cursor) =
  copyInto dest, n:
    tr c, dest, n # left hand side
    tr c, dest, n # right hand side

proc trScope(c: var Context; dest: var TokenBuf; n: var Cursor) =
  c.typeCache.openScope()
  dest.add n
  inc n
  while n.kind != ParRi:
    tr c, dest, n
  takeParRi dest, n
  c.typeCache.closeScope()

proc tr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var nested = 0
  while true:
    case n.kind
    of Symbol, SymbolDef, Ident, IntLit, UIntLit, FloatLit, CharLit, StringLit, UnknownToken, DotToken, EofToken:
      dest.add n
      inc n
    of ParLe:
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

proc injectRaisingCalls*(n: Cursor; ptrSize: int; needsXelim: var bool): TokenBuf =
  var c = Context(ptrSize: ptrSize, typeCache: createTypeCache(), needsXelim: needsXelim)
  c.typeCache.openScope()
  result = createTokenBuf(300)
  var n = n
  tr(c, result, n)
  c.typeCache.closeScope()
  needsXelim = c.needsXelim

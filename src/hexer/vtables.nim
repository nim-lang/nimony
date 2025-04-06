#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## VTable generation. We need to do:
## - Patch object constructors to setup the vtable pointer
## - Generate vtables for all classes
## - Transform calls to virtual methods into calls to the vtable
## - Translate the `of` operator into something NIFC can understand

import std/[assertions, tables]

include nifprelude
import nifindexes, symparser, treemangler, typekeys, hexer_context
import ".." / nimony / [nimony_model, decls, programs, typenav, expreval, xints, renderer, typeprops]
from duplifier import constructsValue

type
  VTableEntry = object
    methodSymId: SymId
    implSymId: SymId

  TypeWithVTable = object
    typeSymId: SymId
    vtableSymId: SymId  # symbol for the vtable
    parent: SymId       # parent type's symbol (empty for root types)
    methods: seq[VTableEntry]

  Context* = object
    vtables: TokenBuf
    tmpCounter: int
    typeCache: TypeCache
    needsXelim: bool

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  var r = asRoutine(n)
  c.typeCache.openScope()
  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, r.params)
      tr c, dest, n
    else:
      takeTree dest, n
  c.typeCache.closeScope()

type
  TempLoc = object
    sym: SymId
    needsDeref, needsParRi: bool

proc useTemp(dest: var TokenBuf; temp: TempLoc; info: PackedLineInfo) =
  if temp.needsDeref:
    copyIntoKind dest, DerefX, info:
      dest.addSymUse temp.sym, info
  else:
    dest.addSymUse temp.sym, info

proc closeTemp(dest: var TokenBuf; temp: TempLoc) =
  if temp.needsParRi:
    addParRi dest

proc evalOnce(c: var Context; dest: var TokenBuf; n: var Cursor): TempLoc =
  if n.kind == Symbol:
    result = TempLoc(sym: n.symId, needsDeref: false, needsParRi: false)
    inc n
    return

  let info = n.info
  let takeAddr = not constructsValue(n)
  let argType = getType(c.typeCache, n)
  c.needsXelim = true

  copyIntoKind dest, ExprX, info:
    copyIntoKind dest, StmtsS, info:
      let symId = pool.syms.getOrIncl("`vtableTemp." & $c.tmpCounter)
      inc c.tmpCounter
      copyIntoKind dest, VarS, info:
        addSymDef dest, symId, info
        dest.addEmpty2 info # export marker, pragma
        copyTree dest, argType
        # value:
        if takeAddr:
          copyIntoKind dest, AddrX, info:
            tr c, dest, n
        else:
          tr c, dest, n
  result = TempLoc(sym: symId, needsDeref: takeAddr, needsParRi: true)

proc isMethod(c: var Context; s: SymId): bool =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    result = res.decl.symKind == MethodY
  else:
    let info = getLocalInfo(c.typeCache, s)
    result = info.kind == MethodY

proc getMethodIndex(c: var Context; cls, fn: SymId): int =
  # XXX To implement
  result = 0

proc trMethodCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let fnNode = n.load()
  let fn = n.symId
  inc n # skip fn
  let typ = getType(c.typeCache, n)
  let cls = getClass(typ)
  if cls == SymId(0):
    error "cannot call method `" & pool.syms[fn] & "` on type " & typeToString(typ)
    dest.add fnNode
  else:
    let info = n.info
    let temp = evalOnce(c, dest, n)
    copyIntoKind dest, ArrAtX, info:
      copyIntoKind dest, DotX, info:
        useTemp dest, temp, info
        dest.addSymUse pool.syms.getOrIncl("vt.0"), info
        dest.addIntLit 0, info # this is getting stupid...
      let idx = getMethodIndex(c, cls, fn)
      dest.addIntLit idx, info
    closeTemp dest, temp
    # first arg still need to be passed to the method:
    useTemp dest, temp, info
    # other arguments are handled by the regular code:
    while n.kind != ParRi:
      tr c, dest, n

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; forceStaticCall: bool) =
  dest.add n
  inc n # skip `(call)`
  if not forceStaticCall and n.kind == Symbol and isMethod(c, n.symId):
    trMethodCall c, dest, n
  else:
    while n.kind != ParRi:
      tr c, dest, n
  takeParRi dest, n

proc trProcCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  inc n # (procCall)
  if n.exprKind in CallKinds:
    trCall c, dest, n, true
  else:
    tr c, dest, n
  skipParRi n

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
      of ProcCallX:
        trProcCall c, dest, n
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

proc transformVTables*(n: Cursor; needsXelim: var bool): TokenBuf =
  var c = Context(
    vtables: createTokenBuf(300),
    typeCache: createTypeCache(),
    needsXelim: needsXelim
  )
  c.typeCache.openScope()

  var dest = createTokenBuf(300)

  var n = n
  assert n.stmtKind == StmtsS
  dest.add n
  inc n
  while n.kind != ParRi: tr c, dest, n
  dest.add c.vtables
  dest.addParRi()

  c.typeCache.closeScope()
  needsXelim = c.needsXelim

  result = ensureMove dest

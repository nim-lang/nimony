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
import ".." / lib / tinyhashes
import nifindexes, symparser, treemangler, typekeys
import ".." / nimony / [nimony_model, decls, programs, typenav,
  renderer, builtintypes, typeprops]
from duplifier import constructsValue

type
  VTable = object
    display: seq[SymId]
    methods: seq[SymId]

  Context* = object
    vtableCounter: int
    tmpCounter: int
    typeCache: TypeCache
    needsXelim: bool
    moduleSuffix: string
    vindex: Table[(SymId, string), int] # (class, method) -> index; \
      # string key takes parameter types into account so it just works with overloading
    vtables: Table[SymId, VTable]
    vtableNames: Table[SymId, SymId]
    getRttiSym: SymId

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc computeVTableName(c: var Context; cls: SymId): SymId =
  var clsName = pool.syms[cls]
  extractBasename clsName
  result = pool.syms.getOrIncl(clsName & ".vt." & $c.vtableCounter & "." & c.moduleSuffix)
  inc c.vtableCounter

proc getVTableName(c: var Context; cls: SymId): SymId =
  if c.vtableNames.hasKey(cls):
    result = c.vtableNames[cls]
  else:
    result = computeVTableName(c, cls)
    c.vtableNames[cls] = result

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
  for i, m in pairs(c.vtables[cls].methods):
    if m == fn: return i
  error "method `" & pool.syms[fn] & "` not found in class " & pool.syms[cls]

proc trMethodCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let fnNode = n.load()
  let fn = n.symId
  inc n # skip fn
  let typ = getType(c.typeCache, n)
  let cls = getClass(typ)
  if cls == SymId(0):
    dest.add fnNode
    error "cannot call method `" & pool.syms[fn] & "` on type " & typeToString(typ)
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

proc trGetRtti(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n # call
  skip n # skip "getRtti" symbol
  assert n.kind == Symbol # we have the class name here
  dest.copyIntoKind AddrX, info:
    dest.addSymUse getVTableName(c, n.symId), info
  inc n
  skipParRi n

proc trObjConstr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.takeToken n # objconstr
  var cls = SymId(0)
  if n.kind == Symbol and not isGeneratedType(pool.syms[n.symId]) and hasRtti(n.symId):
    cls = n.symId

  dest.takeTree n # type
  if cls != SymId(0):
    dest.copyIntoKind KvU, info:
      dest.copyIntoSymUse pool.syms.getOrIncl(VTableField), info
      dest.copyIntoKind AddrX, info:
        dest.addSymUse getVTableName(c, cls), info
  while n.kind != ParRi:
    tr c, dest, n
  takeParRi dest, n

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; forceStaticCall: bool) =
  if not forceStaticCall and n.kind == Symbol and isMethod(c, n.symId):
    dest.takeToken n # skip `(call)`
    trMethodCall c, dest, n
    takeParRi dest, n
  elif n.kind == Symbol and n.symId == c.getRttiSym:
    trGetRtti c, dest, n
  else:
    dest.takeToken n # skip `(call)`
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

proc classData(typ: Cursor): (int, UHash) =
  var n = typ
  if n.typeKind in {RefT, PtrT}:
    inc n
  result = (0, UHash(0))
  if n.kind == Symbol:
    let s = n.symId
    for _ in inheritanceChain(s): inc result[0]
    result[1] = uhash(pool.syms[s])

proc trInstanceof(c: var Context; dest: var TokenBuf; n: var Cursor) =
  # `v of T` is translated into a logical 'and' expression:
  # let vtab = v.vtab
  # (level < len(vtab.display)) and (vtab.display[level] == hash(T))
  let info = n.info
  inc n # skip `instanceof`

  let vtabTempSym = pool.syms.getOrIncl("`vtableTemp." & $c.tmpCounter)
  inc c.tmpCounter

  c.needsXelim = true
  copyIntoKind dest, ExprX, info:
    copyIntoKind dest, StmtsS, info:
      copyIntoKind dest, VarS, info:
        dest.addSymDef vtabTempSym, info
        dest.addEmpty2 info # export marker, pragma
        dest.copyIntoKind PtrT, info:
          dest.addSymUse pool.syms.getOrIncl("Rtti.0." & c.moduleSuffix), info

        copyIntoKind dest, DotX, info:
          copyIntoKind dest, DerefX, info:
            copyIntoKind dest, DotX, info:
              tr c, dest, n
              dest.copyIntoSymUse pool.syms.getOrIncl(VTableField), info
              dest.addIntLit 0, info

      # Get the class data (level and hash)
      let (level, h) = classData(n)
      skip n # skip type

    # Create the AndX expression for the range check and hash comparison
    copyIntoKind dest, AndX, info:
      # First expression: level < len(vtab.display)
      copyIntoKind dest, LtX, info:
        copyIntoKind dest, IT, info:
          dest.addIntLit -1, info

        dest.addIntLit level, info
        copyIntoKind dest, DotX, info:
          copyIntoKind dest, DerefX, info:
            dest.addSymUse vtabTempSym, info
          dest.copyIntoSymUse pool.syms.getOrIncl(DisplayLenField), info
          dest.addIntLit 0, info

      # Second expression: vtab.display[level] == hash(T)
      copyIntoKind dest, EqX, info:
        copyIntoKind dest, UT, info:
          dest.addIntLit 32, info

        copyIntoKind dest, PatX, info:
          copyIntoKind dest, DotX, info:
            dest.addSymUse vtabTempSym, info
            dest.copyIntoSymUse pool.syms.getOrIncl(DisplayField), info
            dest.addIntLit 0, info
          dest.addIntLit level, info

        dest.addUIntLit h, info

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
      of OconstrX:
        trObjConstr c, dest, n
      of InstanceofX:
        trInstanceof c, dest, n
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

when false:
  # maybe we can use this later to provide better error messages
  proc sameMethodSignatures(a, b: Cursor): bool =
    var a = a
    var b = b
    if a.substructureKind != ParamsU: return false
    if b.substructureKind != ParamsU: return false
    inc a
    inc b
    # first parameter is of the class type and must be ignored:
    skip a
    skip b
    while a.kind != ParRi and b.kind != ParRi:
      let pa = takeLocal(a, SkipFinalParRi)
      let pb = takeLocal(b, SkipFinalParRi)
      if not sameTrees(pa.typ, pb.typ):
        return false
    if a.kind == ParRi and b.kind == ParRi:
      inc a
      inc b
      # check return types
      return sameTrees(a, b)
    else:
      return false

proc methodKey(a: Cursor): string =
  # First parameter was the class type and has already been skipped here!
  var a = a
  var b = createMangler(60)
  while a.kind != ParRi:
    let pa = takeLocal(a, SkipFinalParRi)
    mangle b, pa.typ
  inc a
  # also add return type:
  mangle b, a
  result = b.extract()

proc registerMethod(c: var Context; r: Routine; methodName: string) =
  var p = r.params
  if p.kind == ParLe:
    inc p
    let param = takeLocal(p, SkipFinalParRi)
    let cls = getClass(param.typ)
    if cls == SymId(0):
      error "cannot attach method to type " & typeToString(param.typ)
    else:
      let sig = methodName & ":" & methodKey(p)
      # see if this is an override:
      var i = 0
      for inh in inheritanceChain(cls):
        if i == 0 and not c.vtables.hasKey(cls):
          # direct base class: Take total number of methods:
          c.vtables[cls] = c.vtables.getOrDefault(inh, VTable(display: @[], methods: @[]))
          c.vtables[cls].display.add cls # add self

        let key = (inh, sig)
        let methodIndex = c.vindex.getOrDefault(key, -1)
        if methodIndex != -1:
          # register as override:
          c.vtables[cls].methods[methodIndex] = r.name.symId
          return
        inc i
      # not an override, register as new method:
      c.vtables.mgetOrPut(cls, VTable(display: @[], methods: @[])).methods.add r.name.symId
      c.vindex[(cls, sig)] = c.vtables[cls].methods.len - 1

proc collectMethods(c: var Context; n: var Cursor) =
  # we only care about top level methods
  case n.stmtKind
  of StmtsS:
    inc n
    while n.kind != ParRi:
      collectMethods c, n
    skipParRi n
  of MethodS:
    let r = takeRoutine(n, SkipFinalParRi)
    if not r.isGeneric:
      var methodName = pool.syms[r.name.symId]
      extractBasename methodName
      registerMethod c, r, methodName
  else:
    skip n

proc emitVTables(c: var Context; dest: var TokenBuf) =
  # Used the `mpairs` and `mitems` here to avoid copies.
  for cls, vtab in mpairs c.vtables:
    dest.copyIntoKind ConstS, NoLineInfo:
      dest.addSymDef getVTableName(c, cls), NoLineInfo
      dest.addEmpty2() # export marker, pragmas
      dest.addSymUse pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix), NoLineInfo
      dest.copyIntoKind OconstrX, NoLineInfo:
        dest.addSymUse pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix), NoLineInfo

        dest.addParLe KvU, NoLineInfo
        dest.addSymUse pool.syms.getOrIncl(DisplayLenField), NoLineInfo
        dest.addIntLit vtab.display.len, NoLineInfo
        dest.addParRi() # KvU

        dest.addParLe KvU, NoLineInfo
        dest.addSymUse pool.syms.getOrIncl(DisplayField), NoLineInfo
        dest.addParLe AconstrX, NoLineInfo
        # array constructor also starts with a type, yuck:
        dest.copyIntoKind ArrayT, NoLineInfo:
          dest.copyIntoKind UT, NoLineInfo:
            dest.addIntLit 32, NoLineInfo
          dest.addIntLit vtab.display.len, NoLineInfo
        for i in countdown(vtab.display.len - 1, 0):
          dest.addUIntLit uhash(pool.syms[vtab.display[i]]), NoLineInfo
        dest.addParRi() # AconstrX
        dest.addParRi() # KvU

        dest.addParLe KvU, NoLineInfo
        dest.addSymUse pool.syms.getOrIncl(MethodsField), NoLineInfo
        dest.addParLe AconstrX, NoLineInfo
        # array constructor also starts with a type, yuck:
        dest.copyIntoKind ArrayT, NoLineInfo:
          dest.addParPair PointerT, NoLineInfo
          dest.addIntLit vtab.methods.len, NoLineInfo
        for m in mitems(vtab.methods):
          dest.copyIntoKind CastX, NoLineInfo:
            dest.addParPair PointerT, NoLineInfo
            dest.addSymUse m, NoLineInfo
        dest.addParRi() # AconstrX
        dest.addParRi() # KvU

proc transformVTables*(n: Cursor; moduleSuffix: string; needsXelim: var bool): TokenBuf =
  var c = Context(
    typeCache: createTypeCache(),
    moduleSuffix: moduleSuffix,
    needsXelim: needsXelim,
    getRttiSym: pool.syms.getOrIncl("getRtti.0." & SystemModuleSuffix)
  )
  c.typeCache.openScope()

  var dest = createTokenBuf(300)

  var n2 = n
  # XXX This has the fatal flaw right now that it assumes
  # the methods of the base class are complete before the processing
  # of the derived class begins!
  collectMethods c, n2

  var n = n
  assert n.stmtKind == StmtsS
  dest.add n
  inc n

  emitVTables c, dest

  while n.kind != ParRi: tr c, dest, n
  dest.addParRi()

  c.typeCache.closeScope()
  needsXelim = c.needsXelim

  result = ensureMove dest

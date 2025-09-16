#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## VTable generation. We need to do:
## - Generate vtables for all classes declared in the module
## - Patch object constructors to setup the vtable pointer
## - Transform calls to virtual methods into calls to the vtable
## - Translate the `of` operator into something NIFC can understand

import std/[assertions, tables]

include nifprelude
import ".." / lib / tinyhashes
import nifindexes, symparser, treemangler
import ".." / nimony / [nimony_model, decls, programs, typenav,
  renderer, builtintypes, typeprops, typekeys, vtables_frontend]
from duplifier import constructsValue

type
  VTableState = enum
    Others
    AlreadyImported
    Mine
  VTable = object
    display: seq[SymId]
    methods: seq[SymId]
    signatureToIndex: Table[string, int]
    parent: SymId
    state: VTableState

  MethodDecl = object
    cls: SymId
    name: SymId
    paramRest: Cursor

  Context* = object
    tmpCounter: int
    typeCache: TypeCache
    needsXelim: bool
    moduleSuffix: string
    vtables: Table[SymId, VTable]
    vtableNames: Table[SymId, SymId]
    classes: seq[(SymId, bool)] # (class, declaredInThisModule)
    getRttiSym: SymId
    methodDecls: seq[MethodDecl]

when not defined(nimony):
  proc tr(c: var Context; dest: var TokenBuf; n: var Cursor)

proc computeVTableName(c: var Context; cls: SymId; middle = ".vt."): SymId =
  var clsName = splitSymName pool.syms[cls]
  clsName.name.add middle
  clsName.name.add clsName.module
  result = pool.syms.getOrIncl(clsName.name)

proc getVTableName(c: var Context; cls: SymId): SymId =
  if c.vtableNames.hasKey(cls):
    result = c.vtableNames[cls]
  else:
    result = computeVTableName(c, cls)
    c.vtableNames[cls] = result

proc trProcDecl(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let decl = n
  var r = asRoutine(n)
  c.typeCache.openScope()
  copyInto(dest, n):
    let isConcrete = c.typeCache.takeRoutineHeader(dest, decl, n)
    if isConcrete:
      let symId = r.name.symId
      if isLocalDecl(symId):
        c.typeCache.registerLocal(symId, r.kind, decl)
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
    return result

  let info = n.info
  var takeAddr = not constructsValue(n) and n.exprKind notin {AddrX, HaddrX}
  let argType = getType(c.typeCache, n)
  if argType.typeKind in {RefT, PtrT}:
    takeAddr = false
  c.needsXelim = true

  dest.addParLe(ExprX, info) # will be closed with closeTemp
  copyIntoKind dest, StmtsS, info:
    let symId = pool.syms.getOrIncl("`vtableTemp." & $c.tmpCounter)
    inc c.tmpCounter

    copyIntoKind dest, VarS, info:
      addSymDef dest, symId, info
      dest.addEmpty2 info # export marker, pragma
      if takeAddr:
        # type:
        copyIntoKind dest, PtrT, info:
          copyTree dest, argType
        # value:
        copyIntoKind dest, AddrX, info:
          tr c, dest, n
      else:
        # type:
        copyTree dest, argType
        # value:
        tr c, dest, n
  result = TempLoc(sym: symId, needsDeref: takeAddr, needsParRi: true)

proc isMethod(c: var Context; s: SymId): bool =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    result = res.decl.symKind == MethodY
  else:
    let info = getLocalInfo(c.typeCache, s)
    result = info.kind == MethodY

proc loadVTable(c: var Context; cls: SymId) =
  # Interface files only store the "diff" of the vtable so we need to
  # compute it properly here.
  var parent = SymId(0)
  for inh in inheritanceChain(cls):
    if parent == SymId(0): parent = inh
    if inh notin c.vtables:
      loadVTable c, inh

  # now apply the diff:
  let diff = programs.loadVTable(cls)
  var dest = VTable(display: @[], methods: @[], state: Others)
  if parent != SymId(0):
    dest.methods = c.vtables[parent].methods
    dest.signatureToIndex = c.vtables[parent].signatureToIndex

  for entry in diff:
    let sig = pool.strings[entry.signature]
    let idx = dest.signatureToIndex.getOrDefault(sig, -1)
    if idx == -1:
      dest.methods.add entry.fn
      dest.signatureToIndex[sig] = dest.methods.len - 1
    else:
      dest.methods[idx] = entry.fn

  c.vtables[cls] = ensureMove dest

proc getMethodIndex(c: var Context; cls, fn: SymId): int =
  if not c.vtables.hasKey(cls):
    c.loadVTable(cls)

  result = c.vtables[cls].methods.find(fn)
  if result == -1:
    error "method `" & pool.syms[fn] & "` not found in class " & pool.syms[cls]

proc isLocalVar(c: var Context; n: Cursor): bool {.inline.} =
  n.kind == Symbol and getLocalInfo(c.typeCache, n.symId).kind in {VarY, LetY, ResultY, GvarY, GletY, TvarY, TletY}

proc genProctype(c: var Context; dest: var TokenBuf; typ: Cursor) =
  dest.addParLe ProctypeT, NoLineInfo
  dest.addDotToken() # name
  dest.addDotToken() # export marker
  dest.addDotToken() # pattern
  dest.addDotToken() # type vars
  var n = typ
  # params:
  dest.takeTree n
  # return type:
  dest.takeTree n
  # calling convention is always nimcall for now:
  dest.addParLe PragmasU, NoLineInfo
  dest.addParPair Nimcall, NoLineInfo
  dest.addParRi()

  # ignore, effects and body:
  dest.addDotToken() # effects
  dest.addDotToken() # body
  dest.addParRi()

type ClassInfo = object
  root: SymId
  level: int
  ptrKind: TypeKind

proc genVtableField(c: var Context; dest: var TokenBuf; x: Cursor; class: ClassInfo; info: PackedLineInfo)

proc trMethodCall(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let fnNode = n.load()
  var fnType = getType(c.typeCache, n)
  assert fnType.typeKind != AutoT
  let fn = n.symId
  inc n # skip fn
  let typ = getType(c.typeCache, n)
  # We assume "object slicing" here:
  let canUseStaticCall = (typ.typeKind notin {RefT, PtrT} and isLocalVar(c, n)) or typ.isFinal
  let cls = getClass(typ)
  if cls == SymId(0):
    dest.add fnNode
    error "cannot call method `" & pool.syms[fn] & "` on type " & typeToString(typ)
  elif canUseStaticCall:
    dest.add fnNode
  else:
    let info = n.info
    var temp = evalOnce(c, dest, n)

    if fnType.typeKind in RoutineTypes:
      inc fnType
      for i in 1..4: skip fnType

    var paramList = fnType
    assert paramList.substructureKind == ParamsU
    inc paramList
    let param = takeLocal(paramList, SkipFinalParRi)
    if param.typ.typeKind in {RefT, PtrT}:
      # nil check
      if not temp.needsParRi:
        c.needsXelim = true
        dest.addParLe(ExprX, info)
        temp.needsParRi = true
      copyIntoKind dest, CallS, info:
        dest.add symToken(pool.syms.getOrIncl("nimChckNilDisp.0." & SystemModuleSuffix), info)
        useTemp dest, temp, info
    copyIntoKind dest, CastX, info:
      genProctype(c, dest, fnType)
      copyIntoKind dest, PatX, info:
        copyIntoKind dest, DotX, info:
          copyIntoKind dest, DerefX, info:
            var tempUseBuf = createTokenBuf(4)
            useTemp tempUseBuf, temp, info
            var root = SymId(0)
            var level = 0
            for parent in inheritanceChain(cls):
              root = parent
              inc level
            genVtableField c, dest, beginRead(tempUseBuf), ClassInfo(root: root, level: level, ptrKind: typ.typeKind), info
          dest.addSymUse pool.syms.getOrIncl(MethodsField & SystemModuleSuffix), info
          dest.addIntLit 0, info # this is getting stupid...
        let idx = getMethodIndex(c, cls, fn)
        dest.addIntLit idx, info
    closeTemp dest, temp
    # first arg still need to be passed to the method:
    useTemp dest, temp, info
  # other arguments are handled by the regular code:
  while n.kind != ParRi:
    tr c, dest, n

proc maybeImport(c: var Context; cls, vtabName: SymId) =
  if not c.vtables.hasKey(cls):
    c.loadVTable(cls)

  let state = c.vtables[cls].state
  if state == Others:
    c.vtables[cls].state = AlreadyImported
    var decl = createTokenBuf(8)
    decl.copyIntoKind ConstS, NoLineInfo:
      decl.addSymDef vtabName, NoLineInfo
      decl.addIdent("x", NoLineInfo) # exported
      decl.addEmpty() # pragmas
      decl.addSymUse pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix), NoLineInfo
      decl.addEmpty() # value
    programs.publish vtabName, decl

proc trGetRtti(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n # call
  skip n # skip "getRtti" symbol
  assert n.kind == Symbol # we have the class name here
  let typ = getType(c.typeCache, n)
  let cls = getClass(typ)
  let vtabName = getVTableName(c, cls)
  dest.copyIntoKind AddrX, info:
    dest.addSymUse vtabName, info
    maybeImport(c, cls, vtabName)
  inc n
  skipParRi n

proc trObjConstr(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  dest.takeToken n # objconstr
  var cls = SymId(0)
  if n.kind == Symbol and hasRtti(n.symId):
    cls = n.symId

  dest.takeTree n # type
  if cls != SymId(0):
    #dest.copyIntoKind KvU, info:
    #  dest.copyIntoSymUse pool.syms.getOrIncl(VTableField), info
    let vtabName = getVTableName(c, cls)
    dest.copyIntoKind AddrX, info:
      dest.addSymUse vtabName, info
    maybeImport(c, cls, vtabName)
  while n.kind != ParRi:
    tr c, dest, n
  takeParRi dest, n

proc trCall(c: var Context; dest: var TokenBuf; n: var Cursor; forceStaticCall: bool) =
  let fn = n.firstSon
  if not forceStaticCall and fn.kind == Symbol and isMethod(c, fn.symId):
    dest.takeToken n # skip `(call)`
    trMethodCall c, dest, n
    takeParRi dest, n
  elif fn.kind == Symbol and fn.symId == c.getRttiSym:
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

proc genBaseobj(c: var Context; dest: var TokenBuf; x: var Cursor; class: ClassInfo; info: PackedLineInfo) =
  if class.ptrKind != NoType:
    # cast is enough, see trBaseobj
    dest.addParLe(CastX, info)
    dest.addParLe(class.ptrKind, info)
    dest.add symToken(class.root, info)
    dest.addParRi()
    tr c, dest, x
    dest.addParRi()
  else:
    dest.addParLe(BaseobjX, info)
    dest.add symToken(class.root, info)
    dest.add intToken(pool.integers.getOrIncl(class.level), info)
    tr c, dest, x
    dest.addParRi()

proc genVtableField(c: var Context; dest: var TokenBuf; x: Cursor; class: ClassInfo; info: PackedLineInfo) =
  # get vtable field of `x`, might need to get to root object
  copyIntoKind dest, DotX, info:
    if class.ptrKind == RefT:
      # past duplifier, so need to do the deref transform here
      dest.addParLe(DotX, info)
      dest.addParLe(DerefX, info)
    elif class.ptrKind == PtrT:
      dest.addParLe(DerefX, info)

    var x = x
    if class.level == 0:
      tr c, dest, x
    else:
      genBaseobj c, dest, x, class, info

    if class.ptrKind == RefT:
      # past duplifier, so need to do the deref transform here
      dest.addParRi()
      let dataField = pool.syms.getOrIncl(DataField)
      dest.add symToken(dataField, info)
      dest.addIntLit(0, info) # inheritance
      dest.addParRi()
    elif class.ptrKind == PtrT:
      dest.addParRi()

    dest.copyIntoSymUse pool.syms.getOrIncl(VTableField), info
    dest.addIntLit 0, info

proc trInstanceofImpl(c: var Context; dest: var TokenBuf; x, typ: Cursor; info: PackedLineInfo) =
  # `v of T` is translated into a logical 'and' expression:
  # let vtab = v.vtab
  # (level < len(vtab.display)) and (vtab.display[level] == hash(T))
  let vtabTempSym = pool.syms.getOrIncl("`vtableTemp." & $c.tmpCounter)
  inc c.tmpCounter

  var xt = getType(c.typeCache, x)
  let xk = xt.typeKind
  if xk in {RefT, PtrT}:
    inc xt
  var xRoot = SymId(0)
  var xLevel = 0
  if xt.kind == Symbol:
    for parent in inheritanceChain(xt.symId):
      xRoot = parent
      inc xLevel

  c.needsXelim = true
  var x2 = x
  let xTemp = evalOnce(c, dest, x2)
  if xk in {RefT, PtrT}:
    # nil check, always false in old compiler
    dest.addParLe(IfS, info)
    copyIntoKind dest, ElifU, info:
      copyIntoKind dest, EqX, info:
        dest.addParPair(PointerT, info)
        copyIntoKind dest, CastX, info:
          dest.addParPair(PointerT, info)
          useTemp dest, xTemp, info
        dest.addParPair(NilX, info)
      copyIntoKind dest, ExprX, info:
        dest.addParPair(FalseX, info)
    dest.addParLe(ElseU, info)
  copyIntoKind dest, ExprX, info:
    copyIntoKind dest, StmtsS, info:
      copyIntoKind dest, VarS, info:
        dest.addSymDef vtabTempSym, info
        dest.addEmpty2 info # export marker, pragma
        dest.copyIntoKind PtrT, info:
          dest.addSymUse pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix), info

        var tempUseBuf = createTokenBuf(4)
        useTemp tempUseBuf, xTemp, info
        genVtableField c, dest, beginRead(tempUseBuf), ClassInfo(root: xRoot, level: xLevel, ptrKind: xk), info

      # Get the class data (level and hash)
      let (level, h) = classData(typ)

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
          dest.copyIntoSymUse pool.syms.getOrIncl(DisplayLenField & SystemModuleSuffix), info
          dest.addIntLit 0, info

      # Second expression: vtab.display[level] == hash(T)
      copyIntoKind dest, EqX, info:
        copyIntoKind dest, UT, info:
          dest.addIntLit 32, info

        copyIntoKind dest, PatX, info:
          copyIntoKind dest, DotX, info:
            copyIntoKind dest, DerefX, info:
              dest.addSymUse vtabTempSym, info
            dest.copyIntoSymUse pool.syms.getOrIncl(DisplayField & SystemModuleSuffix), info
            dest.addIntLit 0, info
          dest.addIntLit level, info

        dest.addUIntLit h, info
  if xk in {RefT, PtrT}:
    # close nil check
    dest.addParRi()
    dest.addParRi()
  closeTemp dest, xTemp

proc trInstanceof(c: var Context; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n # skip `instanceof`
  let x = n
  skip n
  let typ = n
  skip n # skip type
  skipParRi n
  trInstanceofImpl c, dest, x, typ, info

type
  MaybeTempKind = enum
    UsesSelf
    UsesTempVal
    UsesTempPtr
  MaybeTemp = object
    kind: MaybeTempKind
    sym: SymId

proc needsTemp(c: var Context; n: Cursor): MaybeTemp =
  if n.kind == Symbol:
    result = MaybeTemp(kind: UsesSelf, sym: n.symId)
  else:
    let symId = pool.syms.getOrIncl("`vtableTemp." & $c.tmpCounter)
    inc c.tmpCounter
    let kind = if constructsValue(n): UsesTempVal else: UsesTempPtr
    result = MaybeTemp(kind: kind, sym: symId)

proc trBaseobj(c: var Context; dest: var TokenBuf; nn: var Cursor) =
  let info = nn.info
  var n = nn
  inc n # skip `baseobj`
  let typ = n
  skip n # skip type
  if n.kind == IntLit and pool.integers[n.intId] < 0:
    inc n # integer literal
    let x = n
    skip n # skip expression
    c.needsXelim = true
    copyIntoKind dest, ExprX, info:
      copyIntoKind dest, StmtsS, info:
        let tmp = needsTemp(c, x)
        case tmp.kind
        of UsesSelf:
          discard "nothing to do"
        of UsesTempVal:
          copyIntoKind dest, VarS, info:
            dest.addSymDef tmp.sym, info
            dest.addEmpty2 info # export marker, pragma
            dest.addSubtree getType(c.typeCache, x)
            var x = x
            tr c, dest, x
        of UsesTempPtr:
          copyIntoKind dest, VarS, info:
            dest.addSymDef tmp.sym, info
            dest.addEmpty2 info # export marker, pragma
            copyIntoKind dest, PtrT, info:
              dest.addSubtree getType(c.typeCache, x)
            copyIntoKind dest, AddrX, info:
              var x = x
              tr c, dest, x

        var buf = createTokenBuf(3)
        if tmp.kind == UsesTempPtr:
          copyIntoKind buf, DerefX, info:
            buf.addSymUse tmp.sym, info
        else:
          buf.addSymUse tmp.sym, info

        copyIntoKind dest, IfS, info:
          copyIntoKind dest, ElifU, info:
            copyIntoKind dest, NotX, info:
              trInstanceofImpl c, dest, beginRead(buf), typ, info
            copyIntoKind dest, StmtsS, info:
              copyIntoKind dest, CallS, info:
                dest.add symToken(pool.syms.getOrIncl("nimInvalidObjConv.0." & SystemModuleSuffix), info)
                dest.addStrLit asNimCode(typ)

      # Negative value means we need to produce a runtime check and a cast:
      copyIntoKind dest, DerefX, info:
        copyIntoKind dest, CastX, info:
          copyIntoKind dest, PtrT, info:
            dest.addSubtree typ
          copyIntoKind dest, AddrX, info:
            var bufn = beginRead(buf)
            tr c, dest, bufn
    skipParRi n
  else:
    let isPtr = typ.typeKind in {RefT, PtrT}
    if isPtr:
      # to preserve the refcount field position, cast entire pointer
      # works since the supertype is always the first field
      dest.addParLe(CastX, info)
      dest.addSubtree typ
      skip n
      tr c, dest, n
      takeParRi dest, n
    else:
      n = nn
      copyInto dest, n:
        while n.kind != ParRi:
          tr c, dest, n
  # store back:
  nn = n

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
      of BaseobjX:
        trBaseobj c, dest, n
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

proc processMethod(c: var Context; m: MethodDecl; methodName: string) =
  let sig = methodKey(methodName, m.paramRest)
  # see if this is an override:
  for inh in inheritanceChain(m.cls):
    let methodIndex = c.vtables[inh].signatureToIndex.getOrDefault(sig, -1)
    if methodIndex != -1:
      # register as override:
      c.vtables[m.cls].methods[methodIndex] = m.name
      return
  # not an override, register as a new base method:
  let idx = c.vtables[m.cls].methods.len
  c.vtables[m.cls].methods.add m.name
  c.vtables[m.cls].signatureToIndex[sig] = idx

proc processMethods(c: var Context) =
  # Methods are fundamentally different from other type-bound symbols in
  # that we need to know the base class's entire vtable so that we know
  # which slots to inherit and which to override. This means we need to
  # load an entire vtable from the module that declares the base class.
  for entry in c.classes:
    if entry[1]:
      let cls = entry[0]
      # inherit methods from parent class:
      let parent = c.vtables[cls].parent
      if parent != SymId(0):
        if parent notin c.vtables:
          loadVTable c, parent
        c.vtables[cls].methods = c.vtables[parent].methods
        c.vtables[cls].signatureToIndex = c.vtables[parent].signatureToIndex
      for m in c.methodDecls:
        if m.cls == cls:
          var methodName = pool.syms[m.name]
          extractBasename methodName
          processMethod c, m, methodName

proc registerClass(c: var Context; cls: SymId; inThisModule: bool) =
  for i in 0 ..< c.classes.len:
    if c.classes[i][0] == cls:
      # inThisModule must be "sticky" so do not always overwrite this value:
      if inThisModule: c.classes[i][1] = true
      return
  c.classes.add (cls, inThisModule)

proc collectClass(c: var Context; n: var Cursor) =
  let d = takeTypeDecl(n, SkipFinalParRi)
  var isClass = false
  if d.isGeneric:
    isClass = false
  elif hasPragma(d.pragmas, InheritableP):
    isClass = true
  else:
    var b = d.body
    if b.typeKind in {RefT, PtrT}: inc b
    if b.typeKind == ObjectT:
      inc b
      isClass = b.kind != DotToken
  if isClass:
    let cls = d.name.symId
    # reasonably cheap way to get a topological order:
    var deps = newSeq[SymId]()
    var firstDep = SymId(0)
    for inh in inheritanceChain(cls):
      if firstDep == SymId(0): firstDep = inh
      deps.add inh
    # reverse:
    for i in 0 ..< deps.len div 2:
      swap deps[i], deps[deps.len - 1 - i]
    for i in 0 ..< deps.len:
      registerClass c, deps[i], false
    deps.add cls # add `self` for the display
    c.vtables[cls] = VTable(display: ensureMove deps, methods: @[], parent: firstDep, state: Mine)
    registerClass c, cls, true

proc collectMethods(c: var Context; n: var Cursor) =
  # we only care about top level methods
  case n.stmtKind
  of StmtsS:
    inc n
    while n.kind != ParRi:
      collectMethods c, n
    skipParRi n
  of MethodS:
    let orig = n
    let r = takeRoutine(n, SkipFinalParRi)
    if not r.isGeneric:
      var p = r.params
      if p.kind == ParLe:
        inc p
        let param = takeLocal(p, SkipFinalParRi)
        let cls = getClass(param.typ)
        if cls == SymId(0):
          error "cannot attach method to type " & typeToString(param.typ)
        else:
          # we might not have registered the class yet, so we use a single flat `methodDecls` list:
          c.methodDecls.add MethodDecl(cls: cls, name: r.name.symId, paramRest: p)
      else:
        error "method needs a first parameter of the class type: " & toString(orig, false)
  of TypeS:
    collectClass c, n
  else:
    skip n

proc emitVTables(c: var Context; dest: var TokenBuf) =
  # Used the `mpairs` and `mitems` here to avoid copies.
  for cls, vtab in mpairs c.vtables:
    if vtab.state != Mine: continue
    var displayName = SymId(0)
    if vtab.display.len > 0:
      displayName = computeVTableName(c, cls, ".dy.")
      dest.copyIntoKind ConstS, NoLineInfo:
        dest.addSymDef displayName, NoLineInfo
        dest.addIdent "x", NoLineInfo # export the vtable!
        dest.addEmpty() # pragmas
        dest.copyIntoKind UarrayT, NoLineInfo:
          dest.copyIntoKind UT, NoLineInfo:
            dest.addIntLit 32, NoLineInfo
          #dest.addIntLit vtab.display.len, NoLineInfo
        dest.addParLe AconstrX, NoLineInfo
        dest.copyIntoKind UarrayT, NoLineInfo:
          dest.copyIntoKind UT, NoLineInfo:
            dest.addIntLit 32, NoLineInfo
        for d in vtab.display:
          dest.addUIntLit uhash(pool.syms[d]), NoLineInfo
        dest.addParRi() # AconstrX

    dest.copyIntoKind ConstS, NoLineInfo:
      dest.addSymDef getVTableName(c, cls), NoLineInfo
      dest.addIdent "x", NoLineInfo # export the vtable!
      dest.addEmpty() # pragmas
      dest.addSymUse pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix), NoLineInfo
      dest.copyIntoKind OconstrX, NoLineInfo:
        dest.addSymUse pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix), NoLineInfo

        dest.addParLe KvU, NoLineInfo
        dest.addSymUse pool.syms.getOrIncl(DisplayLenField & SystemModuleSuffix), NoLineInfo
        dest.addIntLit vtab.display.len, NoLineInfo
        dest.addParRi() # KvU

        dest.addParLe KvU, NoLineInfo
        dest.addSymUse pool.syms.getOrIncl(DisplayField & SystemModuleSuffix), NoLineInfo
        if displayName != SymId(0):
          #dest.copyIntoKind AddrX, NoLineInfo:
          # cast to pointer type to remove `const` modifier in C
          copyIntoKind dest, CastX, NoLineInfo:
            copyIntoKind dest, PtrT, NoLineInfo:
              copyIntoKind dest, UT, NoLineInfo:
                dest.addIntLit 32, NoLineInfo
            dest.addSymUse displayName, NoLineInfo
        else:
          dest.addParPair NilX, NoLineInfo
        dest.addParRi() # KvU

        dest.addParLe KvU, NoLineInfo
        dest.addSymUse pool.syms.getOrIncl(MethodsField & SystemModuleSuffix), NoLineInfo
        if vtab.methods.len > 0:
          dest.addParLe AconstrX, NoLineInfo
          # array constructor also starts with a type, yuck:
          dest.copyIntoKind UarrayT, NoLineInfo:
            dest.addParPair PointerT, NoLineInfo
          for m in vtab.methods:
            dest.copyIntoKind CastX, NoLineInfo:
              dest.addParPair PointerT, NoLineInfo
              dest.addSymUse m, NoLineInfo
          dest.addParRi() # AconstrX
        else:
          # flexible array needs at least one element:
          dest.copyIntoKind AconstrX, NoLineInfo:
            dest.addParPair PointerT, NoLineInfo
            dest.addParPair NilX, NoLineInfo
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
  collectMethods c, n2
  processMethods c

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

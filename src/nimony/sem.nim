#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Semantic checking:
## Most important task is to turn identifiers into symbols and to perform
## type checking.

import std / [tables, sets, syncio, formatfloat, assertions, packedsets, strutils]
include nifprelude
import nimony_model, symtabs, builtintypes, decls, symparser, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig, nifindexes,
  intervals, xints, typeprops,
  semdata, sembasics, semos, expreval, semborrow, enumtostr, derefs, sizeof, renderer

import ".." / gear2 / modnames
import ".." / models / tags

# ------------------ include/import handling ------------------------

proc semStmt(c: var SemContext; n: var Cursor; isNewScope: bool)

proc typeMismatch(c: var SemContext; info: PackedLineInfo; got, expected: TypeCursor) =
  c.buildErr info, "type mismatch: got: " & typeToString(got) & " but wanted: " & typeToString(expected)

proc typecheck(c: var SemContext; info: PackedLineInfo; got, expected: TypeCursor) =
  if sameTrees(expected, got):
    discard "fine"
  else:
    c.typeMismatch info, got, expected

proc combineType(c: var SemContext; info: PackedLineInfo; dest: var Cursor; src: Cursor) =
  if typeKind(dest) == AutoT:
    dest = src
  elif sameTrees(dest, src):
    discard "fine"
  else:
    c.typeMismatch info, src, dest

proc implicitlyDiscardable(n: Cursor, noreturnOnly = false): bool =
  template checkBranch(branch) =
    if not implicitlyDiscardable(branch, noreturnOnly):
      return false

  var it = n
  #const
  #  skipForDiscardable = {nkStmtList, nkStmtListExpr,
  #    nkOfBranch, nkElse, nkFinally, nkExceptBranch,
  #    nkElifBranch, nkElifExpr, nkElseExpr, nkBlockStmt, nkBlockExpr,
  #    nkHiddenStdConv, nkHiddenSubConv, nkHiddenDeref}
  while it.kind == ParLe and stmtKind(it) in {StmtsS, BlockS}:
    inc it
    var last = it
    while true:
      skip it
      if it.kind == ParRi:
        it = last
        break
      else:
        last = it

  if it.kind != ParLe: return false
  case stmtKind(it)
  of IfS:
    inc it
    while it.kind != ParRi:
      case it.substructureKind
      of ElifU:
        inc it
        skip it # condition
        checkBranch(it)
        skip it
        skipParRi it
      of ElseU:
        inc it
        checkBranch(it)
        skip it
        skipParRi it
      else:
        error "illformed AST: `elif` or `else` inside `if` expected, got ", it
    # all branches are discardable
    result = true
  of CaseS:
    inc it
    while it.kind != ParRi:
      case it.substructureKind
      of OfU:
        inc it
        skip it # ranges
        checkBranch(it)
        skip it
        skipParRi it
      of ElifU:
        inc it
        skip it # condition
        checkBranch(it)
        skip it
        skipParRi it
      of ElseU:
        inc it
        checkBranch(it)
        skip it
        skipParRi it
      else:
        error "illformed AST: `of`, `elif` or `else` inside `case` expected, got ", it
    # all branches are discardable
    result = true
  #of TryS:
  #  checkBranch(it[0])
  #  for i in 1 ..< it.len:
  #    let branch = it[i]
  #    if branch.kind != nkFinally:
  #      checkBranch(branch[^1])
  #  # all branches are discardable
  #  result = true
  of CallS, CmdS:
    inc it
    if it.kind == Symbol:
      let sym = tryLoadSym(it.symId)
      if sym.status == LacksNothing:
        var decl = sym.decl
        if isRoutine(symKind(decl)):
          inc decl
          skip decl # name
          skip decl # exported
          skip decl # pattern
          skip decl # typevars
          skip decl # params
          skip decl # retType
          # decl should now be pragmas:
          inc decl
          let accepted =
            if noreturnOnly: {NoreturnP}
            else: {DiscardableP, NoreturnP}
          while decl.kind != ParRi:
            if pragmaKind(decl) in accepted:
              return true
            skip decl
    result = false
  of RetS, BreakS, ContinueS, RaiseS:
    result = true
  else:
    result = false

proc isNoReturn(n: Cursor): bool {.inline.} =
  result = implicitlyDiscardable(n, noreturnOnly = true)

proc requestRoutineInstance(c: var SemContext; origin: SymId;
                            typeArgs: TokenBuf;
                            inferred: var Table[SymId, Cursor];
                            info: PackedLineInfo): ProcInstance

proc tryConverterMatch(c: var SemContext; convMatch: var Match; f: TypeCursor, arg: Item): bool

template emptyNode(): Cursor =
  # XXX find a better solution for this
  c.types.voidType

proc commonType(c: var SemContext; it: var Item; argBegin: int; expected: TypeCursor) =
  if typeKind(expected) == AutoT:
    return
  elif typeKind(it.typ) == AutoT:
    it.typ = expected
    return

  let info = it.n.info
  var m = createMatch(addr c)
  var arg = Item(n: cursorAt(c.dest, argBegin), typ: it.typ)
  if typeKind(arg.typ) == VoidT and isNoReturn(arg.n):
    # noreturn allowed in expression context
    # maybe use sem flags to restrict this to statement branches
    discard
  else:
    typematch m, expected, arg
  endRead(c.dest)
  if m.err:
    # try converter
    var convMatch = default(Match)
    if tryConverterMatch(c, convMatch, expected, arg):
      shrink c.dest, argBegin
      c.dest.add parLeToken(HcallX, info)
      c.dest.add symToken(convMatch.fn.sym, info)
      if convMatch.genericConverter:
        let inst = c.requestRoutineInstance(convMatch.fn.sym, convMatch.typeArgs, convMatch.inferred, arg.n.info)
        c.dest[c.dest.len-1].setSymId inst.targetSym
      # ignore genericEmpty case, probably environment is generic
      c.dest.add convMatch.args
      c.dest.addParRi()
      it.typ = expected
    else:
      c.typeMismatch info, it.typ, expected
  else:
    shrink c.dest, argBegin
    c.dest.add m.args
    it.typ = expected

proc producesVoid(c: var SemContext; info: PackedLineInfo; dest: var Cursor) =
  if typeKind(dest) in {AutoT, VoidT}:
    combineType c, info, dest, c.types.voidType
  else:
    c.typeMismatch info, c.types.voidType, dest

proc producesNoReturn(c: var SemContext; info: PackedLineInfo; dest: var Cursor) =
  if typeKind(dest) in {AutoT, VoidT}:
    combineType c, info, dest, c.types.voidType
  else:
    # allowed in expression context
    discard

proc semInclude(c: var SemContext; it: var Item) =
  var files: seq[ImportedFilename] = @[]
  var hasError = false
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `include`
  while x.kind != ParRi:
    filenameVal(x, files, hasError, allowAs = false)

  if hasError:
    c.buildErr info, "wrong `include` statement"
  else:
    for f1 in items(files):
      let f2 = resolveFile(c.g.config.paths, getFile(info), f1.path)
      c.meta.includedFiles.add f2
      # check for recursive include files:
      var isRecursive = false
      for a in c.includeStack:
        if a == f2:
          isRecursive = true
          break

      if not isRecursive:
        var buf = parseFile(f2, c.g.config.paths)
        c.includeStack.add f2
        #c.m.includes.add f2
        var n = cursorAt(buf, 0)
        semStmt(c, n, false)
        c.includeStack.setLen c.includeStack.len - 1
      else:
        var m = ""
        for i in 0..<c.includeStack.len:
          m.add shortenDir c.includeStack[i]
          m.add " -> "
        m.add shortenDir f2
        c.buildErr info, "recursive include: " & m

  producesVoid c, info, it.typ

type
  ImportModeKind = enum
    ImportAll, FromImport, ImportExcept, ImportSystem

  ImportMode = object
    kind: ImportModeKind
    list: PackedSet[StrId] # `from import` or `import except` symbol list

proc importSingleFile(c: var SemContext; f1: ImportedFilename; origin: string; mode: ImportMode; info: PackedLineInfo) =
  let f2 = resolveFile(c.g.config.paths, origin, f1.path)
  let suffix = moduleSuffix(f2, c.g.config.paths)
  if not c.processedModules.containsOrIncl(suffix):
    c.meta.importedFiles.add f2
    if c.canSelfExec and needsRecompile(f2, suffixToNif suffix):
      selfExec c, f2, (if mode.kind == ImportSystem: " --isSystem" else: "")

    let moduleName = pool.strings.getOrIncl(f1.name)
    let moduleSym = identToSym(c, moduleName, ModuleY)
    let s = Sym(kind: ModuleY, name: moduleSym, pos: ImportedPos)
    c.currentScope.addOverloadable(moduleName, s)
    var moduleDecl = createTokenBuf(2)
    moduleDecl.addParLe(ModuleY, info)
    moduleDecl.addParRi()
    publish moduleSym, moduleDecl
    var module = ImportedModule()
    var marker = mode.list
    loadInterface suffix, module.iface, moduleSym, c.importTab, c.converters,
      marker, negateMarker = mode.kind == FromImport
    c.importedModules[moduleSym] = module

proc cyclicImport(c: var SemContext; x: var Cursor) =
  c.buildErr x.info, "cyclic module imports are not implemented"

proc doImportMode(c: var SemContext; files: seq[ImportedFilename]; mode: ImportMode; info: PackedLineInfo) =
  let origin = getFile(info)
  for f in files:
    importSingleFile c, f, origin, mode, info

proc semImport(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `import`

  if x.kind == ParLe and x == "pragmax":
    inc x
    var y = x
    skip y
    if y.substructureKind == PragmasU:
      inc y
      if y.kind == Ident and pool.strings[y.litId] == "cyclic":
        cyclicImport(c, x)
        return

  var files: seq[ImportedFilename] = @[]
  var hasError = false
  while x.kind != ParRi:
    filenameVal(x, files, hasError, allowAs = true)
  if hasError:
    c.buildErr info, "wrong `import` statement"
  else:
    doImportMode c, files, ImportMode(kind: ImportAll, list: initPackedSet[StrId]()), info

  producesVoid c, info, it.typ

proc semImportExcept(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `importexcept`

  if x.kind == ParLe and x == "pragmax":
    inc x
    var y = x
    skip y
    if y.substructureKind == PragmasU:
      inc y
      if y.kind == Ident and pool.strings[y.litId] == "cyclic":
        cyclicImport(c, x)
        return

  var files: seq[ImportedFilename] = @[]
  var hasError = false
  filenameVal(x, files, hasError, allowAs = true)
  if hasError:
    c.buildErr info, "wrong `import except` statement"
  else:
    var excluded = initPackedSet[StrId]()
    while x.kind != ParRi:
      excluded.incl getIdent(x)
    doImportMode c, files, ImportMode(kind: ImportExcept, list: excluded), info

  producesVoid c, info, it.typ

proc semFromImport(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `from`

  if x.kind == ParLe and x == "pragmax":
    inc x
    var y = x
    skip y
    if y.substructureKind == PragmasU:
      inc y
      if y.kind == Ident and pool.strings[y.litId] == "cyclic":
        cyclicImport(c, x)
        return

  var files: seq[ImportedFilename] = @[]
  var hasError = false
  filenameVal(x, files, hasError, allowAs = true)
  if hasError:
    c.buildErr info, "wrong `from import` statement"
  else:
    var included = initPackedSet[StrId]()
    while x.kind != ParRi:
      if x.kind == ParLe and x == $NilX:
        # from a import nil
        discard
      else:
        included.incl getIdent(x)
    doImportMode c, files, ImportMode(kind: FromImport, list: included), info

  producesVoid c, info, it.typ

# -------------------- declare `result` -------------------------

proc classifyType(c: var SemContext; n: Cursor): TypeKind =
  result = typeKind(n)

proc declareResult(c: var SemContext; info: PackedLineInfo): SymId =
  if c.routine.kind in {ProcY, FuncY, ConverterY, MethodY, MacroY} and
      classifyType(c, c.routine.returnType) != VoidT:
    let name = pool.strings.getOrIncl("result")
    result = identToSym(c, name, ResultY)
    let s = Sym(kind: ResultY, name: result,
                pos: c.dest.len)
    discard c.currentScope.addNonOverloadable(name, s)
    c.routine.resId = result

    let declStart = c.dest.len
    buildTree c.dest, ResultS, info:
      c.dest.add symdefToken(result, info) # name
      c.dest.addDotToken() # export marker
      if NoinitP in c.routine.pragmas:
        c.dest.add parLeToken(PragmasU, info)
        c.dest.add parLeToken(NoinitP, info)
        c.dest.addParRi()
        c.dest.addParRi()
      else:
        c.dest.addDotToken() # pragmas
      c.dest.copyTree(c.routine.returnType) # type
      c.dest.addDotToken() # value
    publish c, result, declStart
  else:
    result = SymId(0)

# -------------------- generics ---------------------------------

proc newSymId(c: var SemContext; s: SymId): SymId =
  var isGlobal = false
  var name = extractBasename(pool.syms[s], isGlobal)
  if isGlobal:
    c.makeGlobalSym(name)
  else:
    c.makeLocalSym(name)
  result = pool.syms.getOrIncl(name)

type
  SubsContext = object
    newVars: Table[SymId, SymId]
    params: ptr Table[SymId, Cursor]

proc addFreshSyms(c: var SemContext, sc: var SubsContext) =
  for _, newVar in sc.newVars:
    c.freshSyms.incl newVar

proc subs(c: var SemContext; dest: var TokenBuf; sc: var SubsContext; body: Cursor) =
  var nested = 0
  var n = body
  while true:
    case n.kind
    of UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
      dest.add n
    of Symbol:
      let s = n.symId
      let arg = sc.params[].getOrDefault(s)
      if arg != default(Cursor):
        dest.addSubtree arg
      else:
        let nv = sc.newVars.getOrDefault(s)
        if nv != SymId(0):
          dest.add symToken(nv, n.info)
        else:
          dest.add n # keep Symbol as it was
    of SymbolDef:
      let s = n.symId
      let newDef = newSymId(c, s)
      sc.newVars[s] = newDef
      dest.add symdefToken(newDef, n.info)
    of ParLe:
      dest.add n
      inc nested
    of ParRi:
      dest.add n
      dec nested
    if nested == 0: break
    inc n

include templates

proc produceInvoke(c: var SemContext; dest: var TokenBuf; req: InstRequest;
                   typeVars: Cursor; info: PackedLineInfo) =
  dest.buildTree InvokeT, info:
    dest.add symToken(req.origin, info)
    var typeVars = typeVars
    if typeVars.substructureKind == TypevarsU:
      inc typeVars
      while typeVars.kind != ParRi:
        if typeVars.symKind == TypeVarY:
          var tv = typeVars
          inc tv
          dest.copyTree req.inferred[tv.symId]
        skip typeVars

proc subsGenericType(c: var SemContext; dest: var TokenBuf; req: InstRequest) =
  #[
  What we need to do is rather simple: A generic instantiation is
  the typical (type :Name ex generic_params pragmas body) tuple but
  this time the generic_params list the used `Invoke` construct for the
  instantiation.
  ]#
  let info = req.requestFrom[^1]
  let decl = getTypeSection(req.origin)
  dest.buildTree TypeS, info:
    dest.add symdefToken(req.targetSym, info)
    dest.addDotToken() # export
    produceInvoke c, dest, req, decl.typevars, info
    # take the pragmas from the origin:
    dest.copyTree decl.pragmas
    var sc = SubsContext(params: addr req.inferred)
    subs(c, dest, sc, decl.body)
    addFreshSyms(c, sc)

proc subsGenericProc(c: var SemContext; dest: var TokenBuf; req: InstRequest) =
  let info = req.requestFrom[^1]
  let decl = getProcDecl(req.origin)
  dest.buildTree decl.kind, info:
    dest.add symdefToken(req.targetSym, info)
    if decl.exported.kind == ParLe:
      # magic?
      dest.copyTree decl.exported
    else:
      dest.addDotToken()
    dest.copyTree decl.pattern
    produceInvoke c, dest, req, decl.typevars, info

    var sc = SubsContext(params: addr req.inferred)
    subs(c, dest, sc, decl.params)
    subs(c, dest, sc, decl.retType)
    subs(c, dest, sc, decl.pragmas)
    subs(c, dest, sc, decl.effects)
    subs(c, dest, sc, decl.body)
    addFreshSyms(c, sc)

template withFromInfo(req: InstRequest; body: untyped) =
  let oldLen = c.instantiatedFrom.len
  for jnfo in items(req.requestFrom):
    pushErrorContext c, jnfo
  body
  shrink c.instantiatedFrom, oldLen

type
  TypeDeclContext = enum
    InLocalDecl, InTypeSection, InReturnTypeDecl, AllowValues,
    InGenericConstraint

proc semLocalTypeImpl(c: var SemContext; n: var Cursor; context: TypeDeclContext)

proc semLocalType(c: var SemContext; n: var Cursor; context = InLocalDecl): TypeCursor =
  let insertPos = c.dest.len
  semLocalTypeImpl c, n, context
  assert c.dest.len > insertPos
  result = typeToCursor(c, insertPos)

proc semTypeSection(c: var SemContext; n: var Cursor)
proc instantiateGenericType(c: var SemContext; req: InstRequest) =
  var dest = createTokenBuf(30)
  withFromInfo req:
    subsGenericType c, dest, req
    var n = beginRead(dest)
    semTypeSection c, n

proc instantiateType(c: var SemContext; typ: Cursor; bindings: Table[SymId, Cursor]): Cursor =
  var dest = createTokenBuf(30)
  var sc = SubsContext(params: addr bindings)
  subs(c, dest, sc, typ)
  var sub = beginRead(dest)
  var instDest = createTokenBuf(30)
  swap c.dest, instDest
  result = semLocalType(c, sub)
  swap c.dest, instDest

type
  PassKind = enum checkSignatures, checkBody, checkGenericInst, checkConceptProc

proc semProc(c: var SemContext; it: var Item; kind: SymKind; pass: PassKind)
proc instantiateGenericProc(c: var SemContext; req: InstRequest) =
  var dest = createTokenBuf(40)
  withFromInfo req:
    subsGenericProc c, dest, req
    var it = Item(n: beginRead(dest), typ: c.types.autoType)
    #echo "now in generic proc: ", toString(it.n)
    semProc c, it, it.n.symKind, checkGenericInst

proc instantiateGenerics(c: var SemContext) =
  while c.typeRequests.len + c.procRequests.len > 0:
    # This way with `move` ensures it is safe even though
    # the semchecking of generics can add to `c.typeRequests`
    # or to `c.procRequests`. This is subtle!
    let typeReqs = move(c.typeRequests)
    for t in typeReqs: instantiateGenericType c, t
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, p

proc instantiateGenericHooks(c: var SemContext) =
  ## hooks are instantiated after all generics have been instantiated
  while c.procRequests.len > 0:
    let procReqs = move(c.procRequests)
    for p in procReqs: instantiateGenericProc c, p

# -------------------- sem checking -----------------------------

type
  SemFlag = enum
    KeepMagics
    AllowOverloads
    PreferIterators
    AllowUndeclared
    AllowModuleSym
    AllowEmpty
    InTypeContext

proc semExpr(c: var SemContext; it: var Item; flags: set[SemFlag] = {})

proc fetchSym(c: var SemContext; s: SymId): Sym =
  # yyy find a better solution
  var name = pool.syms[s]
  extractBasename name
  let identifier = pool.strings.getOrIncl(name)
  var it {.cursor.} = c.currentScope
  while it != nil:
    for sym in it.tab.getOrDefault(identifier):
      if sym.name == s:
        return sym
    it = it.up

  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    result = Sym(kind: symKind(res.decl), name: s, pos: ImportedPos)
  else:
    result = Sym(kind: NoSym, name: s, pos: InvalidPos)

proc semBoolExpr(c: var SemContext; n: var Cursor) =
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if classifyType(c, it.typ) != BoolT:
    buildErr c, it.n.info, "expected `bool` but got: " & typeToString(it.typ)

proc semConstBoolExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if classifyType(c, it.typ) != BoolT:
    buildErr c, it.n.info, "expected `bool` but got: " & typeToString(it.typ)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstBoolValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, it.n.info, "expected constant bool value but got: " & asNimCode(value)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstStrExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if not isStringType(it.typ):
    buildErr c, it.n.info, "expected `string` but got: " & typeToString(it.typ)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstStringValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, it.n.info, "expected constant string value but got: " & asNimCode(value)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstIntExpr(c: var SemContext; n: var Cursor) =
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  if classifyType(c, it.typ) != IntT:
    buildErr c, it.n.info, "expected `int` but got: " & typeToString(it.typ)
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  let value = cursorAt(valueBuf, 0)
  if not isConstIntValue(value):
    if value.kind == ParLe and value.tagId == ErrT:
      c.dest.add valueBuf
    else:
      buildErr c, it.n.info, "expected constant integer value but got: " & asNimCode(value)
  else:
    c.dest.shrink start
    c.dest.add valueBuf

proc semConstExpr(c: var SemContext; it: var Item) =
  let start = c.dest.len
  semExpr c, it
  var e = cursorAt(c.dest, start)
  var valueBuf = evalExpr(c, e)
  endRead(c.dest)
  c.dest.shrink start
  c.dest.add valueBuf

proc semStmtsExprImpl(c: var SemContext; it: var Item) =
  while it.n.kind != ParRi:
    if not isLastSon(it.n):
      semStmt c, it.n, false
    else:
      semExpr c, it
  takeParRi c, it.n

proc semStmtsExpr(c: var SemContext; it: var Item; isNewScope: bool) =
  let before = c.dest.len
  takeToken c, it.n
  semStmtsExprImpl c, it
  let kind =
    if classifyType(c, it.typ) == VoidT:
      (if isNewScope: ScopeTagId else: StmtsTagId)
    else: ExprTagId
  c.dest[before] = parLeToken(TagId(kind), c.dest[before].info)

proc semProcBody(c: var SemContext; itB: var Item) =
  let beforeBodyPos = c.dest.len
  let info = itB.n.info
  var it = Item(n: itB.n, typ: c.types.autoType)
  semStmtsExprImpl c, it
  if c.routine.kind == TemplateY:
    case c.routine.returnType.typeKind
    of UntypedT:
      discard "ok"
    of VoidT:
      typecheck(c, info, it.typ, c.routine.returnType)
    else:
      # uses closing paren of (stmts:
      c.dest.insert [parLeToken(ExprX, info)], beforeBodyPos
      commonType c, it, beforeBodyPos, c.routine.returnType
      # now add closing paren
      c.dest.addParRi()
  elif classifyType(c, it.typ) == VoidT:
    discard "ok"
  else:
    # uses closing paren of (stmts:
    c.dest.insert [parLeToken(ExprX, info)], beforeBodyPos
    commonType c, it, beforeBodyPos, c.routine.returnType
    # now add closing paren
    c.dest.addParRi()
    # transform `expr` to `result = expr`:
    if c.routine.resId != SymId(0):
      var prefix = [
        parLeToken(AsgnS, info),
        symToken(c.routine.resId, info)]
      c.dest.insert prefix, beforeBodyPos
      c.dest.addParRi()
  itB.n = it.n

proc semStmt(c: var SemContext; n: var Cursor; isNewScope: bool) =
  let info = n.info
  var it = Item(n: n, typ: c.types.autoType)
  let exPos = c.dest.len
  semExpr c, it
  if classifyType(c, it.typ) in {NoType, VoidT, AutoT, UntypedT}:
    discard "ok"
  else:
    # analyze the expression that was just produced:
    let ex = cursorAt(c.dest, exPos)
    let discardable = implicitlyDiscardable(ex)
    endRead(c.dest)
    if not discardable:
      buildErr c, info, "expression of type `" & typeToString(it.typ) & "` must be discarded"
  n = it.n

template skipToLocalType(n) =
  inc n # skip ParLe
  inc n # skip name
  skip n # skip export marker
  skip n # skip pragmas

template skipToParams(n) =
  inc n # skip ParLe
  skip n # skip name
  skip n # skip export marker
  skip n # skip pattern
  skip n # skip generics

proc fetchCallableType(c: var SemContext; n: Cursor; s: Sym): TypeCursor =
  if s.kind == NoSym:
    c.buildErr n.info, "undeclared identifier"
    result = c.types.autoType
  else:
    let res = declToCursor(c, s)
    if res.status == LacksNothing:
      var d = res.decl
      if s.kind.isLocal:
        skipToLocalType d
        if d.typeKind == ProctypeT:
          skipToParams d
      elif s.kind.isRoutine:
        skipToParams d
      else:
        # consider not callable
        discard
      result = d
    else:
      c.buildErr n.info, "could not load symbol: " & pool.syms[s.name] & "; errorCode: " & $res.status
      result = c.types.autoType

proc pickBestMatch(c: var SemContext; m: openArray[Match]): int =
  result = -1
  var other = -1
  for i in 0..<m.len:
    if not m[i].err:
      if result < 0:
        result = i
      else:
        case cmpMatches(m[result], m[i])
        of NobodyWins:
          other = i
          #echo "ambiguous ", pool.syms[m[result].fn.sym], " vs ", pool.syms[m[i].fn.sym]
        of FirstWins:
          discard "result remains the same"
        of SecondWins:
          result = i
          other = -1
  if other >= 0: result = -2 # ambiguous

const
  ConceptProcY = CchoiceY

type MagicCallKind = enum
  NonMagicCall, MagicCall, MagicCallNeedsSemcheck

proc addFn(c: var SemContext; fn: FnCandidate; fnOrig: Cursor; args: openArray[Item]): MagicCallKind =
  result = NonMagicCall
  if fn.kind in RoutineKinds:
    assert fn.sym != SymId(0)
    let res = tryLoadSym(fn.sym)
    if res.status == LacksNothing:
      var n = res.decl
      inc n # skip the symbol kind
      if n.kind == SymbolDef:
        inc n # skip the SymbolDef
        if n.kind == ParLe:
          if n.exprKind in {DefinedX, DeclaredX, CompilesX, TypeofX,
              LowX, HighX, AddrX, EnumToStrX, DefaultObjX, DefaultTupX,
              ArrAtX, DerefX, TupAtX}:
            # magic needs semchecking after overloading
            result = MagicCallNeedsSemcheck
          else:
            result = MagicCall
          # ^ export marker position has a `(`? If so, it is a magic!
          copyKeepLineInfo c.dest[c.dest.len-1], n.load # overwrite the `(call` node with the magic itself
          inc n
          if n.kind == IntLit:
            if pool.integers[n.intId] == TypedMagic:
              c.dest.addSubtree skipModifier(args[0].typ)
            else:
              c.dest.add n
            inc n
          if n.kind != ParRi:
            error "broken `magic`: expected ')', but got: ", n
    if result == NonMagicCall:
      c.dest.add symToken(fn.sym, fnOrig.info)
  elif fn.kind == ConceptProcY and fn.sym != SymId(0):
    c.dest.add identToken(symToIdent(fn.sym), fnOrig.info)
  else:
    c.dest.addSubtree fnOrig

proc semTemplateCall(c: var SemContext; it: var Item; fnId: SymId; beforeCall: int;
                     m: Match) =
  var expandedInto = createTokenBuf(30)

  let s = fetchSym(c, fnId)
  let res = declToCursor(c, s)
  if res.status == LacksNothing:
    let args = cursorAt(c.dest, beforeCall + 2)
    let firstVarargMatch = cursorAt(c.dest, beforeCall + 2 + m.firstVarargPosition)
    expandTemplate(c, expandedInto, res.decl, args, firstVarargMatch, addr m.inferred)
    # We took 2 cursors, so we have to do the `endRead` twice too:
    endRead(c.dest)
    endRead(c.dest)
    shrink c.dest, beforeCall
    expandedInto.addParRi() # extra token so final `inc` doesn't break
    var a = Item(n: cursorAt(expandedInto, 0), typ: c.types.autoType)
    semExpr c, a
    it.typ = a.typ
    it.kind = a.kind
  else:
    c.buildErr it.n.info, "could not load symbol: " & pool.syms[fnId] & "; errorCode: " & $res.status

proc sameIdent(sym: SymId; str: StrId): bool =
  # XXX speed this up by using the `fieldCache` idea
  var name = pool.syms[sym]
  extractBasename(name)
  result = pool.strings.getOrIncl(name) == str

proc sameIdent(a, b: SymId): bool =
  # XXX speed this up by using the `fieldCache` idea
  var x = pool.syms[a]
  extractBasename(x)
  var y = pool.syms[b]
  extractBasename(y)
  result = x == y

type
  FnCandidates = object
    a: seq[FnCandidate]
    s: HashSet[SymId]

proc addUnique(c: var FnCandidates; x: FnCandidate) =
  if not containsOrIncl(c.s, x.sym):
    c.a.add x

proc maybeAddConceptMethods(c: var SemContext; fn: StrId; typevar: SymId; cands: var FnCandidates) =
  let res = tryLoadSym(typevar)
  assert res.status == LacksNothing
  let local = asLocal(res.decl)
  if local.kind == TypevarY and local.typ.kind == Symbol:
    let concpt = local.typ.symId
    let section = getTypeSection concpt

    var ops = section.body
    inc ops  # (concept
    skip ops # .
    skip ops # .
    skip ops #   (typevar Self ...)
    if ops == "stmts":
      inc ops
      while ops.kind != ParRi:
        let sk = ops.symKind
        if sk in RoutineKinds:
          var prc = ops
          inc prc # (proc
          if prc.kind == SymbolDef and sameIdent(prc.symId, fn):
            var d = ops
            skipToParams d
            cands.addUnique FnCandidate(kind: ConceptProcY, sym: prc.symId, typ: d)
        skip ops

proc considerTypeboundOps(c: var SemContext; m: var seq[Match]; candidates: FnCandidates; args: openArray[Item], genericArgs: Cursor) =
  for candidate in candidates.a:
    m.add createMatch(addr c)
    sigmatch(m[^1], candidate, args, genericArgs)

proc requestRoutineInstance(c: var SemContext; origin: SymId;
                            typeArgs: TokenBuf;
                            inferred: var Table[SymId, Cursor];
                            info: PackedLineInfo): ProcInstance =
  let key = typeToCanon(typeArgs, 0)
  var targetSym = c.instantiatedProcs.getOrDefault((origin, key))
  if targetSym == SymId(0):
    let targetSym = newSymId(c, origin)
    var signature = createTokenBuf(30)
    let decl = getProcDecl(origin)
    assert decl.typevars == "typevars", pool.syms[origin]
    buildTree signature, decl.kind, info:
      signature.add symdefToken(targetSym, info)
      signature.addDotToken() # a generic instance is not exported
      signature.copyTree decl.pattern
      # InvokeT for the generic params:
      signature.buildTree InvokeT, info:
        signature.add symToken(origin, info)
        signature.add typeArgs
      var sc = SubsContext(params: addr inferred)
      subs(c, signature, sc, decl.params)
      let beforeRetType = signature.len
      subs(c, signature, sc, decl.retType)
      subs(c, signature, sc, decl.pragmas)
      subs(c, signature, sc, decl.effects)
      addFreshSyms(c, sc)
      signature.addDotToken() # no body

    result = ProcInstance(targetSym: targetSym, procType: cursorAt(signature, 0),
      returnType: cursorAt(signature, beforeRetType))
    publish targetSym, ensureMove signature

    c.instantiatedProcs[(origin, key)] = targetSym
    var req = InstRequest(
      origin: origin,
      targetSym: targetSym,
      inferred: move(inferred)
    )
    for ins in c.instantiatedFrom: req.requestFrom.add ins
    req.requestFrom.add info

    c.procRequests.add ensureMove req
  else:
    let res = tryLoadSym(targetSym)
    assert res.status == LacksNothing
    var n = res.decl
    skipToParams n
    skip n
    result = ProcInstance(targetSym: targetSym, procType: res.decl,
      returnType: n)
  assert result.returnType.kind != UnknownToken

proc typeofCallIs(c: var SemContext; it: var Item; beforeCall: int; returnType: TypeCursor) {.inline.} =
  let expected = it.typ
  it.typ = returnType
  commonType c, it, beforeCall, expected

proc getFnIdent(c: var SemContext): StrId =
  var n = beginRead(c.dest)
  result = getIdent(n)
  endRead(c.dest)

type
  DotExprState = enum
    MatchedDotField ## matched a dot field, i.e. result is a dot expression
    MatchedDotSym ## matched a qualified identifier
    FailedDot
    InvalidDot

proc tryBuiltinDot(c: var SemContext; it: var Item; lhs: Item; fieldName: StrId; info: PackedLineInfo; flags: set[SemFlag]): DotExprState

proc tryBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item): bool
proc semBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item)

type
  TransformedCallSource = enum
    RegularCall, MethodCall,
    DotCall, SubscriptCall,
    DotAsgnCall, SubscriptAsgnCall

  CallState = object
    beforeCall: int
    fn: Item
    fnKind: SymKind
    fnName: StrId
    callNode: PackedToken
    dest, genericDest: TokenBuf
    args: seq[Item]
    hasGenericArgs: bool
    flags: set[SemFlag]
    candidates: FnCandidates
    source: TransformedCallSource
      ## type of expression the call was transformed from

proc untypedCall(c: var SemContext; it: var Item; cs: CallState) =
  c.dest.add cs.callNode
  c.dest.addSubtree cs.fn.n
  for a in cs.args:
    c.dest.addSubtree a.n
  # untyped propagates to the result type:
  typeofCallIs c, it, cs.beforeCall, c.types.untypedType
  takeParRi c, it.n

proc semConvArg(c: var SemContext; destType: Cursor; arg: Item; info: PackedLineInfo) =
  const
    IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, EnumT, HoleyEnumT}

  var srcType = skipModifier(arg.typ)

  # distinct type conversion?
  var isDistinct = false
  # also skips to type body for symbols:
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)

  if (destBase.typeKind in IntegralTypes and srcBase.typeKind in IntegralTypes) or
     (destBase.isSomeStringType and srcBase.isSomeStringType) or
     (destBase.containsGenericParams or srcBase.containsGenericParams):
    discard "ok"
    # XXX Add hderef here somehow
    c.dest.addSubtree arg.n
  elif isDistinct:
    var matchArg = Item(n: arg.n, typ: srcBase)
    var m = createMatch(addr c)
    typematch m, destBase, matchArg
    if m.err:
      c.typeMismatch info, arg.typ, destType
    else:
      # distinct type conversions can also involve conversions
      # between different integer sizes or object types and then
      # `m.args` contains these so use them here:
      c.dest.add m.args
  else:
    # maybe object types with an inheritance relation?
    var matchArg = arg
    var m = createMatch(addr c)
    typematch m, destType, matchArg
    if not m.err:
      c.dest.add m.args
    else:
      # also try the other direction:
      var m = createMatch(addr c)
      m.flipped = true
      matchArg.typ = destType
      typematch m, srcType, matchArg
      if not m.err:
        c.dest.add m.args
      else:
        c.typeMismatch info, arg.typ, destType

proc semLocalTypeExpr(c: var SemContext, it: var Item)

proc semConvFromCall(c: var SemContext; it: var Item; cs: CallState) =
  let beforeExpr = c.dest.len
  let info = cs.callNode.info
  var destType = cs.fn.typ
  if destType.typeKind == TypedescT: inc destType
  if destType.typeKind in {SinkT, LentT} and cs.args[0].typ.typeKind == TypedescT:
    var nullary = destType
    inc nullary
    if nullary.kind == ParRi:
      # sink T/lent T call
      var typeBuf = createTokenBuf(16)
      typeBuf.add destType
      typeBuf.addSubtree cs.args[0].n
      typeBuf.addParRi()
      var item = Item(n: beginRead(typeBuf), typ: it.typ)
      semLocalTypeExpr(c, item)
      takeParRi c, it.n
      it.typ = item.typ
      return
  c.dest.add parLeToken(ConvX, info)
  c.dest.copyTree destType
  semConvArg(c, destType, cs.args[0], info)
  takeParRi c, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, it, beforeExpr, expected

proc semObjConstr(c: var SemContext, it: var Item)

proc semObjConstrFromCall(c: var SemContext; it: var Item; cs: CallState) =
  skipParRi it.n
  var objBuf = createTokenBuf()
  objBuf.add parLeToken(OconstrX, cs.callNode.info)
  objBuf.addSubtree cs.fn.n
  objBuf.addParRi()
  var objConstr = Item(n: cursorAt(objBuf, 0), typ: it.typ)
  semObjConstr c, objConstr
  it.typ = objConstr.typ

proc isCastableType(t: TypeCursor): bool =
  const IntegralTypes = {FloatT, CharT, IntT, UIntT, BoolT, PointerT, CstringT, RefT, PtrT, RefobjT, PtrobjT, NiltT, EnumT, HoleyEnumT}
  result = t.typeKind in IntegralTypes or isEnumType(t)

proc semCast(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  let destType = semLocalType(c, it.n)
  var x = Item(n: it.n, typ: c.types.autoType)
  # XXX Add hderef here somehow
  semExpr c, x
  it.n = x.n
  takeParRi c, it.n

  var srcType = skipModifier(x.typ)

  # distinct type conversion?
  var isDistinct = false
  # also skips to type body for symbols:
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(srcType, isDistinct)
  if destBase.isCastableType and srcBase.isCastableType:
    commonType c, it, beforeExpr, destType
  elif containsGenericParams(srcType) or containsGenericParams(destType):
    commonType c, it, beforeExpr, destType
  else:
    c.dest.shrink beforeExpr
    c.buildErr info, "cannot `cast` between types " & typeToString(srcType) & " and " & typeToString(destType)

proc buildCallSource(buf: var TokenBuf; cs: CallState) =
  case cs.source
  of RegularCall:
    buf.add cs.callNode
    buf.addSubtree cs.fn.n
    for a in cs.args:
      buf.addSubtree a.n
  of MethodCall:
    assert cs.args.len >= 1
    buf.add cs.callNode
    buf.addParLe(DotX, cs.callNode.info)
    buf.addSubtree cs.args[0].n
    buf.addParRi()
    buf.addSubtree cs.fn.n
    for i in 1 ..< cs.args.len:
      buf.addSubtree cs.args[i].n
  of DotCall:
    assert cs.args.len == 1
    buf.addParLe(DotX, cs.callNode.info)
    buf.addSubtree cs.args[0].n
    buf.addSubtree cs.fn.n
  of SubscriptCall:
    buf.addParLe(AtX, cs.callNode.info)
    for a in cs.args:
      buf.addSubtree a.n
  of DotAsgnCall:
    assert cs.args.len == 2
    buf.addParLe(AsgnS, cs.callNode.info)
    buf.addParLe(DotX, cs.callNode.info)
    buf.addSubtree cs.args[0].n
    var callee = cs.fn.n
    let nameId = getIdent(callee)
    assert nameId != StrId(0)
    var name = pool.strings[nameId]
    assert name[^1] == '='
    name.setLen name.len - 1
    buf.add identToken(pool.strings.getOrIncl(name), cs.callNode.info)
    buf.addParRi()
    buf.addSubtree cs.args[1].n
  of SubscriptAsgnCall:
    buf.addParLe(AsgnS, cs.callNode.info)
    buf.addParLe(AtX, cs.callNode.info)
    let valueIndex = cs.args.len - 1
    for i in 0 ..< valueIndex:
      buf.addSubtree cs.args[i].n
    buf.addParRi()
    buf.addSubtree cs.args[valueIndex].n
  buf.addParRi()

proc semReturnType(c: var SemContext; n: var Cursor): TypeCursor =
  result = semLocalType(c, n, InReturnTypeDecl)

proc addArgsInstConverters(c: var SemContext; m: var Match; origArgs: openArray[Item]) =
  if not (m.genericConverter or m.genericEmpty):
    c.dest.add m.args
  else:
    m.args.addParRi()
    var arg = beginRead(m.args)
    var i = 0
    while arg.kind != ParRi:
      var nested = 0
      while arg.exprKind in {HconvX, OconvX, HderefX, HaddrX}:
        takeToken c, arg
        inc nested
      if m.genericEmpty and isEmptyLiteral(arg):
        takeToken c, arg
        inc nested
        if containsGenericParams(arg):
          let start = c.dest.len
          c.dest.addSubtree instantiateType(c, arg, m.inferred)
          skip arg
        else:
          takeTree c, arg
        # leave ParRi token to while loop below
      elif arg.exprKind == HcallX:
        let convInfo = arg.info
        takeToken c, arg
        inc nested
        if arg.kind == Symbol:
          let sym = arg.symId
          takeToken c, arg
          let res = tryLoadSym(sym)
          if res.status == LacksNothing and res.decl.symKind == ConverterY:
            let routine = asRoutine(res.decl)
            if isGeneric(routine):
              let conv = FnCandidate(kind: routine.kind, sym: sym, typ: routine.params)
              var convMatch = createMatch(addr c)
              sigmatch convMatch, conv, [Item(n: arg, typ: origArgs[i].typ)], emptyNode()
              # ^ could also use origArgs[i] directly but commonType would have to keep the expression alive
              assert not convMatch.err
              let inst = c.requestRoutineInstance(conv.sym, convMatch.typeArgs, convMatch.inferred, convInfo)
              c.dest[c.dest.len-1].setSymId inst.targetSym
      while true:
        case arg.kind
        of ParLe: inc nested
        of ParRi: dec nested
        else: discard
        takeToken c, arg
        if nested == 0: break
      inc i

proc tryConverterMatch(c: var SemContext; convMatch: var Match; f: TypeCursor, arg: Item): bool =
  ## looks for a converter from `arg` to `f`, returns `true` if found and
  ## sets `convMatch` to the match to the converter
  result = false
  let root = nominalRoot(f)
  if root == SymId(0) and not c.g.config.compat: return
  var converters = c.converters.getOrDefault(root)
  if root != SymId(0) and c.g.config.compat:
    converters.add c.converters.getOrDefault(SymId(0))
  var convMatches: seq[Match] = @[]
  for conv in items converters:
    # f(a)
    # --> f(conv(a)) ?
    # conv's return type must match `f`.
    # conv's input type must match `a`.
    let res = tryLoadSym(conv)
    assert res.status == LacksNothing
    var fn = asRoutine(res.decl)
    assert fn.kind == ConverterY

    var inputMatch = createMatch(addr c)
    let candidate = FnCandidate(kind: fn.kind, sym: conv, typ: fn.params)

    # first match the input argument of `conv` so that the unification algorithm works as expected:
    sigmatch(inputMatch, candidate, [arg], emptyNode())
    if classifyMatch(inputMatch) notin {EqualMatch, GenericMatch, SubtypeMatch}:
      continue
    # use inputMatch.returnType here so the caller doesn't have to instantiate it again:
    if inputMatch.inferred.len != 0 and containsGenericParams(inputMatch.returnType):
      inputMatch.returnType = instantiateType(c, inputMatch.returnType, inputMatch.inferred)
    let dest = inputMatch.returnType
    var callBuf = createTokenBuf(16) # dummy call node to use for matching dest type
    callBuf.add parLeToken(HcallX, arg.n.info)
    callBuf.add symToken(conv, arg.n.info)
    callBuf.add inputMatch.args
    callBuf.addParRi()
    var newArg = Item(n: beginRead(callBuf), typ: dest)
    var fMatch = f
    var destMatch = createMatch(addr c)
    typematch(destMatch, fMatch, newArg)
    if classifyMatch(destMatch) in {EqualMatch, GenericMatch}:
      if isGeneric(fn):
        inputMatch.genericConverter = true
      convMatches.add inputMatch
  let idx = pickBestMatch(c, convMatches)
  if idx >= 0:
    result = true
    convMatch = convMatches[idx]

proc resolveOverloads(c: var SemContext; it: var Item; cs: var CallState) =
  let genericArgs =
    if cs.hasGenericArgs: cursorAt(cs.genericDest, 0)
    else: emptyNode()

  var m: seq[Match] = @[]
  if cs.fn.n.exprKind in {OchoiceX, CchoiceX}:
    var f = cs.fn.n
    inc f
    while f.kind != ParRi:
      if f.kind == Symbol:
        let sym = f.symId
        let s = fetchSym(c, sym)
        let typ = fetchCallableType(c, f, s)
        if typ.typeKind == ParamsT:
          let candidate = FnCandidate(kind: s.kind, sym: sym, typ: typ)
          m.add createMatch(addr c)
          sigmatch(m[^1], candidate, cs.args, genericArgs)
      else:
        buildErr c, cs.fn.n.info, "`choice` node does not contain `symbol`"
      inc f
    considerTypeboundOps(c, m, cs.candidates, cs.args, genericArgs)
    if m.len == 0:
      # symchoice contained no callable symbols and no typebound ops
      assert cs.fnName != StrId(0)
      buildErr c, cs.fn.n.info, "attempt to call routine: '" & pool.strings[cs.fnName] & "'"
  elif cs.fn.n.kind == Ident:
    # error should have been given above already:
    # buildErr c, fn.n.info, "attempt to call undeclared routine"
    discard
  elif cs.fn.typ.typeKind == TypedescT and cs.args.len == 1:
    semConvFromCall c, it, cs
    return
  elif cs.fn.typ.typeKind == TypedescT and cs.args.len == 0:
    semObjConstrFromCall c, it, cs
    return
  else:
    # Keep in mind that proc vars are a thing:
    let sym = if cs.fn.n.kind == Symbol: cs.fn.n.symId else: SymId(0)
    let typ = skipProcTypeToParams(cs.fn.typ)
    if typ.typeKind == ParamsT:
      let candidate = FnCandidate(kind: cs.fnKind, sym: sym, typ: typ)
      m.add createMatch(addr c)
      sigmatch(m[^1], candidate, cs.args, genericArgs)
      considerTypeboundOps(c, m, cs.candidates, cs.args, genericArgs)
    elif sym != SymId(0):
      # non-callable symbol, look up all overloads
      assert cs.fnName != StrId(0)
      var choiceBuf = createTokenBuf(16)
      swap c.dest, choiceBuf
      discard buildSymChoice(c, cs.fnName, cs.fn.n.info, FindAll)
      swap c.dest, choiceBuf
      # could store choiceBuf in CallState but cs.fn should not outlive it
      cs.fn = Item(n: beginRead(choiceBuf), typ: c.types.autoType, kind: CchoiceY)
      resolveOverloads(c, it, cs)
      return
    else:
      buildErr c, cs.fn.n.info, "cannot call expression of type " & typeToString(typ)
  var idx = pickBestMatch(c, m)

  if idx < 0:
    # try converters
    var matchAdded = false
    let L = m.len
    for mi in 0 ..< L:
      if not m[mi].err: continue
      var newMatch = createMatch(addr c)
      var newArgs: seq[Item] = @[]
      var newArgBufs: seq[TokenBuf] = @[] # to keep alive
      var param = skipProcTypeToParams(m[mi].fn.typ)
      assert param == "params"
      inc param
      var ai = 0
      var anyConverters = false
      while param.kind != ParRi:
        # varargs not handled yet
        if ai >= cs.args.len: break
        let f = asLocal(param).typ
        var arg = cs.args[ai]
        var convMatch = default(Match)
        if tryConverterMatch(c, convMatch, f, arg):
          anyConverters = true
          var argBuf = createTokenBuf(16)
          argBuf.add parLeToken(HcallX, arg.n.info)
          argBuf.add symToken(convMatch.fn.sym, arg.n.info)
          if convMatch.genericConverter:
            # instantiate after match
            newMatch.genericConverter = true
          argBuf.add convMatch.args
          argBuf.addParRi()
          newArgs.add Item(n: beginRead(argBuf), typ: convMatch.returnType)
          newArgBufs.add argBuf
        else:
          newArgs.add arg
        skip param
        inc ai
      if anyConverters:
        sigmatch(newMatch, m[mi].fn, newArgs, genericArgs)
        m.add newMatch
        matchAdded = true
    if matchAdded: # m.len != L
      idx = pickBestMatch(c, m)

  if idx >= 0:
    c.dest.add cs.callNode
    let finalFn = m[idx].fn
    let isMagic = c.addFn(finalFn, cs.fn.n, cs.args)
    addArgsInstConverters(c, m[idx], cs.args)
    takeParRi c, it.n

    if finalFn.kind == TemplateY:
      typeofCallIs c, it, cs.beforeCall, m[idx].returnType
      if c.templateInstCounter <= MaxNestedTemplates:
        inc c.templateInstCounter
        withErrorContext c, cs.callNode.info:
          semTemplateCall c, it, finalFn.sym, cs.beforeCall, m[idx]
        dec c.templateInstCounter
      else:
        buildErr c, cs.callNode.info, "recursion limit exceeded for template expansions"
    elif isMagic == MagicCallNeedsSemcheck:
      # semcheck produced magic expression
      var magicExprBuf = createTokenBuf(c.dest.len - cs.beforeCall)
      magicExprBuf.addUnstructured cursorAt(c.dest, cs.beforeCall)
      endRead(c.dest)
      c.dest.shrink cs.beforeCall
      var magicExpr = Item(n: cursorAt(magicExprBuf, 0), typ: it.typ)
      semExpr c, magicExpr, cs.flags
      it.typ = magicExpr.typ
    elif m[idx].inferred.len > 0:
      var matched = m[idx]
      let returnType: Cursor
      if isMagic == NonMagicCall and c.routine.inGeneric == 0:
        let inst = c.requestRoutineInstance(finalFn.sym, matched.typeArgs, matched.inferred, cs.callNode.info)
        c.dest[cs.beforeCall+1].setSymId inst.targetSym
        var instReturnType = createTokenBuf(16)
        swap c.dest, instReturnType
        var subsReturnType = inst.returnType
        returnType = semReturnType(c, subsReturnType)
        swap c.dest, instReturnType
      else:
        if matched.returnType.kind == DotToken:
          returnType = matched.returnType
        else:
          returnType = instantiateType(c, matched.returnType, matched.inferred)
      typeofCallIs c, it, cs.beforeCall, returnType
    else:
      typeofCallIs c, it, cs.beforeCall, m[idx].returnType

  else:
    skipParRi it.n
    var errorMsg: string
    if idx == -2:
      errorMsg = "ambiguous call: '"
      if cs.fnName != StrId(0):
        errorMsg.add pool.strings[cs.fnName]
      errorMsg.add "'"
    elif cs.source in {DotCall, DotAsgnCall} and cs.fnName != StrId(0):
      errorMsg = "undeclared field: '"
      if cs.fnName != StrId(0):
        errorMsg.add pool.strings[cs.fnName]
      errorMsg.add "'"
      if cs.args.len != 0: # just to be safe
        errorMsg.add " for type "
        errorMsg.add typeToString(cs.args[0].typ)
    elif m.len > 0:
      errorMsg = "Type mismatch at [position]"
      for i in 0..<m.len:
        errorMsg.add "\n"
        addErrorMsg errorMsg, m[i]
    else:
      errorMsg = "undeclared identifier: '"
      if cs.fnName != StrId(0):
        errorMsg.add pool.strings[cs.fnName]
      errorMsg.add "'"
    var errored = createTokenBuf(4)
    buildCallSource errored, cs
    buildErr c, cs.callNode.info, errorMsg, cursorAt(errored, 0)

proc findMagicInSyms(syms: Cursor): ExprKind =
  var syms = syms
  result = NoExpr
  var nested = 0
  while true:
    case syms.kind
    of Symbol:
      let res = tryLoadSym(syms.symId)
      if res.status == LacksNothing:
        var n = res.decl
        inc n # skip the symbol kind
        if n.kind == SymbolDef:
          inc n # skip the SymbolDef
          if n.kind == ParLe:
            result = n.exprKind
            if result != NoExpr: break
    of ParLe:
      if syms.exprKind notin {OchoiceX, CchoiceX}: break
      inc nested
    of ParRi:
      dec nested
    else: break
    if nested == 0: break
    inc syms

proc unoverloadableMagicCall(c: var SemContext; it: var Item; cs: var CallState; magic: ExprKind) =
  let nifTag = parLeToken(magic, cs.callNode.info)
  if cs.args.len != 0:
    # keep args after if they were produced by dotcall:
    cs.dest.replace fromBuffer([nifTag]), 0
  else:
    cs.dest.shrink 0
    cs.dest.add nifTag
  while it.n.kind != ParRi:
    # add all args in call:
    takeTree cs.dest, it.n
  takeParRi cs.dest, it.n
  var magicCall = Item(n: beginRead(cs.dest), typ: it.typ)
  semExpr c, magicCall, cs.flags
  it.typ = magicCall.typ

proc semCall(c: var SemContext; it: var Item; flags: set[SemFlag]; source: TransformedCallSource = RegularCall) =
  var cs = CallState(
    beforeCall: c.dest.len,
    callNode: it.n.load(),
    dest: createTokenBuf(16),
    source: source,
    flags: {InTypeContext}*flags
  )
  inc it.n
  swap c.dest, cs.dest
  cs.fn = Item(n: it.n, typ: c.types.autoType)
  var argIndexes: seq[int] = @[]
  if cs.fn.n.exprKind == AtX:
    inc cs.fn.n # skip tag
    var lhsBuf = createTokenBuf(4)
    var lhs = Item(n: cs.fn.n, typ: c.types.autoType)
    swap c.dest, lhsBuf
    semExpr c, lhs, {KeepMagics, AllowUndeclared} # don't consider all overloads
    swap c.dest, lhsBuf
    cs.fn.n = lhs.n
    lhs.n = cursorAt(lhsBuf, 0)
    var maybeRoutine = lhs.n
    if maybeRoutine.exprKind in {OchoiceX, CchoiceX}:
      inc maybeRoutine
    if maybeRoutine.kind == Symbol:
      let res = tryLoadSym(maybeRoutine.symId)
      assert res.status == LacksNothing
      if isRoutine(res.decl.symKind) and isGeneric(asRoutine(res.decl)):
        cs.hasGenericArgs = true
        cs.genericDest = createTokenBuf(16)
        swap c.dest, cs.genericDest
        while cs.fn.n.kind != ParRi:
          semLocalTypeImpl c, cs.fn.n, AllowValues
        takeParRi c, cs.fn.n
        swap c.dest, cs.genericDest
        it.n = cs.fn.n
        c.dest.addSubtree lhs.n
        cs.fn.typ = lhs.typ
        cs.fn.kind = lhs.kind
        cs.fnName = getFnIdent(c)
    if not cs.hasGenericArgs:
      semBuiltinSubscript(c, cs.fn, lhs)
      cs.fnName = getFnIdent(c)
      it.n = cs.fn.n
  elif cs.fn.n.exprKind == DotX:
    let dotStart = c.dest.len
    let dotInfo = cs.fn.n.info
    # read through the dot expression first:
    inc cs.fn.n # skip tag
    var lhsBuf = createTokenBuf(4)
    var lhs = Item(n: cs.fn.n, typ: c.types.autoType)
    swap c.dest, lhsBuf
    semExpr c, lhs, {AllowModuleSym}
    swap c.dest, lhsBuf
    cs.fn.n = lhs.n
    lhs.n = cursorAt(lhsBuf, 0)
    let fieldNameCursor = cs.fn.n
    let fieldName = getIdent(cs.fn.n)
    # skip optional inheritance depth:
    if cs.fn.n.kind == IntLit:
      inc cs.fn.n
    skipParRi cs.fn.n
    it.n = cs.fn.n
    # now interpret the dot expression:
    let dotState = tryBuiltinDot(c, cs.fn, lhs, fieldName, dotInfo, {KeepMagics, AllowUndeclared, AllowOverloads})
    if dotState == FailedDot or
        # also ignore non-proc fields:
        (dotState == MatchedDotField and cs.fn.typ.typeKind != ProctypeT):
      cs.source = MethodCall
      # turn a.b(...) into b(a, ...)
      # first, delete the output of `tryBuiltinDot`:
      c.dest.shrink dotStart
      # sem b:
      cs.fn = Item(n: fieldNameCursor, typ: c.types.autoType)
      semExpr c, cs.fn, {KeepMagics, AllowUndeclared, AllowOverloads}
      cs.fnName = getFnIdent(c)
      # add a as argument:
      let lhsIndex = c.dest.len
      c.dest.addSubtree lhs.n
      argIndexes.add lhsIndex
      # scope extension: If the type is Typevar and it has attached
      # a concept, use the concepts symbols too:
      if cs.fnName != StrId(0):
        let root = nominalRoot(lhs.typ, allowTypevar = true)
        if root != SymId(0):
          maybeAddConceptMethods c, cs.fnName, root, cs.candidates
      # lhs.n escapes here, but is not read and will be set by argIndexes:
      cs.args.add lhs
  else:
    semExpr(c, cs.fn, {KeepMagics, AllowUndeclared, AllowOverloads})
    cs.fnName = getFnIdent(c)
    it.n = cs.fn.n
  if c.g.config.compat and cs.fnName in c.unoverloadableMagics:
    # transform call early before semchecking arguments
    let syms = beginRead(c.dest)
    let magic = findMagicInSyms(syms)
    endRead(c.dest)
    if magic != NoExpr:
      swap c.dest, cs.dest
      unoverloadableMagicCall(c, it, cs, magic)
      return
  cs.fnKind = cs.fn.kind
  var skipSemCheck = false
  while it.n.kind != ParRi:
    var arg = Item(n: it.n, typ: c.types.autoType)
    argIndexes.add c.dest.len
    semExpr c, arg, {AllowEmpty}
    if arg.typ.typeKind == UntypedT:
      skipSemCheck = true
    # scope extension: If the type is Typevar and it has attached
    # a concept, use the concepts symbols too:
    if cs.fnName != StrId(0):
      let root = nominalRoot(arg.typ, allowTypevar = true)
      if root != SymId(0):
        maybeAddConceptMethods c, cs.fnName, root, cs.candidates
    it.n = arg.n
    cs.args.add arg
  assert cs.args.len == argIndexes.len
  swap c.dest, cs.dest
  cs.fn.n = beginRead(cs.dest)
  for i in 0 ..< cs.args.len:
    cs.args[i].n = cursorAt(cs.dest, argIndexes[i])
  if skipSemCheck:
    untypedCall c, it, cs
  else:
    resolveOverloads c, it, cs

proc genericRootSym(td: TypeDecl): SymId =
  result = td.name.symId
  if td.typevars.typeKind == InvokeT:
    var root = td.typevars
    inc root
    assert root.kind == Symbol
    result = root.symId

proc findObjFieldAux(t: Cursor; name: StrId; level = 0): ObjField =
  assert t == "object"
  var n = t
  inc n # skip `(object` token
  var baseType = n
  skip n # skip basetype
  while n.kind == ParLe and n.substructureKind == FldU:
    inc n # skip FldU
    if n.kind == SymbolDef and sameIdent(n.symId, name):
      let symId = n.symId
      inc n # skip name
      let exported = n.kind != DotToken
      skip n # export marker
      skip n # pragmas
      return ObjField(sym: symId, level: level, typ: n, exported: exported, rootOwner: SymId(0))
    skip n # skip name
    skip n # export marker
    skip n # pragmas
    skip n # type
    skip n # value
    skipParRi n
  if baseType.kind == DotToken:
    result = ObjField(level: -1)
  else:
    if baseType.typeKind in {RefT, PtrT}:
      inc baseType
    if baseType.typeKind == InvokeT:
      inc baseType # get to root symbol
    if baseType.kind == Symbol:
      let decl = getTypeSection(baseType.symId)
      var objType = decl.body
      # emulate objtypeImpl
      if objType.typeKind in {RefT, PtrT}:
        inc objType
      result = findObjFieldAux(objType, name, level+1)
      if result.level == level+1:
        result.rootOwner = genericRootSym(decl)
    else:
      # maybe error
      result = ObjField(level: -1)

proc findObjFieldConsiderVis(c: var SemContext; decl: TypeDecl; name: StrId): ObjField =
  var impl = decl.body
  # emulate objtypeImpl
  if impl.typeKind in {RefT, PtrT}:
    inc impl
  result = findObjFieldAux(impl, name)
  if result.level == 0:
    result.rootOwner = genericRootSym(decl)
  if result.level >= 0:
    # check visibility
    var visible = false
    if result.exported:
      visible = true
    else:
      let owner = result.rootOwner
      if owner == SymId(0):
        visible = true
      else:
        let ownerModule = extractModule(pool.syms[owner])
        visible = ownerModule == "" or ownerModule == c.thisModuleSuffix
    if not visible:
      # treat as undeclared
      result = ObjField(level: -1)

proc findModuleSymbol(n: Cursor): SymId =
  result = SymId(0)
  if n.kind == Symbol:
    let res = tryLoadSym(n.symId)
    if res.status == LacksNothing and symKind(res.decl) == ModuleY:
      result = n.symId
  elif n.kind == ParLe and exprKind(n) in {OchoiceX, CchoiceX}:
    # if any sym in choice is module sym, count it as a module reference
    # this emulates behavior that was caused by sym order shenanigans before, could be removed
    var n = n
    inc n
    while n.kind != ParRi:
      result = findModuleSymbol(n)
      if result != SymId(0): break
      inc n

proc semQualifiedIdent(c: var SemContext; module: SymId; ident: StrId; info: PackedLineInfo): Sym =
  # mirrors semIdent
  let insertPos = c.dest.len
  # XXX does not handle the case where `module` is the current module
  let count = buildSymChoiceForForeignModule(c, c.importedModules[module], ident, info)
  if count == 1:
    let sym = c.dest[insertPos+1].symId
    c.dest.shrink insertPos
    c.dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semExprSym(c: var SemContext; it: var Item; s: Sym; start: int; flags: set[SemFlag])

proc tryBuiltinDot(c: var SemContext; it: var Item; lhs: Item; fieldName: StrId;
                   info: PackedLineInfo; flags: set[SemFlag]): DotExprState =
  let exprStart = c.dest.len
  let expected = it.typ
  c.dest.addParLe(DotX, info)
  c.dest.addSubtree lhs.n
  result = FailedDot
  if fieldName == StrId(0):
    # fatal error
    c.buildErr info, "identifier after `.` expected"
    result = InvalidDot
  else:
    let t = skipModifier(lhs.typ)
    var root = t
    var doDeref = false # maybe arbitrary number of derefs for compat mode
    if root.typeKind in {RefT, PtrT}:
      doDeref = true
      inc root
    if root.typeKind == InvokeT:
      inc root
    if root.kind == Symbol:
      let decl = getTypeSection(root.symId)
      if decl.kind == TypeY:
        var objType = decl.body
        # emulate objtypeImpl
        if objType.typeKind in {RefT, PtrT}:
          doDeref = true
          inc objType
        if objType.typeKind in {ObjectT, RefobjT, PtrobjT}:
          let field = findObjFieldConsiderVis(c, decl, fieldName)
          if field.level >= 0:
            if doDeref or objType.typeKind in {RefobjT, PtrobjT}:
              c.dest[exprStart] = parLeToken(DdotX, info)
            c.dest.add symToken(field.sym, info)
            c.dest.add intToken(pool.integers.getOrIncl(field.level), info)
            it.typ = field.typ # will be fit later with commonType
            it.kind = FldY
            result = MatchedDotField
          else:
            c.dest.add identToken(fieldName, info)
        else:
          # typevars reach here, maybe return untyped state
          # though probably better to fail if this is a call, i.e. `x.int`
          c.dest.add identToken(fieldName, info)
      else:
        c.dest.add identToken(fieldName, info)
    elif lhs.kind == ModuleY:
      # this is a qualified identifier, i.e. module.name
      # consider matched even if undeclared
      result = MatchedDotSym
      c.dest.shrink exprStart
      let module = findModuleSymbol(lhs.n)
      let s = semQualifiedIdent(c, module, fieldName, info)
      semExprSym c, it, s, exprStart, flags
      return
    elif t.typeKind == TupleT:
      var tup = t
      inc tup
      var i = 0
      while tup.kind != ParRi:
        let field = asLocal(tup)
        if field.name.kind == SymbolDef and sameIdent(field.name.symId, fieldName):
          c.dest[exprStart] = parLeToken(TupAtX, info)
          c.dest.addIntLit(i, info)
          it.typ = field.typ # will be fit later with commonType
          it.kind = FldY
          result = MatchedDotField
          break
        skip tup
        inc i
      if result != MatchedDotField:
        c.dest.add identToken(fieldName, info)
        c.dest.add intToken(pool.integers.getOrIncl(0), info)
    else:
      c.dest.add identToken(fieldName, info)
  c.dest.addParRi()
  if result == MatchedDotField:
    commonType c, it, exprStart, expected

proc semDot(c: var SemContext, it: var Item; flags: set[SemFlag]) =
  let exprStart = c.dest.len
  let info = it.n.info
  let expected = it.typ
  # read through the dot expression first:
  inc it.n # skip tag
  var lhsBuf = createTokenBuf(4)
  var lhs = Item(n: it.n, typ: c.types.autoType)
  swap c.dest, lhsBuf
  semExpr c, lhs, {AllowModuleSym}
  swap c.dest, lhsBuf
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)
  let fieldNameCursor = it.n
  let fieldName = getIdent(it.n)
  # skip optional inheritance depth:
  if it.n.kind == IntLit:
    inc it.n
  skipParRi it.n
  # now interpret the dot expression:
  let state = tryBuiltinDot(c, it, lhs, fieldName, info, flags)
  if state == FailedDot:
    # attempt a dot call, i.e. build b(a) from a.b
    c.dest.shrink exprStart
    var callBuf = createTokenBuf(16)
    callBuf.addParLe(CallX, info)
    callBuf.addSubtree fieldNameCursor
    callBuf.addSubtree lhs.n # add lhs as first argument
    callBuf.addParRi()
    var call = Item(n: cursorAt(callBuf, 0), typ: expected)
    semCall c, call, flags, DotCall
    it.typ = call.typ

proc semWhile(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  semBoolExpr c, it.n
  inc c.routine.inLoop
  withNewScope c:
    semStmt c, it.n, true
  dec c.routine.inLoop
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semBlock(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n

  inc c.routine.inBlock
  withNewScope c:
    if it.n.kind == DotToken:
      takeToken c, it.n
    else:
      let declStart = c.dest.len
      let delayed = handleSymDef(c, it.n, BlockY)
      c.addSym delayed
      publish c, delayed.s.name, declStart

    semStmt c, it.n, true
  dec c.routine.inBlock

  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semBreak(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.inLoop+c.routine.inBlock == 0:
    buildErr c, it.n.info, "`break` only possible within a `while` or `block` statement"
  else:
    if it.n.kind == DotToken:
      wantDot c, it.n
    else:
      var a = Item(n: it.n, typ: c.types.autoType)
      semExpr(c, a)
      if a.kind != BlockY:
        buildErr c, it.n.info, "`break` needs a block label"
      it.n = a.n
  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semContinue(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.inLoop == 0:
    buildErr c, it.n.info, "`continue` only possible within a loop"
  else:
    wantDot c, it.n
  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc wantExportMarker(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    c.dest.add n
    inc n
  elif n.kind == Ident and pool.strings[n.litId] == "x":
    if c.currentScope.kind != ToplevelScope:
      buildErr c, n.info, "only toplevel declarations can be exported"
    else:
      c.dest.add n
    inc n
  elif n.kind == ParLe:
    # export marker could have been turned into a NIF tag
    takeTree c, n
  else:
    buildErr c, n.info, "expected '.' or 'x' for an export marker"

proc insertType(c: var SemContext; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  c.dest.insert t, patchPosition

proc patchType(c: var SemContext; typ: TypeCursor; patchPosition: int) =
  let t = skipModifier(typ)
  c.dest.replace t, patchPosition

proc semProposition(c: var SemContext; n: var Cursor; kind: PragmaKind) =
  let prevPhase = c.phase
  if prevPhase != SemcheckBodies:
    takeTree c, n
  else:
    c.phase = SemcheckBodies
    withNewScope c:
      if kind == EnsuresP:
        c.dest.add parLeToken(ExprX, n.info)
        discard declareResult(c, n.info)
      #let start = c.dest.len
      semBoolExpr c, n
      if kind == EnsuresP:
        c.dest.addParRi()
      # XXX More checking here: Expression can only use parameters and `result`
      # and consts. Function calls are not allowed either. The grammar is:
      # atom ::= const | param | result
      # arith ::= atom | arith `+` arith | arith `-` arith | arith `*` arith | arith `/` arith # etc.
      # expr ::= arith | expr `and` expr | expr `or` expr | `not` expr
    c.phase = prevPhase

type
  CrucialPragma* = object
    sym: SymId
    magic, externName: string
    bits: int
    hasVarargs: PackedLineInfo
    flags: set[PragmaKind]

proc semPragma(c: var SemContext; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  let pk = pragmaKind(n)
  case pk
  of NoPragma:
    if kind.isRoutine and (let cc = callConvKind(n); cc != NoCallConv):
      c.dest.addParLe(cc, n.info)
      inc n
      c.dest.addParRi()
    else:
      buildErr c, n.info, "expected pragma"
      inc n
      c.dest.addParRi()
      #skip n
  of MagicP:
    c.dest.add parLeToken(MagicP, n.info)
    inc n
    if n.kind in {StringLit, Ident}:
      let m = parseMagic(pool.strings[n.litId])
      if m == mNone:
        buildErr c, n.info, "unknown `magic`"
      else:
        let (magicWord, bits) = magicToTag(m)
        crucial.magic = magicWord
        crucial.bits = bits
      takeToken c, n
    else:
      buildErr c, n.info, "`magic` pragma takes a string literal"
    c.dest.addParRi()
  of ImportcP, ImportcppP, ExportcP, HeaderP, PluginP:
    crucial.flags.incl pk
    let info = n.info
    c.dest.add parLeToken(pk, info)
    inc n
    let strPos = c.dest.len
    if n.kind != ParRi:
      semConstStrExpr c, n
    elif crucial.sym != SymId(0):
      var name = pool.syms[crucial.sym]
      extractBasename name
      c.dest.add strToken(pool.strings.getOrIncl(name), info)
    else:
      c.buildErr info, "invalid import/export symbol"
      c.dest.addParRi()
      return
    if pk in {ImportcP, ImportcppP, ExportcP} and c.dest[strPos].kind == StringLit:
      crucial.externName = pool.strings[c.dest[strPos].litId]
    # Header pragma extra
    if pk == HeaderP:
      let idx = c.dest.len - 1
      let tok = c.dest[idx]
      var name = replaceSubs(pool.strings[tok.litId], info.getFile(), c.g.config)
      name = name.toRelativePath(c.g.config.nifcachePath)
      c.dest[idx] = strToken(pool.strings.getOrIncl(name), tok.info)
    # Finalize expression
    c.dest.addParRi()
  of AlignP, BitsP:
    c.dest.add parLeToken(pk, n.info)
    inc n
    semConstIntExpr(c, n)
    c.dest.addParRi()
  of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoreturnP, BorrowP,
     NoSideEffectP, NodestroyP, BycopyP, ByrefP, InlineP, NoinlineP, NoinitP:
    crucial.flags.incl pk
    c.dest.add parLeToken(pk, n.info)
    c.dest.addParRi()
    inc n
  of VarargsP:
    crucial.hasVarargs = n.info
    c.dest.add parLeToken(pk, n.info)
    c.dest.addParRi()
    inc n
  of RequiresP, EnsuresP:
    crucial.flags.incl pk
    c.dest.add parLeToken(pk, n.info)
    inc n
    if n.kind != ParRi:
      semProposition c, n, pk
    else:
      buildErr c, n.info, "`requires`/`ensures` pragma takes a bool expression"
    c.dest.addParRi()
  of EmitP, BuildP, StringP, RaisesP:
    buildErr c, n.info, "pragma not supported"

proc semPragmas(c: var SemContext; n: var Cursor; crucial: var CrucialPragma; kind: SymKind) =
  if n.kind == DotToken:
    takeToken c, n
  elif n.substructureKind == PragmasU:
    takeToken c, n
    while n.kind != ParRi:
      let hasParRi = n.kind == ParLe
      if n.substructureKind == KvU:
        inc n
      semPragma c, n, crucial, kind
      if hasParRi:
        skipParRi n
    takeParRi c, n
  else:
    buildErr c, n.info, "expected '.' or 'pragmas'"

proc semIdentImpl(c: var SemContext; n: var Cursor; ident: StrId; flags: set[SemFlag]): Sym =
  let mode =
    if AllowOverloads in flags: FindOverloads
    else: InnerMost
  let insertPos = c.dest.len
  let info = n.info
  let count = buildSymChoice(c, ident, info, mode)
  if count == 1:
    let sym = c.dest[insertPos+1].symId
    c.dest.shrink insertPos
    c.dest.add symToken(sym, info)
    result = fetchSym(c, sym)
  else:
    result = Sym(kind: if count == 0: NoSym else: CchoiceY)

proc semIdent(c: var SemContext; n: var Cursor; flags: set[SemFlag]): Sym =
  result = semIdentImpl(c, n, n.litId, flags)
  inc n

proc semQuoted(c: var SemContext; n: var Cursor; flags: set[SemFlag]): Sym =
  let nameId = unquote(n)
  result = semIdentImpl(c, n, nameId, flags)

proc maybeInlineMagic(c: var SemContext; res: LoadResult) =
  if res.status == LacksNothing:
    var n = res.decl
    inc n # skip the symbol kind
    if n.kind == SymbolDef:
      inc n # skip the SymbolDef
      if n.kind == ParLe:
        # ^ export marker position has a `(`? If so, it is a magic!
        let info = c.dest[c.dest.len-1].info
        c.dest[c.dest.len-1] = withLineInfo(n.load, info)
        inc n
        while true:
          c.dest.add withLineInfo(n.load, info)
          if n.kind == ParRi: break
          inc n

proc exprToType(c: var SemContext; exprType: Cursor; start: int; context: TypeDeclContext; info: PackedLineInfo) =
  case exprType.typeKind
  of TypedescT:
    c.dest.shrink start
    var base = exprType
    inc base
    c.dest.addSubtree base
  of ErrT, AutoT:
    # propagate error
    discard
  else:
    # otherwise, is a static value
    if context != AllowValues:
      c.buildErr info, "not a type"

proc semTypeExpr(c: var SemContext; n: var Cursor; context: TypeDeclContext; info: PackedLineInfo) =
  # expression needs to be fully evaluated, switch to body phase
  var phase = SemcheckBodies
  swap c.phase, phase
  let start = c.dest.len
  var it = Item(n: n, typ: c.types.autoType)
  semExpr c, it
  n = it.n
  exprToType c, it.typ, start, context, info
  swap c.phase, phase

proc semTypeSym(c: var SemContext; s: Sym; info: PackedLineInfo; start: int; context: TypeDeclContext) =
  if s.kind in {TypeY, TypevarY}:
    let res = tryLoadSym(s.name)
    let beforeMagic = c.dest.len
    maybeInlineMagic c, res
    let afterMagic = c.dest.len
    if s.kind == TypevarY:
      # likely was not magic
      # maybe substitution performed here?
      inc c.usedTypevars
    elif beforeMagic != afterMagic:
      # was magic symbol, may be typeclass, otherwise nothing to do
      if context == InGenericConstraint:
        let magic = cursorAt(c.dest, start).typeKind
        endRead(c.dest)
        # magic types that are just symbols and not in the syntax:
        if magic in {ArrayT, SetT, RangetypeT}:
          var typeclassBuf = createTokenBuf(4)
          typeclassBuf.addParLe(TypeKindT, info)
          typeclassBuf.addParLe(magic, info)
          typeclassBuf.addParRi()
          typeclassBuf.addParRi()
          replace(c.dest, cursorAt(typeclassBuf, 0), start)
    elif res.status == LacksNothing:
      let typ = asTypeDecl(res.decl)
      if isGeneric(typ) or isNominal(typ.body.typeKind):
        # types that should stay as symbols, see sigmatch.matchSymbol
        discard
      else:
        # remove symbol, inline type:
        c.dest.shrink c.dest.len-1
        var t = typ.body
        semLocalTypeImpl c, t, context
  else:
    # non type symbol, treat as expression
    # mirror semTypeExpr but just call semExprSym
    var phase = SemcheckBodies
    swap c.phase, phase
    var dummyBuf = createTokenBuf(1)
    dummyBuf.add dotToken(info)
    var it = Item(n: cursorAt(dummyBuf, 0), typ: c.types.autoType)
    semExprSym c, it, s, start, {}
    exprToType c, it.typ, start, context, info
    swap c.phase, phase

proc semParams(c: var SemContext; n: var Cursor)
proc semLocal(c: var SemContext; n: var Cursor; kind: SymKind)

proc semObjectType(c: var SemContext; n: var Cursor) =
  takeToken c, n
  # inherits from?
  if n.kind == DotToken:
    takeToken c, n
  else:
    semLocalTypeImpl c, n, InLocalDecl
  if n.kind == DotToken:
    takeToken c, n
  else:
    # object fields:
    let oldScopeKind = c.currentScope.kind
    withNewScope c:
      # copy toplevel scope status for exported fields
      c.currentScope.kind = oldScopeKind
      while n.substructureKind == FldU:
        semLocal(c, n, FldY)
  takeParRi c, n

proc semTupleFieldType(c: var SemContext; elemType: var Cursor; i: int) =
  c.dest.add parLeToken(FldU, elemType.info) # start field
  c.dest.add identToken(pool.strings.getOrIncl("Field" & $i), elemType.info)
  c.dest.addDotToken() # export marker
  c.dest.addDotToken() # pragmas
  semLocalTypeImpl c, elemType, InLocalDecl
  c.dest.addDotToken() # value
  c.dest.addParRi() # end field

proc semTupleType(c: var SemContext; n: var Cursor) =
  c.dest.add parLeToken(TupleT, n.info)
  inc n
  # tuple fields:
  withNewScope c:
    var i = 0
    while n.kind != ParRi:
      if n.substructureKind == FldU:
        semLocal(c, n, FldY)
      else:
        semTupleFieldType(c, n, i)
        inc i
  takeParRi c, n

type
  EnumTypeState = object
    isBoolType: bool # `bool` is a magic enum and needs special handling
    enumType: SymId
    thisValue: xint
    hasHole: bool

proc semEnumField(c: var SemContext; n: var Cursor; state: var EnumTypeState)

proc semEnumType(c: var SemContext; n: var Cursor; enumType: SymId; beforeExportMarker: int) =
  let start = c.dest.len
  takeToken c, n
  let baseTypeStart = c.dest.len
  if n.kind == DotToken:
    wantDot c, n
  else:
    takeTree c, n
  let magicToken = c.dest[beforeExportMarker]
  var state = EnumTypeState(enumType: enumType, thisValue: createXint(0'i64), hasHole: false,
    isBoolType: magicToken.kind == ParLe and pool.tags[magicToken.tagId] == $BoolT)
  var signed = false
  var lastValue = state.thisValue
  while n.substructureKind == EfldU:
    semEnumField(c, n, state)
    if state.thisValue.isNegative:
      signed = true
    lastValue = state.thisValue
    inc state.thisValue
  if state.hasHole:
    c.dest[start] = parLeToken(HoleyEnumT, c.dest[start].info)
  var baseType: Cursor
  if signed:
    baseType = c.types.int32Type
  else:
    var err = false
    let max = asUnsigned(lastValue, err)
    # according to old size align computation:
    if max <= high(uint8).uint64:
      baseType = c.types.uint8Type
    elif max <= high(uint16).uint64:
      baseType = c.types.uint16Type
    elif max <= high(uint32).uint64:
      baseType = c.types.int32Type # according to old codegen
    else:
      baseType = c.types.int64Type # according to old codegen
  c.dest.replace baseType, baseTypeStart
  takeParRi c, n

proc declareConceptSelf(c: var SemContext; info: PackedLineInfo) =
  let name = pool.strings.getOrIncl("Self")
  let result = identToSym(c, name, TypevarY)
  let s = Sym(kind: TypevarY, name: result,
              pos: c.dest.len)
  discard c.currentScope.addNonOverloadable(name, s)
  let declStart = c.dest.len
  buildTree c.dest, TypevarY, info:
    c.dest.add symdefToken(result, info) # name
    c.dest.addDotToken() # export marker
    c.dest.addDotToken() # pragmas
    c.dest.addDotToken() # typ
    c.dest.addDotToken() # value
  publish c, result, declStart

proc semConceptType(c: var SemContext; n: var Cursor) =
  takeToken c, n
  wantDot c, n
  wantDot c, n
  declareConceptSelf c, n.info
  skip n # skip dot or previous `Self` declaration
  if n != "stmts":
    error "(stmts) expected, but got: ", n
  takeToken c, n
  withNewScope c:
    while true:
      let k = n.symKind
      if k in RoutineKinds:
        var it = Item(n: n, typ: c.types.voidType)
        semProc(c, it, k, checkConceptProc)
        n = it.n
      else:
        break
  takeParRi c, n
  takeParRi c, n

proc subsGenericTypeFromArgs(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo;
                             origin, targetSym: SymId; decl: TypeDecl; args: Cursor) =
  #[
  What we need to do is rather simple: A generic instantiation is
  the typical (type :Name ex generic_params pragmas body) tuple but
  this time the generic_params list the used `Invoke` construct for the
  instantiation.
  ]#
  var inferred = initTable[SymId, Cursor]()
  var err = 0
  dest.buildTree TypeS, info:
    dest.add symdefToken(targetSym, info)
    dest.addDotToken() # export
    dest.buildTree InvokeT, info:
      dest.add symToken(origin, info)
      var a = args
      var typevars = decl.typevars
      inc typevars
      while a.kind != ParRi and typevars.kind != ParRi:
        var tv = typevars
        assert tv == "typevar"
        inc tv
        assert tv.kind == SymbolDef
        inferred[tv.symId] = a
        takeTree dest, a
        skip typevars
      if a.kind != ParRi:
        err = -1
      elif typevars.kind != ParRi:
        err = 1
    # take the pragmas from the origin:
    dest.copyTree decl.pragmas
    if err == 0:
      var sc = SubsContext(params: addr inferred)
      subs(c, dest, sc, decl.body)
      addFreshSyms(c, sc)
    elif err == 1:
      dest.buildLocalErr info, "too few generic arguments provided"
    else:
      dest.buildLocalErr info, "too many generic arguments provided"

proc isRangeExpr(n: Cursor): bool =
  var n = n
  if n.exprKind notin {CallX, InfixX}:
    return false
  inc n
  let name = getIdent(n)
  result = name != StrId(0) and pool.strings[name] == ".."

proc addRangeValues(c: var SemContext; n: var Cursor) =
  var err: bool = false
  let first = asSigned(evalOrdinal(c, n), err)
  if err:
    c.buildErr n.info, "could not evaluate as ordinal", n
    err = false
  else:
    c.dest.addIntLit(first, n.info)
  skip n
  let last = asSigned(evalOrdinal(c, n), err)
  if err:
    c.buildErr n.info, "could not evaluate as ordinal", n
    err = false
  else:
    c.dest.addIntLit(last, n.info)

proc semRangeTypeFromExpr(c: var SemContext; n: var Cursor; info: PackedLineInfo) =
  inc n # call tag
  skip n # `..`
  c.dest.addParLe(RangetypeT, info)
  var it = Item(n: n, typ: c.types.autoType)
  var valuesBuf = createTokenBuf(4)
  swap c.dest, valuesBuf
  semExpr c, it
  semExpr c, it
  swap c.dest, valuesBuf
  n = it.n
  # insert base type:
  c.dest.addSubtree it.typ
  var values = cursorAt(valuesBuf, 0)
  addRangeValues c, values
  takeParRi c, n

const InvocableTypeMagics = {ArrayT, RangetypeT, VarargsT,
  PtrT, RefT, UncheckedArrayT, SetT, StaticT, TypedescT,
  SinkT, LentT}

proc semMagicInvoke(c: var SemContext; n: var Cursor; kind: TypeKind; info: PackedLineInfo) =
  # `n` is at first arg
  var typeBuf = createTokenBuf(16)
  typeBuf.addParLe(kind, info)
  # reorder invocation according to type specifications:
  case kind
  of ArrayT:
    # invoked as array[len, elem], but needs to become (array elem len)
    let indexPart = n
    skip n
    takeTree typeBuf, n # element type
    typeBuf.addSubtree indexPart
    takeParRi typeBuf, n
  of RangetypeT:
    # range types are invoked as `range[a..b]`
    if isRangeExpr(n):
      # don't bother calling semLocalTypeImpl, fully build type here
      semRangeTypeFromExpr c, n, info
      skipParRi n
    else:
      c.buildErr info, "expected `a..b` expression for range type"
      skipToEnd n
    return
  of PtrT, RefT, UncheckedArrayT, SetT, StaticT, TypedescT, SinkT, LentT:
    # unary invocations
    takeTree typeBuf, n
    takeParRi typeBuf, n
  of VarargsT:
    takeTree typeBuf, n
    if n.kind != ParRi:
      # optional varargs call
      takeTree typeBuf, n
    takeParRi typeBuf, n
  else:
    raiseAssert "unreachable" # see type kind check for magicKind
  var m = cursorAt(typeBuf, 0)
  semLocalTypeImpl c, m, InLocalDecl

proc semInvoke(c: var SemContext; n: var Cursor) =
  let typeStart = c.dest.len
  let info = n.info
  takeToken c, n # copy `at`
  semLocalTypeImpl c, n, InLocalDecl

  var headId: SymId = SymId(0)
  var decl = default TypeDecl
  var ok = false
  if c.dest[typeStart+1].kind == Symbol:
    headId = c.dest[typeStart+1].symId
    decl = getTypeSection(headId)
    if decl.kind != TypeY:
      c.buildErr info, "cannot attempt to instantiate a non-type"
    if decl.typevars != "typevars":
      c.buildErr info, "cannot attempt to instantiate a concrete type"
    else:
      ok = true
  else:
    # symbol may have inlined into a magic
    let head = cursorAt(c.dest, typeStart+1)
    let kind = head.typeKind
    endRead(c.dest)
    if kind in InvocableTypeMagics:
      # magics that can be invoked
      c.dest.shrink typeStart
      semMagicInvoke(c, n, kind, info)
      return
    else:
      c.buildErr info, "cannot attempt to instantiate a non-type"

  var genericArgs = 0
  swap c.usedTypevars, genericArgs
  let beforeArgs = c.dest.len
  while n.kind != ParRi:
    semLocalTypeImpl c, n, AllowValues
  swap c.usedTypevars, genericArgs
  takeParRi c, n
  if ok and (genericArgs == 0 or
      # structural types are inlined even with generic arguments
      # XXX does not instantiate properly if structural type is forward declared
      # because typevar syms are not created in the SemcheckTopLevelSyms phase
      # maybe  they could be declared and captured in the body with an untyped prepass
      not isNominal(decl.body.typeKind)):
    # we have to be eager in generic type instantiations so that type-checking
    # can do its job properly:
    let key = typeToCanon(c.dest, typeStart)
    var sym = Sym(kind: TypeY, name: SymId(0), pos: InvalidPos) # pos unused by semTypeSym
    if c.instantiatedTypes.hasKey(key):
      let cachedSym = c.instantiatedTypes[key]
      c.dest.shrink typeStart
      c.dest.add symToken(cachedSym, info)
      sym.name = cachedSym
    else:
      var args = cursorAt(c.dest, beforeArgs)
      let targetSym = newSymId(c, headId)
      if genericArgs == 0:
        c.instantiatedTypes[key] = targetSym
      var sub = createTokenBuf(30)
      subsGenericTypeFromArgs c, sub, info, headId, targetSym, decl, args
      c.dest.endRead()
      var phase = SemcheckTopLevelSyms
      var topLevel = createTokenBuf(30)
      swap c.phase, phase
      swap c.dest, topLevel
      var tn = beginRead(sub)
      semTypeSection c, tn
      swap c.dest, topLevel
      c.phase = SemcheckSignatures
      var instance = createTokenBuf(30)
      swap c.dest, instance
      tn = beginRead(topLevel)
      semTypeSection c, tn
      swap c.dest, instance
      swap c.phase, phase
      publish targetSym, ensureMove instance
      c.dest.shrink typeStart
      c.dest.add symToken(targetSym, info)
      sym.name = targetSym
    assert sym.name != SymId(0)
    semTypeSym c, sym, info, typeStart, InLocalDecl

proc addVarargsParameter(c: var SemContext; paramsAt: int; info: PackedLineInfo) =
  const vanon = "vanon"
  var varargsParam = @[
    parLeToken(ParamY, info),
    identToken(pool.strings.getOrIncl(vanon), info),
    dotToken(info), # export marker
    dotToken(info), # pragmas
    parLeToken(VarargsT, info),
    parRiToken(info),
    dotToken(info), # value
    parRiToken(info)
  ]
  if c.dest[paramsAt].kind == DotToken:
    c.dest[paramsAt] = parLeToken(ParamsT, info)
    varargsParam.add parRiToken(info)
    c.dest.insert fromBuffer(varargsParam), paramsAt+1
  else:
    var n = cursorAt(c.dest, paramsAt)
    if n.typeKind == ParamsT:
      inc n
      while n.kind != ParRi:
        if n.symKind == ParamY:
          inc n
          let lit = getIdent(n)
          if lit != StrId(0) and pool.strings[lit] == vanon:
            # already added:
            endRead(c.dest)
            return
          skipToEnd n
        else:
          break
      let insertPos = cursorToPosition(c.dest, n)
      endRead(c.dest)
      c.dest.insert fromBuffer(varargsParam), insertPos

proc semArrayType(c: var SemContext; n: var Cursor; context: TypeDeclContext) =
  let info = n.info
  takeToken c, n
  semLocalTypeImpl c, n, InLocalDecl
  # index type, possibilities are:
  # 1. length as integer
  # 2. range expression i.e. `a..b`
  # 3. full ordinal type i.e. `uint8`, `Enum`, `range[a..b]`
  # 4. standalone unresolved expression/type variable, could resolve to 1 or 3
  if isRangeExpr(n):
    semRangeTypeFromExpr c, n, info
  else:
    var indexBuf = createTokenBuf(4)
    swap c.dest, indexBuf
    semLocalTypeImpl c, n, AllowValues
    swap c.dest, indexBuf
    var index = cursorAt(indexBuf, 0)
    if index.typeKind == RangetypeT:
      # direct range type
      c.dest.addSubtree index
    elif isOrdinalType(index):
      # ordinal type, turn it into a range type
      c.dest.addParLe(RangetypeT, index.info)
      c.dest.addSubtree index # base type
      var err = false
      let first = asSigned(firstOrd(c, index), err)
      if err:
        c.buildErr index.info, "could not get first index of ordinal type: " & typeToString(index)
      else:
        c.dest.addIntLit(first, index.info)
      err = false
      let last = asSigned(lastOrd(c, index), err)
      if err:
        c.buildErr index.info, "could not get last index of ordinal type: " & typeToString(index)
      else:
        c.dest.addIntLit(last, index.info)
      c.dest.addParRi()
    elif containsGenericParams(index):
      # unresolved types are left alone
      c.dest.addSubtree index
    elif index.typeKind != NoType:
      c.buildErr index.info, "invalid array index type: " & typeToString(index)
    else:
      # length expression
      var err = false
      let length = asSigned(evalOrdinal(c, index), err)
      if err:
        c.buildErr index.info, "invalid array index type: " & typeToString(index)
      else:
        c.dest.addParLe(RangetypeT, info)
        c.dest.addSubtree c.types.intType
        c.dest.addIntLit 0, info
        c.dest.addIntLit length - 1, info
        c.dest.addParRi()
  takeParRi c, n

proc semRangeType(c: var SemContext; n: var Cursor; context: TypeDeclContext) =
  takeToken c, n
  semLocalTypeImpl c, n, InLocalDecl
  var valuesBuf = createTokenBuf(4)
  swap c.dest, valuesBuf
  semLocalTypeImpl c, n, AllowValues
  semLocalTypeImpl c, n, AllowValues
  swap c.dest, valuesBuf
  var values = cursorAt(valuesBuf, 0)
  addRangeValues c, values
  takeParRi c, n

proc tryTypeClass(c: var SemContext; n: var Cursor): bool =
  # if the type tree has no children, interpret it as a type kind typeclass
  var op = n
  inc op
  if op.kind == ParRi:
    c.dest.addParLe(TypeKindT, n.info)
    takeTree c, n
    c.dest.addParRi()
    result = true
  else:
    result = false

proc isOrExpr(n: Cursor): bool =
  # old nim special cases `|` infixes in type contexts
  result = n.exprKind == InfixX
  if result:
    var n = n
    inc n
    let name = getIdent(n)
    result = name != StrId(0) and pool.strings[name] == "|"

proc semTypeof(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  inc it.n # typeof
  semExpr c, it
  var t = it.typ
  if t.typeKind == TypedescT: inc t
  c.dest.shrink beforeExpr
  c.dest.addParLe(TypedescT, t.info)
  c.dest.addSubtree t
  c.dest.addParRi()
  it.typ = typeToCursor(c, beforeExpr)
  #echo "CAME HERE! ", typeToString(t), " ", c.phase
  #writeStackTrace()
  skipParRi it.n

proc semLocalTypeImpl(c: var SemContext; n: var Cursor; context: TypeDeclContext) =
  let info = n.info
  case n.kind
  of Ident:
    let start = c.dest.len
    let s = semIdent(c, n, {})
    semTypeSym c, s, info, start, context
  of Symbol:
    let start = c.dest.len
    let s = fetchSym(c, n.symId)
    c.dest.add n
    inc n
    semTypeSym c, s, info, start, context
  of ParLe:
    case typeKind(n)
    of NoType:
      let xkind = exprKind(n)
      if xkind == QuotedX:
        let start = c.dest.len
        let s = semQuoted(c, n, {})
        semTypeSym c, s, info, start, context
      elif xkind == ParX:
        inc n
        semLocalTypeImpl c, n, context
        skipParRi n
      elif xkind == TupleConstrX:
        semTupleType c, n
      elif isOrExpr(n):
        c.dest.addParLe(OrT, info)
        inc n # tag
        inc n # `|`
        var nested = 1
        while nested != 0:
          if isOrExpr(n):
            inc n # tag
            inc n # `|`
            inc nested
          elif n.kind == ParRi:
            inc n
            dec nested
          else:
            semLocalTypeImpl c, n, context
        c.dest.addParRi()
      elif false and isRangeExpr(n):
        # a..b, interpret as range type but only without AllowValues
        # to prevent conflict with HSlice
        # disabled for now, array types special case range expressions
        semRangeTypeFromExpr c, n, info
      else:
        semTypeExpr c, n, context, info
    of IntT, FloatT, CharT, BoolT, UIntT, VoidT, NiltT, AutoT,
        SymKindT, UntypedT, TypedT, CstringT, PointerT, TypeKindT, OrdinalT:
      takeTree c, n
    of PtrT, RefT, MutT, OutT, LentT, SinkT, NotT, UncheckedArrayT,
       StaticT, TypedescT, OpenArrayT:
      if tryTypeClass(c, n):
        return
      takeToken c, n
      semLocalTypeImpl c, n, InLocalDecl
      takeParRi c, n
    of SetT:
      if tryTypeClass(c, n):
        return
      takeToken c, n
      let elemTypeStart = c.dest.len
      semLocalTypeImpl c, n, InLocalDecl
      takeParRi c, n
      let elemType = cursorAt(c.dest, elemTypeStart)
      if containsGenericParams(elemType):
        # allow
        c.dest.endRead()
      elif not isOrdinalType(elemType, allowEnumWithHoles = true):
        c.dest.endRead()
        c.buildErr info, "set element type must be ordinal"
      else:
        let length = lengthOrd(c, elemType)
        c.dest.endRead()
        if length.isNaN or length > MaxSetElements:
          c.buildErr info, "type " & typeToString(elemType) & " is too large to be a set element type"
    of OrT, AndT:
      takeToken c, n
      while n.kind != ParRi:
        semLocalTypeImpl c, n, context
      takeParRi c, n
    of TupleT:
      if tryTypeClass(c, n):
        return
      semTupleType c, n
    of ArrayT:
      if tryTypeClass(c, n):
        return
      semArrayType c, n, context
    of RangetypeT:
      if tryTypeClass(c, n):
        return
      semRangeType c, n, context
    of VarargsT:
      takeToken c, n
      if n.kind != ParRi:
        semLocalTypeImpl c, n, InLocalDecl
        if n.kind == DotToken:
          takeToken c, n
        else:
          var it = Item(n: n, typ: c.types.autoType)
          semExpr c, it
          # XXX Check the expression is a symchoice or a sym
          n = it.n
      takeParRi c, n
    of ObjectT, RefobjT, PtrobjT:
      if tryTypeClass(c, n):
        discard
      elif context != InTypeSection:
        c.buildErr info, "`object` type must be defined in a `type` section"
        skip n
      else:
        semObjectType c, n
    of EnumT, HoleyEnumT:
      if tryTypeClass(c, n):
        discard
      else:
        c.buildErr info, "`enum` type must be defined in a `type` section"
        skip n
    of ConceptT:
      if context != InTypeSection:
        c.buildErr info, "`concept` type must be defined in a `type` section"
        skip n
      else:
        semConceptType c, n
    of DistinctT:
      if context != InTypeSection:
        c.buildErr info, "`distinct` type must be defined in a `type` section"
        skip n
      else:
        takeToken c, n
        semLocalTypeImpl c, n, InLocalDecl
        takeParRi c, n
    of ProctypeT, IteratorT, ParamsT:
      if tryTypeClass(c, n):
        return
      if typeKind(n) != ParamsT:
        takeToken c, n
        wantDot c, n # name
        wantDot c, n # export marker
        wantDot c, n # pattern
        wantDot c, n # generics
      else:
        takeToken c, n
      let beforeParams = c.dest.len
      c.openScope()
      semParams c, n
      semLocalTypeImpl c, n, InReturnTypeDecl
      var crucial = default CrucialPragma
      semPragmas c, n, crucial, ProcY
      wantDot c, n # exceptions
      wantDot c, n # body
      # close it here so that pragmas like `requires` can refer to the params:
      c.closeScope()
      takeParRi c, n
      if crucial.hasVarargs.isValid:
        addVarargsParameter c, beforeParams, crucial.hasVarargs
    of InvokeT:
      semInvoke c, n
    of ErrT:
      takeTree c, n
    of ItertypeT:
      c.buildErr info, "itertype not supported"
      skip n
  of DotToken:
    if context in {InReturnTypeDecl, InGenericConstraint}:
      takeToken c, n
    else:
      c.buildErr info, "not a type", n
      inc n
  else:
    semTypeExpr c, n, context, info

proc exportMarkerBecomesNifTag(c: var SemContext; insertPos: int; crucial: CrucialPragma) =
  assert crucial.magic.len > 0
  let info = c.dest[insertPos].info

  if crucial.bits != 0:
    let nifTag = [
      parLeToken(pool.tags.getOrIncl(crucial.magic), info),
      intToken(pool.integers.getOrIncl(crucial.bits), info),
      parRiToken(info)
    ]
    c.dest.replace fromBuffer(nifTag), insertPos
  else:
    let nifTag = [
      parLeToken(pool.tags.getOrIncl(crucial.magic), info),
      parRiToken(info)
    ]
    c.dest.replace fromBuffer(nifTag), insertPos

proc semLocalValue(c: var SemContext; it: var Item; crucial: CrucialPragma) =
  if ThreadvarP in crucial.flags:
    c.buildErr it.n.info, "a `threadvar` cannot have an init value"
    skip it.n
  else:
    semExpr c, it

proc semLocal(c: var SemContext; n: var Cursor; kind: SymKind) =
  let declStart = c.dest.len
  takeToken c, n
  let delayed = handleSymDef(c, n, kind) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1
  var crucial = CrucialPragma(sym: delayed.s.name)
  semPragmas c, n, crucial, kind # 2
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial
  case kind
  of TypevarY:
    discard semLocalType(c, n, InGenericConstraint)
    wantDot c, n
  of ParamY, LetY, VarY, ConstY, CursorY, ResultY, FldY:
    let beforeType = c.dest.len
    if n.kind == DotToken:
      # no explicit type given:
      inc n # 3
      var it = Item(n: n, typ: c.types.autoType)
      if false and kind == ConstY:
        withNewScope c:
          semConstExpr c, it # 4
      else:
        semLocalValue c, it, crucial # 4
      n = it.n
      insertType c, it.typ, beforeType
    else:
      let typ = semLocalType(c, n) # 3
      if n.kind == DotToken:
        # empty value
        takeToken c, n
      else:
        var it = Item(n: n, typ: typ)
        if false and kind == ConstY:
          withNewScope c:
            semConstExpr c, it # 4
        else:
          semLocalValue c, it, crucial # 4
        n = it.n
        patchType c, it.typ, beforeType
  else:
    assert false, "bug"
  c.addSym delayed
  takeParRi c, n
  publish c, delayed.s.name, declStart

proc semLocal(c: var SemContext; it: var Item; kind: SymKind) =
  let info = it.n.info
  semLocal c, it.n, kind
  producesVoid c, info, it.typ

proc addXint(c: var SemContext; x: xint; info: PackedLineInfo) =
  var err = false
  let val = asSigned(x, err)
  if not err:
    c.dest.add intToken(pool.integers.getOrIncl(val), info)
  else:
    let val = asUnsigned(x, err)
    if not err:
      c.dest.add uintToken(pool.uintegers.getOrIncl(val), info)
    else:
      c.buildErr info, "enum value not a constant expression"

proc evalConstIntExpr(c: var SemContext; n: var Cursor; expected: TypeCursor): xint =
  let beforeExpr = c.dest.len
  var x = Item(n: n, typ: expected)
  semExpr c, x
  n = x.n
  result = evalOrdinal(c, cursorAt(c.dest, beforeExpr))
  endRead c.dest

proc semEnumField(c: var SemContext; n: var Cursor; state: var EnumTypeState) =
  let declStart = c.dest.len
  takeToken c, n
  let delayed = handleSymDef(c, n, EfldY) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1
  var crucial = CrucialPragma(sym: delayed.s.name)
  semPragmas c, n, crucial, EfldY # 2
  if state.isBoolType and crucial.magic.len == 0:
    # bool type, set magic to fields if unset
    if state.thisValue == zero():
      crucial.magic = "false"
    else:
      crucial.magic = "true"
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial
  if n.kind == DotToken or n.kind == Symbol:
    if state.isBoolType:
      c.dest.addParLe(BoolT, n.info)
      c.dest.addParRi()
    else:
      c.dest.add symToken(state.enumType, n.info)
    inc n # 3
  else:
    c.buildErr n.info, "enum field's type must be empty"

  if n.kind == DotToken:
    # empty value
    let info = c.dest[declStart].info
    c.dest.add parLeToken(TupleConstrX, info)
    c.addXint state.thisValue, info
    c.dest.add strToken(delayed.lit, info)
    c.dest.addParRi()
    inc n
  else:
    if n.kind == ParLe and n.exprKind == TupleConstrX:
      c.dest.add n
      inc n
      let explicitValue = evalConstIntExpr(c, n, c.types.autoType) # 4
      if explicitValue != state.thisValue:
        state.hasHole = true
        state.thisValue = explicitValue
      c.dest.add evalExpr(c, n)
      takeParRi c, n
    else:
      var ec = initEvalContext(addr c)
      var valueCursor = n
      let fieldValue = eval(ec, valueCursor)
      if fieldValue.kind == StringLit:
        c.dest.add parLeToken(TupleConstrX, n.info)
        c.addXint state.thisValue, n.info
        c.dest.add fieldValue
        c.dest.addParRi()
        n = valueCursor
      else:
        c.dest.add parLeToken(TupleConstrX, n.info)
        let explicitValue = evalConstIntExpr(c, n, c.types.autoType) # 4
        if explicitValue != state.thisValue:
          state.hasHole = true
          state.thisValue = explicitValue
        c.dest.add strToken(delayed.lit, n.info)
        c.dest.addParRi()
  c.addSym delayed
  takeParRi c, n
  publish c, delayed.s.name, declStart

proc semGenericParam(c: var SemContext; n: var Cursor) =
  if n == "typevar":
    semLocal c, n, TypevarY
  else:
    buildErr c, n.info, "expected 'typevar'"

proc semGenericParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n == "typevars":
    inc c.routine.inGeneric
    takeToken c, n
    while n.kind != ParRi:
      semGenericParam c, n
    takeParRi c, n
  elif n == $InvokeT:
    takeTree c, n
  else:
    buildErr c, n.info, "expected '.' or 'typevars'"

proc semParam(c: var SemContext; n: var Cursor) =
  if n == "param":
    semLocal c, n, ParamY
  else:
    buildErr c, n.info, "expected 'param'"

proc semParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n == "params":
    takeToken c, n
    while n.kind != ParRi:
      semParam c, n
    takeParRi c, n
  else:
    buildErr c, n.info, "expected '.' or 'params'"

proc addReturnResult(c: var SemContext; resId: SymId; info: PackedLineInfo) =
  if resId != SymId(0):
    assert c.dest[c.dest.len-1].kind == ParRi
    c.dest.shrink c.dest.len-1 # remove the ParRi
    # maybe add `return result`:
    buildTree(c.dest, RetS, info):
      c.dest.addSymUse resId, info
    c.dest.addParRi() # add it back

proc semBorrow(c: var SemContext; fn: StrId; beforeParams: int) =
  let signature = cursorAt(c.dest, beforeParams)
  var procBody = genBorrowedProcBody(c, fn, signature, signature.info)
  endRead(c.dest)
  var n = cursorAt(procBody, 0)
  takeToken c, n # `(stmts`
  var it = Item(n: n, typ: c.types.autoType)
  semProcBody c, it

proc getParamsType(c: var SemContext; paramsAt: int): seq[TypeCursor] =
  result = @[]
  if c.dest[paramsAt].kind != DotToken:
    var n = cursorAt(c.dest, paramsAt)
    if n.typeKind == ParamsT:
      inc n
      while n.kind != ParRi:
        if n.symKind == ParamY:
          var local = takeLocal(n, SkipFinalParRi)
          result.add local.typ
        else:
          break
      endRead(c.dest)

proc getObjSymId(c: var SemContext; obj: TypeCursor): SymId =
  var obj = skipModifier(obj)
  while true:
    if obj.typeKind == InvokeT:
      inc obj
    else:
      break
  if obj.kind == Symbol:
    result = obj.symId
  else:
    result = SymId(0)

proc checkTypeHook(c: var SemContext; params: seq[TypeCursor]; op: HookKind; info: PackedLineInfo) =
  var cond: bool
  case op
  of NoHook:
    return
  of DestroyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 1
    if not cond:
      buildErr c, info, "signature for '=destroy' must be proc[T: object](x: T)"
  of TraceH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT and classifyType(c, params[1]) == PointerT
  of WasmovedH:
    cond = classifyType(c, c.routine.returnType) == VoidT and
        params.len == 1 and classifyType(c, params[0]) == MutT
  of CopyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of SinkhH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of DupH:
    cond = params.len == 1 and sameTrees(params[0], c.routine.returnType)

  if cond:
    let obj = getObjSymId(c, params[0])

    if obj == SymId(0):
      cond = false
    else:
      let res = tryLoadSym(obj)
      assert res.status == LacksNothing
      if res.decl.symKind == TypeY:
        let typeDecl = asTypeDecl(res.decl)

        if not (classifyType(c, typeDecl.body) in {ObjectT, RefobjT, PtrobjT}):
          cond = false
      else:
        cond = false

  if not cond:
    case op
    of NoHook:
      discard
    of DestroyH:
      buildErr c, info, "signature for '=destroy' must be proc[T: object](x: T)"
    of TraceH:
      buildErr c, info, "signature for '=trace' must be proc[T: object](x: var T; env: pointer)"
    of WasmovedH:
      buildErr c, info, "signature for '=wasMoved' must be proc[T: object](x: var T)"
    of CopyH:
      buildErr c, info, "signature for '=copy' must be proc[T: object](x: var T; y: T)"
    of SinkhH:
      buildErr c, info, "signature for '=sink' must be proc[T: object](x: var T; y: T)"
    of DupH:
      buildErr c, info, "signature for '=dup' must be proc[T: object](x: T): T"

proc expandHook(c: var SemContext; obj: SymId, symId: SymId, op: HookKind) =
  c.hookIndexMap.mgetOrPut($op, @[]).add (obj, symId)

proc getHookName(symId: SymId): string =
  result = pool.syms[symId]
  extractBasename(result)
  #result = result.normalize

proc semHook(c: var SemContext; name: string; beforeParams: int; symId: SymId, info: PackedLineInfo): TypeCursor =
  let params = getParamsType(c, beforeParams)
  case name
  of "=destroy":
    checkTypeHook(c, params, DestroyH, info)
    result = params[0]
  of "=wasMoved":
    checkTypeHook(c, params, WasmovedH, info)
    result = params[0]
  of "=trace":
    checkTypeHook(c, params, TraceH, info)
    result = params[0]
  of "=copy":
    checkTypeHook(c, params, CopyH, info)
    result = params[0]
  of "=sink":
    checkTypeHook(c, params, SinkhH, info)
    result = params[0]
  of "=dup":
    checkTypeHook(c, params, DupH, info)
    result = params[0]
  else:
    raiseAssert "unreachable"

proc hookToKind(name: string): HookKind =
  case name
  of "=destroy": DestroyH
  of "=wasMoved": WasmovedH
  of "=trace": TraceH
  of "=copy": CopyH
  of "=sink": SinkhH
  of "=dup": DupH
  else: NoHook

proc semProc(c: var SemContext; it: var Item; kind: SymKind; pass: PassKind) =
  let info = it.n.info
  let declStart = c.dest.len
  takeToken c, it.n
  let (symId, status) = declareOverloadableSym(c, it, kind)

  let beforeExportMarker = c.dest.len
  wantExportMarker c, it.n
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    buildErr c, it.n.info, "TR pattern not implemented"
    skip it.n
  c.routine = createSemRoutine(kind, c.routine)
  # 'break' and 'continue' are valid in a template regardless of whether we
  # really have a loop or not:
  if kind == TemplateY:
    inc c.routine.inLoop
    inc c.routine.inGeneric

  try:
    c.openScope() # open parameter scope
    let beforeGenericParams = c.dest.len
    semGenericParams c, it.n
    let beforeParams = c.dest.len
    semParams c, it.n
    c.routine.returnType = semReturnType(c, it.n)
    var crucial = CrucialPragma(sym: symId)
    semPragmas c, it.n, crucial, kind
    c.routine.pragmas = crucial.flags
    if crucial.hasVarargs.isValid:
      addVarargsParameter c, beforeParams, crucial.hasVarargs
    if crucial.magic.len > 0:
      exportMarkerBecomesNifTag c, beforeExportMarker, crucial
    if it.n.kind == DotToken:
      takeToken c, it.n
    else:
      buildErr c, it.n.info, "`effects` must be empty"
      skip it.n

    publishSignature c, symId, declStart
    if kind == ConverterY and status in {OkNew, OkExistingFresh}:
      let root = nominalRoot(c.routine.returnType)
      if root == SymId(0) and not c.g.config.compat:
        var errBuf = createTokenBuf(16)
        swap c.dest, errBuf
        buildErr c, info, "cannot attach converter to type " & typeToString(c.routine.returnType)
        swap c.dest, errBuf
        c.dest.insert errBuf, declStart
      else:
        c.converters.mgetOrPut(root, @[]).add(symId)
        if c.dest[beforeExportMarker].kind != DotToken:
          # exported
          if not (c.dest[beforeGenericParams].kind == ParLe and
              pool.tags[c.dest[beforeGenericParams].tagId] == $InvokeT):
            # don't register instances
            c.converterIndexMap.add((root, symId))
    if it.n.kind != DotToken:
      case pass
      of checkGenericInst:
        if it.n != "stmts":
          error "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        takeToken c, it.n
        semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope

        let hk = hookToKind(getHookName(symId))
        if hk != NoHook:
          let params = getParamsType(c, beforeParams)
          assert params.len >= 1
          let obj = getObjSymId(c, params[0])
          expandHook(c, obj, symId, hk)

      of checkBody:
        if it.n != "stmts":
          error "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        takeToken c, it.n
        let resId = declareResult(c, it.n.info)
        semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope
        addReturnResult c, resId, it.n.info
        let name = getHookName(symId)
        let hk = hookToKind(name)
        if hk != NoHook:
          let objCursor = semHook(c, name, beforeParams, symId, info)
          let obj = getObjSymId(c, objCursor)

          if c.routine.inGeneric == 0:
            # because it's a hook for sure
            expandHook(c, obj, symId, hk)
          else:
            c.genericHooks.mgetOrPut(obj, @[]).add symId

      of checkSignatures:
        c.takeTree it.n
        c.closeScope() # close parameter scope
      of checkConceptProc:
        c.closeScope() # close parameter scope
        if it.n.kind == DotToken:
          inc it.n
        else:
          c.buildErr it.n.info, "inside a `concept` a routine cannot have a body"
          skip it.n
    else:
      if BorrowP in crucial.flags and pass in {checkGenericInst, checkBody}:
        if kind notin {ProcY, FuncY, ConverterY, TemplateY, MethodY}:
          c.buildErr it.n.info, ".borrow only valid for proc, func, converter, template or method"
        else:
          semBorrow(c, symToIdent(symId), beforeParams)
        inc it.n # skip DotToken
      else:
        takeToken c, it.n
      c.closeScope() # close parameter scope
  finally:
    c.routine = c.routine.parent
  takeParRi c, it.n
  producesVoid c, info, it.typ
  publish c, symId, declStart

proc semExprSym(c: var SemContext; it: var Item; s: Sym; start: int; flags: set[SemFlag]) =
  it.kind = s.kind
  let expected = it.typ
  if s.kind == NoSym:
    if AllowUndeclared notin flags:
      var orig = createTokenBuf(1)
      orig.add c.dest[c.dest.len-1]
      c.dest.shrink c.dest.len-1
      if pool.syms.hasId(s.name):
        c.buildErr it.n.info, "undeclared identifier: " & pool.syms[s.name], cursorAt(orig, 0)
      else:
        c.buildErr it.n.info, "undeclared identifier", cursorAt(orig, 0)
    it.typ = c.types.autoType
  elif s.kind == CchoiceY:
    if AllowOverloads notin flags and c.routine.kind != TemplateY:
      c.buildErr it.n.info, "ambiguous identifier"
    it.typ = c.types.autoType
  elif s.kind in {TypeY, TypevarY}:
    let typeStart = c.dest.len
    let info = c.dest[c.dest.len-1].info
    c.dest.buildTree TypedescT, info:
      let symStart = c.dest.len
      c.dest.add symToken(s.name, info)
      semTypeSym c, s, info, symStart, InLocalDecl
    it.typ = typeToCursor(c, typeStart)
    c.dest.shrink typeStart
    commonType c, it, start, expected
  else:
    let res = declToCursor(c, s)
    if KeepMagics notin flags:
      maybeInlineMagic c, res
    if res.status == LacksNothing:
      var n = res.decl
      if s.kind.isLocal or s.kind == EfldY:
        skipToLocalType n
      elif s.kind.isRoutine:
        skipToParams n
      elif s.kind == BlockY:
        discard
      elif s.kind == ModuleY:
        if AllowModuleSym notin flags:
          c.buildErr it.n.info, "module symbol '" & pool.syms[s.name] & "' not allowed in this context"
      else:
        # XXX enum field?
        assert false, "not implemented"
      it.typ = n
      commonType c, it, start, expected
    else:
      c.buildErr it.n.info, "could not load symbol: " & pool.syms[s.name] & "; errorCode: " & $res.status
      it.typ = c.types.autoType

proc semLocalTypeExpr(c: var SemContext, it: var Item) =
  let val = semLocalType(c, it.n)
  let start = c.dest.len
  c.dest.buildTree TypedescT, it.n.info:
    c.dest.addSubtree val
  it.typ = typeToCursor(c, start)
  c.dest.shrink start

proc semSubscriptAsgn(c: var SemContext; it: var Item; info: PackedLineInfo) =
  # check if LHS is builtin subscript:
  var subscript = Item(n: it.n, typ: c.types.autoType)
  inc subscript.n # tag
  var subscriptLhsBuf = createTokenBuf(4)
  swap c.dest, subscriptLhsBuf
  var subscriptLhs = Item(n: subscript.n, typ: c.types.autoType)
  semExpr c, subscriptLhs, {KeepMagics}
  swap c.dest, subscriptLhsBuf
  let afterSubscriptLhs = subscriptLhs.n
  subscript.n = afterSubscriptLhs
  subscriptLhs.n = cursorAt(subscriptLhsBuf, 0)
  var subscriptBuf = createTokenBuf(8)
  swap c.dest, subscriptBuf
  let builtin = tryBuiltinSubscript(c, subscript, subscriptLhs)
  swap c.dest, subscriptBuf
  if builtin:
    # build regular assignment:
    c.dest.addParLe(AsgnS, info)
    c.dest.add subscriptBuf
    subscript.typ = skipModifier(subscript.typ) # remove `var` for rhs
    semExpr c, subscript # use the type and position from the subscript
    it.n = subscript.n
    takeParRi c, it.n
    producesVoid c, info, it.typ
  else:
    # generate call to `[]=`:
    var callBuf = createTokenBuf(16)
    callBuf.addParLe(CallX, subscriptLhs.n.info)
    callBuf.add identToken(pool.strings.getOrIncl("[]="), subscriptLhs.n.info)
    callBuf.addSubtree subscriptLhs.n
    it.n = afterSubscriptLhs
    while it.n.kind != ParRi:
      # arguments of the subscript
      callBuf.takeTree it.n
    skipParRi it.n # end subscript expression
    callBuf.takeTree it.n # assignment value
    callBuf.addParRi()
    skipParRi it.n # end assignment
    var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
    semCall c, call, {}, SubscriptAsgnCall
    it.typ = call.typ

proc semDotAsgn(c: var SemContext; it: var Item; info: PackedLineInfo) =
  # check if LHS is builtin subscript:
  let dotInfo = it.n.info
  var dot = Item(n: it.n, typ: c.types.autoType)
  inc dot.n # tag
  var dotLhsBuf = createTokenBuf(4)
  swap c.dest, dotLhsBuf
  var dotLhs = Item(n: dot.n, typ: c.types.autoType)
  semExpr c, dotLhs, {KeepMagics}
  swap c.dest, dotLhsBuf
  dot.n = dotLhs.n
  dotLhs.n = cursorAt(dotLhsBuf, 0)
  let fieldName = getIdent(dot.n)
  # skip optional inheritance depth:
  if dot.n.kind == IntLit:
    inc dot.n
  skipParRi dot.n
  var dotBuf = createTokenBuf(8)
  swap c.dest, dotBuf
  let builtin = tryBuiltinDot(c, dot, dotLhs, fieldName, dotInfo, {}) != FailedDot
  swap c.dest, dotBuf
  if builtin:
    # build regular assignment:
    c.dest.addParLe(AsgnS, info)
    c.dest.add dotBuf
    dot.typ = skipModifier(dot.typ) # remove `var` for rhs
    semExpr c, dot # use the type and position from the dot expression
    it.n = dot.n
    takeParRi c, it.n
    producesVoid c, info, it.typ
  else:
    # generate call to `field=`:
    var callBuf = createTokenBuf(16)
    callBuf.addParLe(CallX, dotLhs.n.info)
    callBuf.add identToken(pool.strings.getOrIncl(pool.strings[fieldName] & "="), dotLhs.n.info)
    callBuf.addSubtree dotLhs.n
    it.n = dot.n
    callBuf.takeTree it.n # assignment value
    callBuf.addParRi()
    skipParRi it.n
    var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
    semCall c, call, {}, DotAsgnCall
    # XXX original compiler also checks if the call fails and tries a dotcall for the LHS
    it.typ = call.typ

proc semAsgn(c: var SemContext; it: var Item) =
  let info = it.n.info
  inc it.n
  case it.n.exprKind
  of AtX:
    semSubscriptAsgn c, it, info
  of DotX, DdotX:
    semDotAsgn c, it, info
  else:
    c.dest.addParLe(AsgnS, info)
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a # infers type of `left-hand-side`
    a.typ = skipModifier(a.typ) # remove `var` for rhs
    semExpr c, a # ensures type compatibility with `left-hand-side`
    it.n = a.n
    takeParRi c, it.n
    producesVoid c, info, it.typ

proc semEmit(c: var SemContext; it: var Item) =
  let info = it.n.info
  c.dest.add parLeToken(EmitS, info)
  inc it.n
  if it.n.exprKind == BracketX:
    inc it.n
    while it.n.kind != ParRi:
      var a = Item(n: it.n, typ: c.types.autoType)
      semExpr c, a
      it.n = a.n
    skipParRi it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a
    it.n = a.n
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semDiscard(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.types.autoType)
    semExpr c, a
    it.n = a.n
    if classifyType(c, it.typ) == VoidT:
      buildErr c, it.n.info, "expression of type `" & typeToString(it.typ) & "` must not be discarded"
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semStmtBranch(c: var SemContext; it: var Item; isNewScope: bool) =
  # handle statements that could be expressions
  case classifyType(c, it.typ)
  of AutoT:
    semExpr c, it
  of VoidT:
    # performs discard check:
    semStmt c, it.n, isNewScope
  else:
    var ex = Item(n: it.n, typ: it.typ)
    let start = c.dest.len
    semExpr c, ex
    # this is handled by commonType, since it has to be done deeply:
    #if classifyType(c, ex.typ) == VoidT:
    #  # allow statement in expression context if it is noreturn
    #  let ignore = isNoReturn(cursorAt(c.dest, start))
    #  endRead(c.dest)
    #  if not ignore:
    #    typeMismatch(c, it.n.info, ex.typ, it.typ)
    commonType(c, ex, start, it.typ)
    it.n = ex.n

proc semIf(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if it.n.substructureKind == ElifU:
    while it.n.substructureKind == ElifU:
      takeToken c, it.n
      semBoolExpr c, it.n
      withNewScope c:
        semStmtBranch c, it, true
      takeParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseU:
    takeToken c, it.n
    withNewScope c:
      semStmtBranch c, it, true
    takeParRi c, it.n
  takeParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semTry(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  withNewScope c:
    semStmtBranch c, it, true
  while it.n.substructureKind == ExceptU:
    takeToken c, it.n
    # XXX Implement `e as Type` properly!
    var exc = Item(n: it.n, typ: c.types.autoType)
    semExpr c, exc
    it.n = exc.n
    withNewScope c:
      semStmtBranch c, it, true
    takeParRi c, it.n
  if it.n.substructureKind == FinU:
    takeToken c, it.n
    withNewScope c:
      semStmt c, it.n, true
    takeParRi c, it.n
  takeParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semWhen(c: var SemContext; it: var Item) =
  let start = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var leaveUnresolved = false
  if it.n.substructureKind == ElifU:
    while it.n.substructureKind == ElifU:
      takeToken c, it.n
      let condStart = c.dest.len
      semConstBoolExpr c, it.n
      let condValue = cursorAt(c.dest, condStart).exprKind
      endRead(c.dest)
      if not leaveUnresolved:
        if condValue == TrueX:
          c.dest.shrink start
          semExpr c, it
          skipParRi it.n # finish elif
          skipToEnd it.n
          return
        elif condValue != FalseX:
          # erroring/unresolved condition, leave entire statement as unresolved
          leaveUnresolved = true
      takeTree c, it.n
      takeParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `elif` inside `if` expected"
  if it.n.substructureKind == ElseU:
    takeToken c, it.n
    if not leaveUnresolved:
      c.dest.shrink start
      semExpr c, it
      skipParRi it.n # finish else
      skipToEnd it.n
      return
    else:
      takeTree c, it.n
    takeParRi c, it.n
  takeParRi c, it.n
  if not leaveUnresolved:
    # none of the branches evaluated, output nothing
    c.dest.shrink start
    producesVoid c, info, it.typ

proc semCaseOfValue(c: var SemContext; it: var Item; selectorType: TypeCursor;
                    seen: var seq[(xint, xint)]) =
  if it.n == "ranges":
    takeToken c, it.n
    while it.n.kind != ParRi:
      let info = it.n.info
      if isRangeExpr(it.n):
        inc it.n # call tag
        skip it.n # `..`
        c.dest.buildTree RangeU, it.n.info:
          let a = evalConstIntExpr(c, it.n, selectorType)
          let b = evalConstIntExpr(c, it.n, selectorType)
          if seen.doesOverlapOrIncl(a, b):
            buildErr c, info, "overlapping values"
        inc it.n # right paren of call
      elif it.n.substructureKind == RangeU:
        takeToken c, it.n
        let a = evalConstIntExpr(c, it.n, selectorType)
        let b = evalConstIntExpr(c, it.n, selectorType)
        if seen.doesOverlapOrIncl(a, b):
          buildErr c, info, "overlapping values"
        takeParRi c, it.n
      else:
        let a = evalConstIntExpr(c, it.n, selectorType)
        if seen.containsOrIncl(a):
          buildErr c, info, "value already handled"
    takeParRi c, it.n
  else:
    buildErr c, it.n.info, "`ranges` within `of` expected"
    skip it.n

proc semCase(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  var selector = Item(n: it.n, typ: c.types.autoType)
  semExpr c, selector
  it.n = selector.n
  var seen: seq[(xint, xint)] = @[]
  if it.n.substructureKind == OfU:
    while it.n.substructureKind == OfU:
      takeToken c, it.n
      semCaseOfValue c, it, selector.typ, seen
      withNewScope c:
        semStmtBranch c, it, true
      takeParRi c, it.n
  else:
    buildErr c, it.n.info, "illformed AST: `of` inside `case` expected"
  if it.n.substructureKind == ElseU:
    takeToken c, it.n
    withNewScope c:
      semStmtBranch c, it, true
    takeParRi c, it.n
  takeParRi c, it.n
  if typeKind(it.typ) == AutoT:
    producesVoid c, info, it.typ

proc semForLoopVar(c: var SemContext; it: var Item; loopvarType: TypeCursor) =
  if stmtKind(it.n) == LetS:
    let declStart = c.dest.len
    takeToken c, it.n
    let delayed = handleSymDef(c, it.n, LetY)
    c.addSym delayed
    wantDot c, it.n # export marker must be empty
    wantDot c, it.n # pragmas
    copyTree c.dest, loopvarType
    skip it.n # skip over the type which might have been set already as we tend to re-sem stuff
    wantDot c, it.n # value
    takeParRi c, it.n
    publish c, delayed.s.name, declStart
  else:
    buildErr c, it.n.info, "illformed AST: `let` inside `unpackflat` expected"
    skip it.n

proc isIterator(c: var SemContext; s: SymId): bool =
  let sym = fetchSym(c, s)
  let res = declToCursor(c, sym)
  result = res.status == LacksNothing and res.decl.symKind == IteratorY

proc semForLoopTupleVar(c: var SemContext; it: var Item; tup: TypeCursor) =
  # TODO: implements mixed unpacktup and unpackflat: e.g. for i, j, (m, n) in ...
  var tup = tup
  inc tup
  while it.n.kind != ParRi and tup.kind != ParRi:
    let field = asLocal(tup)
    semForLoopVar c, it, field.typ
    skip tup
  if it.n.kind == ParRi:
    if tup.kind == ParRi:
      discard "all fine"
    else:
      buildErr c, it.n.info, "too few for loop variables"
  else:
    buildErr c, it.n.info, "too many for loop variables"
    skipToEnd it.n

proc semFor(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  var iterCall = Item(n: it.n, typ: c.types.autoType)
  let beforeCall = c.dest.len
  semExpr c, iterCall, {PreferIterators, KeepMagics}
  var isMacroLike = false
  if c.dest[beforeCall+1].kind == Symbol and c.isIterator(c.dest[beforeCall+1].symId):
    discard "fine"
  elif iterCall.typ.typeKind == UntypedT:
    isMacroLike = true
  else:
    buildErr c, it.n.info, "iterator expected"
  it.n = iterCall.n
  withNewScope c:
    case substructureKind(it.n)
    of UnpackflatU:
      takeToken c, it.n
      if iterCall.typ.typeKind == TupleT:
        semForLoopTupleVar c, it, iterCall.typ
      else:
        semForLoopVar c, it, iterCall.typ

      takeParRi c, it.n
    of UnpacktupU:
      takeToken c, it.n
      if iterCall.typ.typeKind == TupleT:
        semForLoopTupleVar c, it, iterCall.typ
      else:
        buildErr c, it.n.info, "tuple types expected, but got: " & $iterCall.typ
      takeParRi c, it.n
    else:
      buildErr c, it.n.info, "illformed AST: `unpackflat` or `unpacktup` inside `for` expected"
      skip it.n

    if isMacroLike and false:
      takeTree c.dest, it.n # don't touch the body
    else:
      inc c.routine.inLoop
      semStmt c, it.n, true
      dec c.routine.inLoop

  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semReturn(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind == NoSym:
    buildErr c, it.n.info, "`return` only allowed within a routine"
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.routine.returnType)
    # `return` within a template refers to the caller, so
    # we allow any type here:
    if c.routine.kind == TemplateY:
      a.typ = c.types.autoType
    semExpr c, a
    it.n = a.n
  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semRaise(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind == NoSym:
    buildErr c, it.n.info, "`raise` only allowed within a routine"
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.routine.returnType)
    # `return` within a template refers to the caller, so
    # we allow any type here:
    if c.routine.kind == TemplateY:
      a.typ = c.types.autoType
    semExpr c, a
    it.n = a.n
  takeParRi c, it.n
  producesNoReturn c, info, it.typ

proc semYield(c: var SemContext; it: var Item) =
  let info = it.n.info
  takeToken c, it.n
  if c.routine.kind != IteratorY:
    buildErr c, it.n.info, "`yield` only allowed within an `iterator`"
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    var a = Item(n: it.n, typ: c.routine.returnType)
    semExpr c, a
    it.n = a.n
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semTypePragmas(c: var SemContext; n: var Cursor; sym: SymId; beforeExportMarker: int): CrucialPragma =
  result = CrucialPragma(sym: sym)
  semPragmas c, n, result, TypeY # 2
  if result.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, result

proc fitTypeToPragmas(c: var SemContext; pragmas: CrucialPragma; typeStart: int) =
  if {ImportcP, ImportcppP} * pragmas.flags != {}:
    let typ = cursorAt(c.dest, typeStart)
    if isNominal(typ.typeKind):
      # ok
      endRead(c.dest)
    elif typ.typeKind in {IntT, UintT}:
      let info = typ.info
      endRead(c.dest)
      let kind = if ImportcP in pragmas.flags: ImportcP else: ImportcppP
      var tokens = [
        parLeToken(pool.tags.getOrIncl($kind), info),
        strToken(pool.strings.getOrIncl(pragmas.externName), info),
        parRiToken(info)
      ]
      c.dest.insert fromBuffer(tokens), typeStart+2
    else:
      let err = "cannot import type " & typeToString(typ)
      let info = typ.info
      endRead(c.dest)
      c.buildErr info, err

proc semTypeSection(c: var SemContext; n: var Cursor) =
  let declStart = c.dest.len
  takeToken c, n
  # name, export marker, generic params, pragmas, body
  let delayed = handleSymDef(c, n, TypeY) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1

  var isEnumTypeDecl = false

  if c.phase == SemcheckSignatures or
      (delayed.status in {OkNew, OkExistingFresh} and
        c.phase != SemcheckTopLevelSyms):
    var isGeneric: bool
    let prevGeneric = c.routine.inGeneric
    if n.kind == DotToken:
      takeToken c, n
      isGeneric = false
    else:
      let oldScopeKind = c.currentScope.kind
      openScope c
      semGenericParams c, n
      # copy toplevel scope status for exported fields
      c.currentScope.kind = oldScopeKind
      isGeneric = true

    let crucial = semTypePragmas(c, n, delayed.s.name, beforeExportMarker)

    # body:
    if n.kind == DotToken:
      takeToken c, n
    else:
      let typeStart = c.dest.len
      if n.typeKind in {EnumT, HoleyEnumT}:
        semEnumType(c, n, delayed.s.name, beforeExportMarker)
        isEnumTypeDecl = true
      else:
        semLocalTypeImpl c, n, InTypeSection
      fitTypeToPragmas(c, crucial, typeStart)
    if isGeneric:
      closeScope c
      c.routine.inGeneric = prevGeneric # revert increase by semGenericParams
  else:
    c.takeTree n # generics
    discard semTypePragmas(c, n, delayed.s.name, beforeExportMarker)
    c.takeTree n # body

  c.addSym delayed
  takeParRi c, n


  publish c, delayed.s.name, declStart

  if isEnumTypeDecl:
    var enumTypeDecl = tryLoadSym(delayed.s.name)
    assert enumTypeDecl.status == LacksNothing
    genEnumToStrProc(c, enumTypeDecl.decl)

proc semTypedBinaryArithmetic(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  semExpr c, it
  semExpr c, it
  takeParRi c, it.n
  commonType c, it, beforeExpr, typ

proc semCmp(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var operand = Item(n: it.n, typ: typ)
  semExpr c, operand
  semExpr c, operand
  it.n = operand.n
  takeParRi c, it.n
  commonType c, it, beforeExpr, c.types.boolType

proc literal(c: var SemContext; it: var Item; literalType: TypeCursor) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let expected = it.typ
  it.typ = literalType
  commonType c, it, beforeExpr, expected

proc literalB(c: var SemContext; it: var Item; literalType: TypeCursor) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  takeParRi c, it.n
  let expected = it.typ
  it.typ = literalType
  commonType c, it, beforeExpr, expected

proc semNil(c: var SemContext; it: var Item) =
  literalB c, it, c.types.nilType

proc semTypedUnaryArithmetic(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  semExpr c, it
  takeParRi c, it.n
  commonType c, it, beforeExpr, typ

proc semBracket(c: var SemContext, it: var Item; flags: set[SemFlag]) =
  let exprStart = c.dest.len
  let info = it.n.info
  inc it.n
  c.dest.addParLe(AconstrX, info)
  if it.n.kind == ParRi:
    # empty array
    if it.typ.typeKind == AutoT:
      if AllowEmpty in flags:
        # keep it.typ as auto
        c.dest.addSubtree it.typ
      else:
        buildErr c, it.n.info, "empty array needs a specified type"
    else:
      c.dest.addSubtree it.typ
    takeParRi c, it.n
    return

  let typeInsertPos = c.dest.len
  var elem = Item(n: it.n, typ: c.types.autoType)
  case it.typ.typeKind
  of ArrayT: # , SeqT, OpenArrayT
    var arr = it.typ
    inc arr
    elem.typ = arr
  of AutoT: discard
  else:
    buildErr c, it.n.info, "invalid expected type for array constructor: " & typeToString(it.typ)
  # XXX index types, `index: value` etc not implemented
  semExpr c, elem
  var count = 1
  while elem.n.kind != ParRi:
    semExpr c, elem
    inc count
  it.n = elem.n
  takeParRi c, it.n
  let typeStart = c.dest.len
  c.dest.buildTree ArrayT, it.n.info:
    c.dest.addSubtree elem.typ
    c.dest.addParLe(RangetypeT, it.n.info)
    c.dest.addSubtree c.types.intType
    c.dest.addIntLit(0, it.n.info)
    c.dest.addIntLit(count - 1, it.n.info)
    c.dest.addParRi()
  let expected = it.typ
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  c.dest.insert it.typ, typeInsertPos
  commonType c, it, exprStart, expected

proc semCurly(c: var SemContext, it: var Item; flags: set[SemFlag]) =
  let exprStart = c.dest.len
  let info = it.n.info
  inc it.n
  c.dest.addParLe(SetConstrX, info)
  if it.n.kind == ParRi:
    # empty set
    if it.typ.typeKind == AutoT:
      if AllowEmpty in flags:
        # keep it.typ as auto
        c.dest.addSubtree it.typ
      else:
        buildErr c, it.n.info, "empty set needs a specified type"
    else:
      c.dest.addSubtree it.typ
    takeParRi c, it.n
    return

  let typeInsertPos = c.dest.len
  var elem = Item(n: it.n, typ: c.types.autoType)
  case it.typ.typeKind
  of SetT:
    var t = it.typ
    inc t
    elem.typ = t
  of AutoT: discard
  else:
    buildErr c, it.n.info, "invalid expected type for set constructor: " & typeToString(it.typ)
  var elemStart = c.dest.len
  var elemInfo = elem.n.info
  while elem.n.kind != ParRi:
    if isRangeExpr(elem.n):
      inc elem.n # call tag
      skip elem.n # `..`
      c.dest.buildTree RangeU, elem.n.info:
        elemStart = c.dest.len
        elemInfo = elem.n.info
        semExpr c, elem
        semExpr c, elem
      inc elem.n # right paren of call
    elif elem.n.substructureKind == RangeU:
      takeToken c, elem.n
      semExpr c, elem
      semExpr c, elem
      takeParRi c, elem.n
    else:
      semExpr c, elem
  if containsGenericParams(elem.typ):
    discard
  elif not isOrdinalType(elem.typ, allowEnumWithHoles = true):
    c.buildErr elemInfo, "set element type must be ordinal"
  #elif elem.typ.typeKind == IntT and c.dest[elemStart].kind == IntLit:
  #  set to range of 0..<DefaultSetElements
  else:
    let length = lengthOrd(c, elem.typ)
    if length.isNaN or length > MaxSetElements:
      c.buildErr elemInfo, "type " & typeToString(elem.typ) & " is too large to be a set element type"
  it.n = elem.n
  takeParRi c, it.n
  let typeStart = c.dest.len
  c.dest.buildTree SetT, it.n.info:
    c.dest.addSubtree elem.typ
  let expected = it.typ
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  c.dest.insert it.typ, typeInsertPos
  commonType c, it, exprStart, expected

proc semArrayConstr(c: var SemContext; it: var Item) =
  let start = c.dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken c, it.n
  it.typ = semLocalType(c, it.n)
  # XXX type length not enforced
  var elem = Item(n: it.n, typ: c.types.autoType)
  if it.typ.typeKind == ArrayT:
    elem.typ = it.typ
    inc elem.typ
  else:
    c.buildErr info, "expected array type for array constructor, got: " & typeToString(it.typ)
  while elem.n.kind != ParRi:
    semExpr c, elem
  it.n = elem.n
  takeParRi c, it.n
  commonType c, it, start, expected

proc semSetConstr(c: var SemContext; it: var Item) =
  let start = c.dest.len
  let expected = it.typ
  let info = it.n.info
  takeToken c, it.n
  it.typ = semLocalType(c, it.n)
  var elem = Item(n: it.n, typ: c.types.autoType)
  if it.typ.typeKind == SetT:
    elem.typ = it.typ
    inc elem.typ
  else:
    c.buildErr info, "expected set type for set constructor, got: " & typeToString(it.typ)
  while elem.n.kind != ParRi:
    if elem.n.substructureKind == RangeU:
      takeToken c, elem.n
      semExpr c, elem
      semExpr c, elem
      takeParRi c, elem.n
    else:
      semExpr c, elem
  it.n = elem.n
  takeParRi c, it.n
  commonType c, it, start, expected

proc semSuf(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  takeToken c, it.n
  var num = Item(n: it.n, typ: c.types.autoType)
  semExpr c, num
  it.n = num.n
  if it.n.kind != StringLit:
    c.buildErr it.n.info, "string literal expected for suf"
    skip it.n
    return
  let expected = it.typ
  case pool.strings[it.n.litId]
  of "i": it.typ = c.types.intType
  of "i8": it.typ = c.types.int8Type
  of "i16": it.typ = c.types.int16Type
  of "i32": it.typ = c.types.int32Type
  of "i64": it.typ = c.types.int64Type
  of "u": it.typ = c.types.uintType
  of "u8": it.typ = c.types.uint8Type
  of "u16": it.typ = c.types.uint16Type
  of "u32": it.typ = c.types.uint32Type
  of "u64": it.typ = c.types.uint64Type
  of "f": it.typ = c.types.floatType
  of "f32": it.typ = c.types.float32Type
  of "f64": it.typ = c.types.float64Type
  of "R": it.typ = c.types.stringType
  else:
    c.buildErr it.n.info, "unknown suffix: " & pool.strings[it.n.litId]
  takeToken c, it.n # suffix
  takeParRi c, it.n # right paren
  commonType c, it, exprStart, expected

proc semTupleConstr(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  let origExpected = it.typ
  takeToken c, it.n
  if it.n.kind == ParRi:
    takeParRi c, it.n
    it.typ = c.types.emptyTupletype
    commonType c, it, exprStart, origExpected
    return
  var expected = origExpected
  var doExpected = expected.typeKind == TupleT
  if doExpected:
    inc expected # skip tag, now at fields
  let named = it.n.substructureKind == KvU
  var typ = createTokenBuf(32)
  typ.add parLeToken(TupleT, it.n.info)
  var i = 0
  while it.n.kind != ParRi:
    typ.add parLeToken(FldU, it.n.info) # start field
    if named:
      if it.n.substructureKind != KvU:
        c.buildErr it.n.info, "expected field name for named tuple constructor"
      else:
        takeToken c, it.n
        typ.addSubtree it.n # add name
        takeToken c, it.n
    else:
      typ.add identToken(pool.strings.getOrIncl("Field" & $i), it.n.info)
      inc i
    typ.addDotToken() # export marker
    typ.addDotToken() # pragmas
    var elem = Item(n: it.n, typ: c.types.autoType)
    if doExpected:
      let fld = asLocal(expected)
      elem.typ = fld.typ
      skip expected
      if expected.kind == ParRi:
        # happens if expected tuple type has less fields than constructor
        doExpected = false
    semExpr c, elem
    it.n = elem.n
    if named:
      # should be KvX
      takeParRi c, it.n
    typ.addSubtree elem.typ # type
    typ.addDotToken() # value
    typ.addParRi() # end field
  takeParRi c, it.n
  typ.addParRi()
  let typeStart = c.dest.len
  var t = typ.cursorAt(0)
  semTupleType c, t
  it.typ = typeToCursor(c, typeStart)
  c.dest.shrink typeStart
  commonType c, it, exprStart, origExpected

proc callDefault(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(CallX, info)
  swap c.dest, callBuf
  discard buildSymChoice(c, pool.strings.getOrIncl("default"), info, FindAll)
  swap c.dest, callBuf
  callBuf.addSubtree typ
  callBuf.addParRi()
  var it = Item(n: cursorAt(callBuf, 0), typ: c.types.autoType)
  semCall c, it, {}

proc buildObjConstrField(c: var SemContext; field: Local; setFields: Table[SymId, Cursor]; info: PackedLineInfo) =
  let fieldSym = field.name.symId
  if fieldSym in setFields:
    c.dest.addSubtree setFields[fieldSym]
  else:
    c.dest.addParLe(KvU, info)
    c.dest.add symToken(fieldSym, info)
    callDefault c, field.typ, info
    c.dest.addParRi()

proc buildDefaultObjConstr(c: var SemContext; typ: Cursor; setFields: Table[SymId, Cursor]; info: PackedLineInfo) =
  var constrKind = NoExpr
  var objImpl = typ
  if objImpl.typeKind == RefT:
    constrKind = NewOconstrX
    inc objImpl
  if objImpl.typeKind == InvokeT:
    inc objImpl
  if objImpl.kind == Symbol:
    objImpl = objtypeImpl(objImpl.symId)
    if constrKind == NoExpr:
      case objImpl.typeKind
      of RefobjT:
        constrKind = NewOconstrX
      of ObjectT:
        constrKind = OconstrX
      else:
        discard # error
  c.dest.addParLe(constrKind, info)
  c.dest.addSubtree typ
  var obj = asObjectDecl(objImpl)
  # same field order as old nim VM: starting with most shallow base type
  while obj.parentType.kind != DotToken:
    var parentImpl = obj.parentType
    if parentImpl.typeKind in {RefT, PtrT}:
      inc parentImpl
    if parentImpl.kind == Symbol:
      parentImpl = objtypeImpl(parentImpl.symId)
    elif parentImpl.typeKind == InvokeT:
      inc parentImpl # get to symbol
      parentImpl = objtypeImpl(parentImpl.symId)
    else:
      # should not be possible
      discard
    let parent = asObjectDecl(parentImpl)
    obj.parentType = parent.parentType
    var currentField = parent.firstField
    if currentField.kind != DotToken:
      while currentField.kind != ParRi:
        let field = asLocal(currentField)
        buildObjConstrField(c, field, setFields, info)
        skip currentField
  var currentField = obj.firstField
  if currentField.kind != DotToken:
    while currentField.kind != ParRi:
      let field = asLocal(currentField)
      buildObjConstrField(c, field, setFields, info)
      skip currentField
  c.dest.addParRi()

proc semObjConstr(c: var SemContext, it: var Item) =
  let exprStart = c.dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, it.n)
  c.dest.shrink exprStart
  var decl = default(TypeDecl)
  var objType = it.typ
  let isGenericObj = containsGenericParams(objType)
  if objType.typeKind in {RefT, PtrT}:
    inc objType
  if objType.typeKind == InvokeT:
    inc objType
  if objType.kind == Symbol:
    decl = getTypeSection(objType.symId)
    objType = decl.body
    # emulate objtypeImpl
    if objType.typeKind in {RefT, PtrT}:
      inc objType
    if objType.typeKind notin {ObjectT, RefobjT, PtrobjT}:
      c.buildErr info, "expected object type for object constructor"
      return
  var fieldBuf = createTokenBuf(16)
  var setFieldPositions = initTable[SymId, int]()
  while it.n.kind != ParRi:
    if it.n.substructureKind != KvU:
      c.buildErr it.n.info, "expected key/value pair in object constructor"
    else:
      let fieldStart = fieldBuf.len
      fieldBuf.add it.n
      inc it.n
      let fieldInfo = it.n.info
      let fieldNameCursor = it.n
      let fieldName = getIdent(it.n)
      if fieldName == StrId(0):
        c.buildErr fieldInfo, "identifier expected for object field"
        skip it.n
      else:
        var field = ObjField(level: -1)
        if fieldNameCursor.kind == Symbol:
          let sym = fieldNameCursor.symId
          let res = tryLoadSym(sym)
          if res.status == LacksNothing and res.decl == $FldY:
            # trust that it belongs to this object for now
            # level is not known but not used either, set it to 0:
            field = ObjField(sym: sym, typ: asLocal(res.decl).typ, level: 0)
          else:
            field = findObjFieldConsiderVis(c, decl, fieldName)
        else:
          field = findObjFieldConsiderVis(c, decl, fieldName)
        if field.level >= 0:
          if field.sym in setFieldPositions:
            c.buildErr fieldInfo, "field already set: " & pool.strings[fieldName]
            skip it.n
          else:
            setFieldPositions[field.sym] = fieldStart
            if isGenericObj:
              # do not save generic field sym
              fieldBuf.add identToken(fieldName, info)
            else:
              fieldBuf.add symToken(field.sym, info)
            # maybe add inheritance depth too somehow?
            var val = Item(n: it.n, typ: field.typ)
            swap c.dest, fieldBuf
            semExpr c, val
            swap c.dest, fieldBuf
            it.n = val.n
        else:
          c.buildErr fieldInfo, "undeclared field: '" & pool.strings[fieldName] & "' for type " & typeToString(it.typ)
          skip it.n
      fieldBuf.addParRi()
      skipParRi it.n
  skipParRi it.n
  var setFields = initTable[SymId, Cursor]()
  for field, pos in setFieldPositions:
    setFields[field] = cursorAt(fieldBuf, pos)
  buildDefaultObjConstr(c, it.typ, setFields, info)
  commonType c, it, exprStart, expected

proc semObjDefault(c: var SemContext; it: var Item) =
  let exprStart = c.dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, it.n)
  c.dest.shrink exprStart
  skipParRi it.n
  buildDefaultObjConstr(c, it.typ, initTable[SymId, Cursor](), info)
  commonType c, it, exprStart, expected

proc buildDefaultTuple(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  c.dest.addParLe(TupleConstrX, info)
  var currentField = typ
  inc currentField # skip tuple tag
  while currentField.kind != ParRi:
    let field = asLocal(currentField)
    callDefault c, field.typ, info
    skip currentField
  c.dest.addParRi()

proc semTupleDefault(c: var SemContext; it: var Item) =
  let exprStart = c.dest.len
  let expected = it.typ
  let info = it.n.info
  inc it.n
  it.typ = semLocalType(c, it.n)
  c.dest.shrink exprStart
  skipParRi it.n
  buildDefaultTuple(c, it.typ, info)
  commonType c, it, exprStart, expected

proc semTupAt(c: var SemContext; it: var Item) =
  # has already been semchecked but we do it again:
  let exprStart = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  var tup = Item(n: it.n, typ: c.types.autoType)
  semExpr c, tup
  if containsGenericParams(tup.typ):
    # leave as is, probably enough to check tup.typ is a typevar
    var index = Item(n: tup.n, typ: c.types.autoType)
    semExpr c, index
    it.n = index.n
    takeParRi c, it.n
    return
  var idx = tup.n
  let idxStart = c.dest.len
  semConstIntExpr c, idx
  var idxValue = evalOrdinal(c, cursorAt(c.dest, idxStart))
  endRead(c.dest)
  it.n = idx
  let zero = createXint(0'i64)
  if idxValue.isNaN or idxValue < zero:
    shrink c.dest, idxStart
    c.buildErr it.n.info, "must be a constant expression >= 0"
    takeParRi c, it.n
  else:
    it.typ = tup.typ
    inc it.typ
    # navigate to the proper type within the tuple type:
    let one = createXint(1'i64)
    while true:
      if it.typ.kind == ParRi:
        shrink c.dest, idxStart
        c.buildErr it.n.info, "tuple index too large"
        break
      if idxValue > zero:
        skip it.typ
        idxValue = idxValue - one
      else:
        break
    if it.typ.substructureKind == FldU:
      let fld = asLocal(it.typ)
      it.typ = fld.typ
    takeParRi c, it.n
    commonType c, it, exprStart, expected

proc getDottedIdent(n: var Cursor): string =
  let isError = n.kind == ParLe and n.tagId == ErrT
  if isError:
    inc n
  if n.kind == ParLe and n == $DotX:
    inc n
    result = getDottedIdent(n)
    let s = getIdent(n)
    if s == StrId(0) or result == "":
      result = ""
    else:
      result.add(pool.strings[s])
    skipParRi n
  else:
    # treat as atom
    let s = getIdent(n)
    if s == StrId(0):
      result = ""
    else:
      result = pool.strings[s]
  if isError:
    skipToEnd n

proc semDefined(c: var SemContext; it: var Item) =
  inc it.n
  let info = it.n.info
  let orig = it.n
  let name = getDottedIdent(it.n)
  skipParRi it.n
  if name == "":
    c.buildErr info, "invalid expression for defined: " & asNimCode(orig), orig
  else:
    let isDefined = name in c.g.config.defines
    let beforeExpr = c.dest.len
    c.dest.addParLe(if isDefined: TrueX else: FalseX, info)
    c.dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, it, beforeExpr, expected

proc isDeclared(c: var SemContext; name: StrId): bool =
  var scope = c.currentScope
  while scope != nil:
    if name in scope.tab:
      return true
    scope = scope.up
  result = name in c.importTab

proc semDeclared(c: var SemContext; it: var Item) =
  inc it.n
  let info = it.n.info
  let orig = it.n
  # XXX maybe always type the argument and check for Symbol/errored Ident instead
  let isError = it.n.kind == ParLe and it.n.tagId == ErrT
  if isError:
    inc it.n
  # does not consider module quoted symbols for now
  let nameId = getIdent(it.n)
  if isError:
    skipToEnd it.n
  skipParRi it.n
  if nameId == StrId(0):
    c.buildErr info, "invalid expression for declared: " & asNimCode(orig), orig
  else:
    let isDeclared = isDeclared(c, nameId)
    let beforeExpr = c.dest.len
    c.dest.addParLe(if isDeclared: TrueX else: FalseX, info)
    c.dest.addParRi()
    let expected = it.typ
    it.typ = c.types.boolType
    commonType c, it, beforeExpr, expected

proc semIsMainModule(c: var SemContext; it: var Item) =
  let info = it.n.info
  inc it.n
  skipParRi it.n
  let isMainModule = IsMain in c.moduleFlags
  let beforeExpr = c.dest.len
  c.dest.addParLe(if isMainModule: TrueX else: FalseX, info)
  c.dest.addParRi()
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, it, beforeExpr, expected

proc tryExplicitRoutineInst(c: var SemContext; syms: Cursor; it: var Item): bool =
  result = false
  let info = syms.info
  let exprStart = c.dest.len
  # build symchoice first so we can directly add the matching syms:
  c.dest.add parLeToken(AtX, info)
  c.dest.add parLeToken(CchoiceX, info)
  var argBuf = createTokenBuf(16)
  swap c.dest, argBuf
  var argRead = it.n
  while argRead.kind != ParRi:
    semLocalTypeImpl c, argRead, AllowValues
  takeParRi c, argRead
  swap c.dest, argBuf
  let args = cursorAt(argBuf, 0)
  var matches = 0
  var lastMatch = default(Match)
  var instLastMatch = false
  var syms = syms
  var nested = 0
  while true:
    # find matching syms
    case syms.kind
    of ParLe:
      if syms.exprKind in {CchoiceX, OchoiceX}:
        inc nested
        inc syms
      else:
        c.dest.shrink exprStart
        c.buildErr syms.info, "invalid tag in symchoice: " & pool.tags[syms.tagId]
        return
    of ParRi:
      dec nested
      inc syms
    of Symbol:
      let sym = syms.symId
      let routine = getProcDecl(sym)
      let candidate = FnCandidate(kind: routine.kind, sym: sym, typ: routine.params)
      var m = createMatch(addr c)
      m.fn = candidate
      matchTypevars m, candidate, args
      if not m.err:
        # match
        c.dest.add symToken(sym, syms.info)
        inc matches
        lastMatch = m
        # mark if routine is suitable for instantiation:
        instLastMatch = routine.kind notin {TemplateY, MacroY} and routine.exported.kind != ParLe
      inc syms
    else:
      c.dest.shrink exprStart
      c.buildErr syms.info, "invalid token in symchoice: " & $syms.kind
      return
    if nested == 0: break
  c.dest.addParRi() # close symchoice
  if matches == 0:
    c.dest.shrink exprStart
    result = false
  elif matches == 1 and c.routine.inGeneric == 0 and instLastMatch:
    # can instantiate single match
    c.dest.shrink exprStart
    # inferred table outlives proc but argsBuf is ephemeral:
    var inferred = lastMatch.inferred
    for _, value in inferred.mpairs:
      c.dest.addSubtree value
      value = typeToCursor(c, exprStart)
      c.dest.shrink exprStart
    let inst = c.requestRoutineInstance(lastMatch.fn.sym, lastMatch.typeArgs, inferred, info)
    c.dest.add symToken(inst.targetSym, info)
    it.typ = asRoutine(inst.procType).params
    it.kind = lastMatch.fn.kind
    it.n = argRead
    result = true
  else:
    # multiple matches, leave as subscript of symchoice
    c.dest.add argBuf
    it.n = argRead
    result = true

proc isSinglePar(n: Cursor): bool =
  var n = n
  inc n
  result = n.kind == ParRi

proc tryBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item): bool =
  # it.n is after lhs, at args
  result = false
  if (lhs.n.kind == Symbol and lhs.kind == TypeY and
        isGeneric(getTypeSection(lhs.n.symId))) or
      (lhs.n.typeKind in InvocableTypeMagics and isSinglePar(lhs.n)):
    # lhs is a generic type symbol, this is a generic invocation
    # treat it as a type expression to call semInvoke
    var typeExpr = createTokenBuf(16)
    typeExpr.addParLe(AtX, lhs.n.info)
    typeExpr.addSubtree lhs.n
    while it.n.kind != ParRi:
      takeTree typeExpr, it.n
    skipParRi it.n
    typeExpr.addParRi()
    var typeItem = Item(n: beginRead(typeExpr), typ: it.typ)
    semLocalTypeExpr c, typeItem
    it.typ = typeItem.typ
    return true
  var maybeRoutine = lhs.n
  if maybeRoutine.exprKind in {OchoiceX, CchoiceX}:
    inc maybeRoutine
  if maybeRoutine.kind == Symbol:
    let res = tryLoadSym(maybeRoutine.symId)
    if res.status == LacksNothing and isRoutine(res.decl.symKind):
      # check for explicit generic routine instantiation
      result = tryExplicitRoutineInst(c, lhs.n, it)
      if result: return

proc semBuiltinSubscript(c: var SemContext; it: var Item; lhs: Item) =
  # it.n is after lhs, at args
  if tryBuiltinSubscript(c, it, lhs):
    return

  # build call:
  var callBuf = createTokenBuf(16)
  callBuf.addParLe(CallX, lhs.n.info)
  callBuf.add identToken(pool.strings.getOrIncl("[]"), lhs.n.info)
  callBuf.addSubtree lhs.n
  while it.n.kind != ParRi:
    callBuf.takeTree it.n
  callBuf.addParRi()
  skipParRi it.n
  var call = Item(n: cursorAt(callBuf, 0), typ: it.typ)
  semCall c, call, {}, SubscriptCall
  it.typ = call.typ

proc semSubscript(c: var SemContext; it: var Item) =
  var n = it.n
  inc n # tag
  var lhsBuf = createTokenBuf(4)
  swap c.dest, lhsBuf
  var lhs = Item(n: n, typ: c.types.autoType)
  semExpr c, lhs, {KeepMagics}
  swap c.dest, lhsBuf
  it.n = lhs.n
  lhs.n = cursorAt(lhsBuf, 0)
  semBuiltinSubscript(c, it, lhs)

proc semTypedAt(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  let lhsInfo = it.n.info
  var lhs = Item(n: it.n, typ: c.types.autoType)
  semExpr c, lhs
  it.n = lhs.n
  var index = Item(n: it.n, typ: c.types.autoType)
  semExpr c, index
  it.n = index.n
  var typ = skipModifier(lhs.typ)
  if typ.typeKind == PtrT:
    inc typ
  case typ.typeKind
  of ArrayT, UncheckedArrayT:
    it.typ = typ
    inc it.typ
  of CstringT:
    it.typ = c.types.charType
  of SetT:
    it.typ = c.types.uint8Type
  else:
    c.buildErr lhsInfo, "invalid lhs type for typed index: " & typeToString(typ)
  takeParRi c, it.n
  commonType c, it, beforeExpr, expected

proc semConv(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var destType = semLocalType(c, it.n)
  var arg = Item(n: it.n, typ: c.types.autoType)
  var argBuf = createTokenBuf(16)
  swap c.dest, argBuf
  semExpr c, arg
  swap c.dest, argBuf
  it.n = arg.n
  arg.n = cursorAt(argBuf, 0)
  semConvArg(c, destType, arg, info)
  takeParRi c, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, it, beforeExpr, expected

proc semDconv(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var destType = semLocalType(c, it.n)
  var x = Item(n: it.n, typ: c.types.autoType)
  let beforeArg = c.dest.len
  semExpr c, x
  it.n = x.n

  var isDistinct = false
  let destBase = skipDistinct(destType, isDistinct)
  let srcBase = skipDistinct(x.typ, isDistinct)
  if not isDistinct:
    shrink c.dest, beforeExpr
    c.buildErr info, "`dconv` operation only valid for type conversions involving `distinct` types"
  else:
    var arg = Item(n: cursorAt(c.dest, beforeArg), typ: srcBase)
    var m = createMatch(addr c)
    typematch m, destBase, arg
    endRead c.dest
    if m.err:
      when defined(debug):
        shrink c.dest, beforeExpr
        c.dest.addErrorMsg m
      else:
        c.typeMismatch info, x.typ, destType
    else:
      # distinct type conversions can also involve conversions
      # between different integer sizes or object types and then
      # `m.args` contains these so use them here:
      shrink c.dest, beforeArg
      c.dest.add m.args
  it.n = x.n
  takeParRi c, it.n
  let expected = it.typ
  it.typ = destType
  commonType c, it, beforeExpr, expected

proc semEnumToStr(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  var x = Item(n: it.n, typ: c.types.autoType)

  var exprTokenBuf = createTokenBuf()
  swap c.dest, exprTokenBuf
  semExpr c, x
  swap c.dest, exprTokenBuf
  it.n = x.n
  let typeSymId = x.typ.symId
  let typeName = pool.syms[typeSymId]
  let dollorName = "dollar`." & typeName
  let dollorSymId = pool.syms.getOrIncl(dollorName)
  shrink c.dest, beforeExpr
  c.dest.add parLeToken(CallX, info)
  c.dest.add symToken(dollorSymId, info)
  c.dest.add exprTokenBuf
  c.dest.addParRi()

  it.typ = c.types.stringType

proc buildLowValue(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr typ.info, "cannot get low value of non-type"
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT:
      # first field
      var field = asEnumDecl(decl.body).firstField
      let first = asLocal(field)
      c.dest.add symToken(first.name.symId, info)
    else:
      c.buildErr info, "invalid type for low: " & typeToString(typ)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: low(int8).int64
        of 16: low(int16).int64
        of 32: low(int32).int64
        else: low(int64)
      c.dest.add intToken(pool.integers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("i" & $rawBits), info)
        c.dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value = 0'u64
      c.dest.add uintToken(pool.uintegers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("u" & $rawBits), info)
        c.dest.addParRi()
    of CharT:
      c.dest.add charToken('\0', info)
    of RangetypeT:
      var first = typ
      inc first
      let base = first
      skip first
      c.dest.addParLe(ConvX, info)
      c.dest.addSubtree base
      c.dest.addSubtree first
      c.dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildLowValue(c, index, info)
    of BoolT:
      c.dest.addParLe(FalseX, info)
      c.dest.addParRi()
    of FloatT:
      c.dest.addParLe(NegInfX, info)
      c.dest.addParRi()
    else:
      c.buildErr info, "invalid type for low: " & typeToString(typ)
  else:
    c.buildErr info, "invalid type for low: " & typeToString(typ)

proc buildHighValue(c: var SemContext; typ: Cursor; info: PackedLineInfo) =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    assert s.status == LacksNothing
    if s.decl.symKind != TypeY:
      c.buildErr typ.info, "cannot get high value of non-type"
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT:
      # last field
      var field = asEnumDecl(decl.body).firstField
      var lastField = field
      while field.kind != ParRi:
        lastField = field
        skip field
      let last = asLocal(lastField)
      c.dest.add symToken(last.name.symId, info)
    else:
      c.buildErr info, "invalid type for high: " & typeToString(typ)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bitsCursor = typ
      inc bitsCursor # skip int tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: high(int8).int64
        of 16: high(int16).int64
        of 32: high(int32).int64
        else: high(int64)
      c.dest.add intToken(pool.integers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("i" & $rawBits), info)
        c.dest.addParRi()
    of UIntT:
      var bitsCursor = typ
      inc bitsCursor # skip uint tag
      let rawBits = typebits(bitsCursor.load)
      var bits = rawBits
      if rawBits != -1:
        c.dest.addParLe(SufX, info)
      else:
        bits = c.g.config.bits
      let value =
        case bits
        of 8: high(uint8).uint64
        of 16: high(uint16).uint64
        of 32: high(uint32).uint64
        else: high(uint64)
      c.dest.add uintToken(pool.uintegers.getOrIncl(value), info)
      if rawBits != -1:
        c.dest.add strToken(pool.strings.getOrIncl("u" & $rawBits), info)
        c.dest.addParRi()
    of CharT:
      c.dest.add charToken(high(char), info)
    of RangetypeT:
      var last = typ
      inc last
      let base = last
      skip last
      skip last
      c.dest.addParLe(ConvX, info)
      c.dest.addSubtree base
      c.dest.addSubtree last
      c.dest.addParRi()
    of ArrayT:
      var index = typ
      inc index # tag
      skip index # element
      buildHighValue(c, index, info)
    of BoolT:
      c.dest.addParLe(TrueX, info)
      c.dest.addParRi()
    of FloatT:
      c.dest.addParLe(InfX, info)
      c.dest.addParRi()
    else:
      c.buildErr info, "invalid type for high: " & typeToString(typ)
  else:
    c.buildErr info, "invalid type for high: " & typeToString(typ)

proc semLow(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  let typ = semLocalType(c, it.n)
  takeParRi c, it.n
  if containsGenericParams(typ):
    discard
  else:
    c.dest.shrink beforeExpr
    buildLowValue(c, typ, info)
  let expected = it.typ
  var resultType = typ
  if resultType.typeKind == ArrayT:
    inc resultType # skip tag, get to range type
    skip resultType # skip element type, get to range type
    if resultType.typeKind == RangetypeT:
      inc resultType # skip range tag, get to base type
  elif resultType.typeKind == RangetypeT:
    inc resultType # skip tag, get to base type
  it.typ = resultType
  commonType c, it, beforeExpr, expected

proc semHigh(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  takeToken c, it.n
  let typ = semLocalType(c, it.n)
  takeParRi c, it.n
  if containsGenericParams(typ):
    discard
  else:
    c.dest.shrink beforeExpr
    buildHighValue(c, typ, info)
  let expected = it.typ
  var resultType = typ
  if resultType.typeKind == ArrayT:
    inc resultType # skip tag
    skip resultType # skip element type, get to range type
    if resultType.typeKind == RangetypeT:
      inc resultType # skip range tag, get to base type
  elif resultType.typeKind == RangetypeT:
    inc resultType # skip tag, get to base type
  it.typ = resultType
  commonType c, it, beforeExpr, expected

proc semVoidHook(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  it.typ = c.types.autoType
  semExpr c, it
  if it.n.kind != ParRi:
    # hook has 2nd argument:
    it.typ = c.types.autoType
    semExpr c, it
  takeParRi c, it.n
  it.typ = c.types.voidType
  commonType c, it, beforeExpr, expected

proc semDupHook(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  takeToken c, it.n
  it.typ = c.types.autoType
  semExpr c, it
  takeParRi c, it.n
  it.typ = skipModifier(it.typ)
  commonType c, it, beforeExpr, expected

proc semDeref(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let info = it.n.info
  let expected = it.typ
  takeToken c, it.n
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, arg
  it.n = arg.n
  takeParRi c, it.n
  case arg.typ.typeKind
  of RefT, PtrT:
    it.typ = arg.typ
    inc it.typ # get to base type
  else:
    c.buildErr info, "invalid type for deref: " & typeToString(arg.typ)
  commonType c, it, beforeExpr, expected

proc semAddr(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let info = it.n.info
  let expected = it.typ
  let beforeArg = c.dest.len
  var arg = Item(n: it.n, typ: c.types.autoType)
  semExpr c, arg
  it.n = arg.n
  takeParRi c, it.n
  let a = cursorAt(c.dest, beforeArg)
  if isAddressable(a):
    endRead c.dest
  else:
    let asStr = asNimCode(a)
    endRead c.dest
    c.dest.shrink beforeArg
    c.buildErr info, "invalid expression for `addr` operation: " & asStr
    c.dest.addParRi()

  it.typ = ptrTypeOf(c, arg.typ)
  commonType c, it, beforeExpr, expected

proc semSizeof(c: var SemContext; it: var Item) =
  let beforeExpr = c.dest.len
  let expected = it.typ
  # We don't really have any kind of restrictions on the argument here
  # and it was semchecked already in overload resolution, so it is fine
  # to just copy it:
  takeTree c, it.n
  it.typ = c.types.intType
  commonType c, it, beforeExpr, expected

proc whichPass(c: SemContext): PassKind =
  result = if c.phase == SemcheckSignatures: checkSignatures else: checkBody

template toplevelGuard(c: var SemContext; body: untyped) =
  if c.phase == SemcheckBodies:
    body
  else:
    c.takeTree it.n

template procGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    c.takeTree it.n

template constGuard(c: var SemContext; body: untyped) =
  if c.phase in {SemcheckSignatures, SemcheckBodies}:
    body
  else:
    c.takeTree it.n

proc semPragmaLine(c: var SemContext; it: var Item; info: PackedLineInfo) =
  inc it.n
  case it.n.pragmaKind:
  of BuildP:
    inc it.n
    var args = newSeq[string]()
    while it.n.kind != ParRi:
      if it.n.kind != StringLit:
        buildErr c, it.n.info, "expected `string` but got: " & asNimCode(it.n)

      args.add pool.strings[it.n.litId]
      inc it.n

    skipParRi it.n

    if args.len != 2 and args.len != 3:
      buildErr c, it.n.info, "build expected 2 or 3 parameters"

    # XXX: makefile is executed parent to nifcachePath
    let nifcacheDir = absoluteParentDir(c.g.config.nifcachePath)
    let currentDir = absoluteParentDir(info.getFile)

    # Extract build pragma arguments
    let compileType = args[0]
    var name = replaceSubs(args[1], currentDir, c.g.config).toAbsolutePath(currentDir)
    let customArgs = if args.len == 3: replaceSubs(args[2], currentDir, c.g.config) else: ""

    if not fileExists2(name):
      buildErr c, it.n.info, "cannot find: " & name
    name = name.toRelativePath(nifcacheDir)

    c.toBuild.buildTree TupleConstrX, info:
      c.toBuild.addStrLit compileType, info
      c.toBuild.addStrLit name, info
      c.toBuild.addStrLit customArgs, info
  of EmitP:
    semEmit c, it
  else:
    buildErr c, it.n.info, "unsupported pragmas"

proc semPragmasLine(c: var SemContext; it: var Item) =
  let info = it.n.info
  inc it.n
  while it.n.kind == ParLe and (it.n.stmtKind in {CallS, CmdS} or
            it.n.substructureKind == KvU):
    semPragmaLine c, it, info

  skipParRi it.n

proc semInclExcl(c: var SemContext; it: var Item) =
  let info = it.n.info
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, op
  if op.typ.typeKind == SetT:
    inc op.typ
  else:
    c.buildErr info, "expected set type"
  semExpr c, op
  it.n = op.n
  takeParRi c, it.n
  producesVoid c, info, it.typ

proc semInSet(c: var SemContext; it: var Item) =
  let info = it.n.info
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, op
  if op.typ.typeKind == SetT:
    inc op.typ
  else:
    c.buildErr info, "expected set type"
  semExpr c, op
  it.n = op.n
  takeParRi c, it.n
  let expected = it.typ
  it.typ = c.types.boolType
  commonType c, it, beforeExpr, expected

proc semCardSet(c: var SemContext; it: var Item) =
  let info = it.n.info
  let beforeExpr = c.dest.len
  takeToken c, it.n
  let typeStart = c.dest.len
  semLocalTypeImpl c, it.n, InLocalDecl
  let typ = typeToCursor(c, typeStart)
  var op = Item(n: it.n, typ: typ)
  semExpr c, op
  it.n = op.n
  takeParRi c, it.n
  let expected = it.typ
  it.typ = c.types.intType
  commonType c, it, beforeExpr, expected

proc semExpr(c: var SemContext; it: var Item; flags: set[SemFlag] = {}) =
  case it.n.kind
  of IntLit:
    literal c, it, c.types.intType
  of UIntLit:
    literal c, it, c.types.uintType
  of FloatLit:
    literal c, it, c.types.floatType
  of StringLit:
    literal c, it, c.types.stringType
  of CharLit:
    literal c, it, c.types.charType
  of Ident:
    let start = c.dest.len
    let s = semIdent(c, it.n, flags)
    semExprSym c, it, s, start, flags
  of Symbol:
    let start = c.dest.len
    let s = fetchSym(c, it.n.symId)
    takeToken c, it.n
    semExprSym c, it, s, start, flags
  of ParLe:
    case exprKind(it.n)
    of QuotedX:
      let start = c.dest.len
      let s = semQuoted(c, it.n, flags)
      semExprSym c, it, s, start, flags
    of NoExpr:
      case stmtKind(it.n)
      of NoStmt:
        case typeKind(it.n)
        of NoType:
          buildErr c, it.n.info, "expression expected; tag: " & pool.tags[it.n.tag]
          skip it.n
        of ErrT:
          c.takeTree it.n
        of ObjectT, RefobjT, PtrobjT, EnumT, HoleyEnumT, DistinctT, ConceptT:
          buildErr c, it.n.info, "expression expected"
          skip it.n
        of IntT, FloatT, CharT, BoolT, UIntT, VoidT, NiltT, AutoT, SymKindT,
            PtrT, RefT, MutT, OutT, LentT, SinkT, UncheckedArrayT, SetT, StaticT, TypedescT,
            TupleT, ArrayT, RangetypeT, VarargsT, ProctypeT, IteratorT, UntypedT, TypedT,
            CstringT, PointerT, TypeKindT, OrdinalT, OpenArrayT, ParamsT, ItertypeT:
          # every valid local type expression
          semLocalTypeExpr c, it
        of OrT, AndT, NotT, InvokeT:
          # should be handled in respective expression kinds
          discard
      of ImportasS, ExportexceptS, UnpackdeclS, StaticstmtS, BindS, MixinS, UsingS, AsmS, DeferS:
        buildErr c, it.n.info, "unsupported statement: " & $stmtKind(it.n)
        skip it.n
      of ProcS:
        procGuard c:
          semProc c, it, ProcY, whichPass(c)
      of FuncS:
        procGuard c:
          semProc c, it, FuncY, whichPass(c)
      of IteratorS:
        procGuard c:
          semProc c, it, IteratorY, whichPass(c)
      of ConverterS:
        procGuard c:
          semProc c, it, ConverterY, whichPass(c)
      of MethodS:
        procGuard c:
          semProc c, it, MethodY, whichPass(c)
      of TemplateS:
        procGuard c:
          semProc c, it, TemplateY, whichPass(c)
      of MacroS:
        procGuard c:
          semProc c, it, MacroY, whichPass(c)
      of WhileS:
        toplevelGuard c:
          semWhile c, it
      of VarS:
        toplevelGuard c:
          semLocal c, it, VarY
      of LetS:
        toplevelGuard c:
          semLocal c, it, LetY
      of CursorS:
        toplevelGuard c:
          semLocal c, it, CursorY
      of ResultS:
        toplevelGuard c:
          semLocal c, it, ResultY
      of ConstS:
        constGuard c:
          semLocal c, it, ConstY
      of StmtsS: semStmtsExpr c, it, false
      of ScopeS: semStmtsExpr c, it, true
      of BreakS:
        toplevelGuard c:
          semBreak c, it
      of ContinueS:
        toplevelGuard c:
          semContinue c, it
      of CallS, CmdS:
        toplevelGuard c:
          semCall c, it, flags
      of IncludeS: semInclude c, it
      of ImportS: semImport c, it
      of ImportExceptS: semImportExcept c, it
      of FromS: semFromImport c, it
      of AsgnS:
        toplevelGuard c:
          semAsgn c, it
      of DiscardS:
        toplevelGuard c:
          semDiscard c, it
      of IfS:
        toplevelGuard c:
          semIf c, it
      of WhenS:
        toplevelGuard c:
          semWhen c, it
      of RetS:
        toplevelGuard c:
          semReturn c, it
      of YldS:
        toplevelGuard c:
          semYield c, it
      of TypeS:
        let info = it.n.info
        semTypeSection c, it.n
        producesVoid c, info, it.typ
      of BlockS:
        toplevelGuard c:
          semBlock c, it
      of CaseS:
        toplevelGuard c:
          semCase c, it
      of ForS:
        toplevelGuard c:
          semFor c, it
      of TryS:
        toplevelGuard c:
          semTry c, it
      of RaiseS:
        toplevelGuard c:
          semRaise c, it
      of ExportS, CommentS:
        # XXX ignored for now
        skip it.n
      of EmitS, GvarS, TvarS, GletS, TletS:
        raiseAssert "unreachable"
      of PragmasS:
        toplevelGuard c:
          semPragmasLine c, it
      of InclS, ExclS:
        toplevelGuard c:
          semInclExcl c, it
    of FalseX, TrueX:
      literalB c, it, c.types.boolType
    of InfX, NegInfX, NanX:
      literalB c, it, c.types.floatType
    of AndX, OrX:
      let start = c.dest.len
      takeToken c, it.n
      semBoolExpr c, it.n
      semBoolExpr c, it.n
      takeParRi c, it.n
      let expected = it.typ
      it.typ = c.types.boolType
      commonType c, it, start, expected
    of NotX:
      let start = c.dest.len
      takeToken c, it.n
      semBoolExpr c, it.n
      takeParRi c, it.n
      let expected = it.typ
      it.typ = c.types.boolType
      commonType c, it, start, expected
    of EmoveX:
      takeToken c, it.n
      semExpr c, it
      takeParRi c, it.n
    of ParX:
      inc it.n
      semExpr c, it
      skipParRi it.n
    of CallX, CmdX, CallStrLitX, InfixX, PrefixX, HcallX:
      toplevelGuard c:
        semCall c, it, flags
    of DotX, DdotX:
      toplevelGuard c:
        semDot c, it, flags
    of TupAtX:
      toplevelGuard c:
        semTupAt c, it
    of DconvX:
      toplevelGuard c:
        semDconv c, it
    of EqX, NeqX, LeX, LtX, EqSetX, LeSetX, LtSetX:
      semCmp c, it
    of AshrX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX, PlusSetX, MinusSetX, MulSetX, XorSetX:
      semTypedBinaryArithmetic c, it
    of BitnotX, NegX:
      semTypedUnaryArithmetic c, it
    of InSetX:
      semInSet c, it
    of CardX:
      semCardSet c, it
    of BracketX:
      semBracket c, it, flags
    of CurlyX:
      semCurly c, it, flags
    of AconstrX:
      semArrayConstr c, it
    of SetConstrX:
      semSetConstr c, it
    of SufX:
      semSuf c, it
    of TupleConstrX:
      semTupleConstr c, it
    of OconstrX, NewOconstrX:
      semObjConstr c, it
    of DefinedX:
      semDefined c, it
    of DeclaredX:
      semDeclared c, it
    of IsMainModuleX:
      semIsMainModule c, it
    of AtX:
      semSubscript c, it
    of ArrAtX, PatX:
      semTypedAt c, it
    of UnpackX:
      takeToken c, it.n
      takeParRi c, it.n
    of OchoiceX, CchoiceX:
      takeTree c, it.n
    of HaddrX, HderefX:
      takeToken c, it.n
      # this is exactly what we need here as these operators have the same
      # type as the operand:
      semExpr c, it
      takeParRi c, it.n
    of CastX:
      semCast c, it
    of NilX:
      semNil c, it
    of ConvX, HconvX:
      semConv c, it
    of EnumToStrX:
      semEnumToStr c, it
    of DefaultObjX:
      semObjDefault c, it
    of DefaultTupX:
      semTupleDefault c, it
    of LowX:
      semLow c, it
    of HighX:
      semHigh c, it
    of ExprX:
      semStmtsExpr c, it, false
    of DerefX:
      semDeref c, it
    of AddrX:
      semAddr c, it
    of SizeofX:
      semSizeof c, it
    of TypeofX:
      semTypeof c, it
    of DestroyX, CopyX, WasMovedX, SinkhX, TraceX:
      semVoidHook c, it
    of DupX:
      semDupHook c, it
    of ErrX:
      takeTree c, it.n
    of OconvX, PragmaxX, CurlyatX, TabconstrX, DoX,
       CompilesX, AlignofX, OffsetofX:
      # XXX To implement
      buildErr c, it.n.info, "to implement: " & $exprKind(it.n)
      takeToken c, it.n
      takeParRi c, it.n

  of ParRi, EofToken, SymbolDef, UnknownToken, DotToken:
    buildErr c, it.n.info, "expression expected"

proc reportErrors(c: var SemContext): int =
  let errTag = pool.tags.getOrIncl("err")
  var i = 0
  var r = Reporter(verbosity: 2, noColors: not useColors())
  result = 0
  while i < c.dest.len:
    if c.dest[i].kind == ParLe and c.dest[i].tagId == errTag:
      inc result
      let info = c.dest[i].info
      inc i
      # original expression, optional:
      if c.dest[i].kind == DotToken:
        inc i
      else:
        let x = cursorAt(c.dest, i)
        inc i, span(x)
        endRead(c.dest)
      # instantiation contexts:
      while c.dest[i].kind == DotToken:
        r.trace infoToStr(c.dest[i].info), "instantiation from here"
        inc i
      # error message:
      assert c.dest[i].kind == StringLit
      r.error infoToStr(info), pool.strings[c.dest[i].litId]
      inc i
    else:
      inc i

proc writeOutput(c: var SemContext; outfile: string) =
  #var b = nifbuilder.open(outfile)
  #b.addHeader "nimony", "nim-sem"
  #b.addRaw toString(c.dest)
  #b.close()
  writeFile outfile, "(.nif24)\n" & toString(c.dest)
  createIndex outfile, true, IndexSections(hooks: c.hookIndexMap, converters: c.converterIndexMap, toBuild: c.toBuild)

proc phaseX(c: var SemContext; n: Cursor; x: SemPhase): TokenBuf =
  assert n == "stmts"
  c.phase = x
  var n = n
  takeToken c, n
  while n.kind != ParRi:
    semStmt c, n, false
  takeParRi c, n
  result = move c.dest

proc requestHookInstance(c: var SemContext; decl: Cursor) =
  let decl = asTypeDecl(decl)
  var typevars = decl.typevars
  assert classifyType(c, typevars) == InvokeT
  inc typevars
  assert typevars.kind == Symbol

  let symId = typevars.symId
  if symId in c.genericHooks:
    var inferred = initTable[SymId, Cursor]()
    var typeArgs = createTokenBuf()
    let originHooks = c.genericHooks[symId]

    inc typevars # skips symbol

    var typevarsSeq: seq[Cursor] = @[]

    while typevars.kind != ParRi:
      typevarsSeq.add typevars
      takeTree(typeArgs, typevars)

    for hook in originHooks:
      let res = tryLoadSym(hook)
      if res.status == LacksNothing:
        let info = res.decl.info
        let procDecl = asRoutine(res.decl)
        var typevarsStart = procDecl.typevars
        inc typevarsStart # skips typevars tag

        var counter = 0
        while typevarsStart.kind != ParRi:
          let name = asTypevar(typevarsStart).name.symId
          inferred[name] = typevarsSeq[counter]
          skip typevarsStart # skip the typevar tree
          inc counter
        discard requestRoutineInstance(c, hook, typeArgs, inferred, info)

proc semcheck*(infile, outfile: string; config: sink NifConfig; moduleFlags: set[ModuleFlag];
               commandLineArgs: sink string; canSelfExec: bool) =
  var n0 = setupProgram(infile, outfile)
  var c = SemContext(
    dest: createTokenBuf(),
    types: createBuiltinTypes(),
    thisModuleSuffix: prog.main,
    moduleFlags: moduleFlags,
    g: ProgramContext(config: config),
    phase: SemcheckTopLevelSyms,
    routine: SemRoutine(kind: NoSym),
    commandLineArgs: commandLineArgs,
    canSelfExec: canSelfExec)
  for magic in ["typeof", "compiles"]:
    c.unoverloadableMagics.incl(pool.strings.getOrIncl(magic))
  c.currentScope = Scope(tab: initTable[StrId, seq[Sym]](), up: nil, kind: ToplevelScope)
  # XXX could add self module symbol here

  assert n0 == "stmts"

  if {SkipSystem, IsSystem} * moduleFlags == {}:
    importSingleFile(c, ImportedFilename(path: stdlibFile("std/system"), name: "system"),
       "", ImportMode(kind: ImportSystem, list: initPackedSet[StrId]()), n0.info)

  #echo "PHASE 1"
  var n1 = phaseX(c, n0, SemcheckTopLevelSyms)
  #echo "PHASE 2: ", toString(n1)
  var n2 = phaseX(c, beginRead(n1), SemcheckSignatures)

  #echo "PHASE 3: ", toString(n2)
  var n = beginRead(n2)
  c.phase = SemcheckBodies
  takeToken c, n
  while n.kind != ParRi:
    semStmt c, n, false
  instantiateGenerics c
  for _, val in mpairs(c.instantiatedTypes):
    let s = fetchSym(c, val)
    let res = declToCursor(c, s)
    if res.status == LacksNothing:
      requestHookInstance(c, res.decl)
      c.dest.copyTree res.decl
  instantiateGenericHooks c
  takeParRi c, n

  if reportErrors(c) == 0:
    var final = move c.dest
    var finalBuf = beginRead(final)
    c.dest = injectDerefs(finalBuf)
  else:
    quit 1

  if reportErrors(c) == 0:
    writeOutput c, outfile
  else:
    quit 1

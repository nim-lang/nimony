# included in sem.nim

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
        var buf = parseFile(f2, c.g.config.paths, c.g.config.nifcachePath)
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

proc importSingleFile(c: var SemContext; f1: ImportedFilename; origin: string;
                      mode: ImportFilter; exports: var seq[(string, ImportFilter)];
                      info: PackedLineInfo): SymId =
  let f2 = resolveFile(c.g.config.paths, origin, f1.path)
  if not fileExists(f2):
    c.buildErr info, "file not found: " & f2
    return
  let suffix = moduleSuffix(f2, c.g.config.paths)
  if c.ignoreErr:
    let indexFile = suffix.suffixToNif.changeFileExt".idx.nif"
    if isEmptyFile indexFile:
      # Importing an invalid module.
      # Need to write `.2.deps.nif` file so that the invalid module is semchecked in 2nd frontend Makefile.
      var deps = createTokenBuf(8)
      deps.buildTree StmtsS, NoLineInfo:
        deps.buildTree ImportS, NoLineInfo:
          deps.addStrLit f2.toAbsolutePath
      let depsFile = changeFileExt(c.outfile, ".deps.nif")
      writeFile depsFile, "(.nif24)\n" & toString(deps)
      quit 1
  result = SymId(0)
  if not c.processedModules.contains(suffix):
    c.meta.importedFiles.add f2
    if c.canSelfExec and needsRecompile(f2, suffixToNif suffix):
      selfExec c, f2, (if f1.isSystem: " --isSystem" else: "")

    let moduleName = pool.strings.getOrIncl(f1.name)
    result = identToSym(c, moduleName, ModuleY)
    c.processedModules[suffix] = result
    let s = Sym(kind: ModuleY, name: result, pos: ImportedPos)
    if f1.name != "":
      c.currentScope.addOverloadable(moduleName, s)
    var moduleDecl = createTokenBuf(2)
    moduleDecl.addParLe(ModuleY, info)
    moduleDecl.addParRi()
    publish result, moduleDecl
  else:
    result = c.processedModules[suffix]
  let module = addr c.importedModules.mgetOrPut(result, ImportedModule(path: f2))
  loadInterface suffix, module.iface, result, c.importTab, c.converters, c.methods, exports, mode

proc importSingleFile(c: var SemContext; f1: ImportedFilename; origin: string;
                      filter: ImportFilter;
                      info: PackedLineInfo): SymId =
  var exports: seq[(string, ImportFilter)] = @[] # ignored
  importSingleFile(c, f1, origin, filter, exports, info)

proc importSingleFileConsiderExports(c: var SemContext; f1: ImportedFilename; origin: string; filter: ImportFilter; info: PackedLineInfo) =
  var exports: seq[(string, ImportFilter)] = @[]
  let source = importSingleFile(c, f1, origin, filter, exports, info)
  while exports.len != 0:
    var newExports: seq[(string, ImportFilter)] = @[]
    for ex in exports:
      let file = ImportedFilename(path: ex[0], name: "")
      let forward = importSingleFile(c, file, origin, ex[1], newExports, info)
      if forward in c.importedModules[source].exports:
        mergeFilter(c.importedModules[source].exports[forward], ex[1])
      else:
        c.importedModules[source].exports[forward] = ex[1]
    exports = newExports

proc cyclicImport(c: var SemContext; x: var Cursor) =
  c.buildErr x.info, "cyclic module imports are not implemented"

proc doImports(c: var SemContext; files: seq[ImportedFilename]; mode: ImportFilter; info: PackedLineInfo) =
  let origin = getFile(info)
  for f in files:
    importSingleFileConsiderExports c, f, origin, mode, info

proc semImport(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `import`

  if x.kind == ParLe and x.exprKind == PragmaxX:
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
    doImports c, files, ImportFilter(kind: ImportAll), info

  producesVoid c, info, it.typ

proc semImportExcept(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `importexcept`

  if x.kind == ParLe and x.exprKind == PragmaxX:
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
    var excluded = initHashSet[StrId]()
    while x.kind != ParRi:
      excluded.incl takeIdent(x)
    doImports c, files, ImportFilter(kind: ImportExcept, list: excluded), info

  producesVoid c, info, it.typ

proc semFromImport(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `from`

  if x.kind == ParLe and x.exprKind == PragmaxX:
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
    var included = initHashSet[StrId]()
    while x.kind != ParRi:
      if x.kind == ParLe and x.exprKind == NilX:
        # from a import nil
        discard
      else:
        included.incl takeIdent(x)
    doImports c, files, ImportFilter(kind: FromImport, list: included), info

  producesVoid c, info, it.typ

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

proc semExportSymbol(c: var SemContext; n: var Cursor) =
  let info = n.info
  if n.exprKind == DotX:
    var it = Item(n: n, typ: c.types.autoType)
    semExpr c, it
  else:
    let ident = takeIdent(n)
    if ident == StrId(0):
      c.buildErr info, "not an identifier"
      return
    discard buildSymChoice(c, ident, info, FindAll)

proc doExport(c: var SemContext; sym: SymId; info: PackedLineInfo) =
  let res = tryLoadSym(sym)
  let isModule = res.status == LacksNothing and res.decl.symKind == ModuleY
  if isModule:
    # overrides previous export mode if exists
    c.exports[sym] = ImportFilter(kind: ImportAll)
  else:
    let name = pool.syms[sym]
    let suffix = extractModule(name)
    if suffix == "":
      c.buildErr info, "cannot export non-global symbol"
      return
    elif suffix == c.thisModuleSuffix:
      # XXX
      c.buildErr info, "exporting local symbol not implemented"
      return
    let moduleSym = c.processedModules[suffix]
    var basename = ensureMove name
    extractBasename(basename)
    let strId = pool.strings.getOrIncl(basename)
    if moduleSym in c.exports:
      case c.exports[moduleSym].kind
      of ImportAll:
        # nothing to do, already exported
        discard
      of FromImport:
        c.exports[moduleSym].list.incl strId
      of ImportExcept:
        c.exports[moduleSym].list.excl strId
    else:
      c.exports[moduleSym] = ImportFilter(kind: FromImport, list: initHashSet[StrId]())
      c.exports[moduleSym].list.incl strId

proc semExport(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `export`

  while x.kind != ParRi:
    let info = x.info
    var symBuf = createTokenBuf(8)
    swap c.dest, symBuf
    semExportSymbol(c, x)
    swap c.dest, symBuf
    var syms = beginRead(symBuf)
    case syms.kind
    of Ident:
      c.buildErr info, "undeclared identifier"
    of Symbol:
      doExport(c, syms.symId, info)
    of ParLe:
      case syms.exprKind
      of ErrX:
        c.dest.add symBuf
      of OchoiceX, CchoiceX:
        inc syms
        while syms.kind != ParRi:
          assert syms.kind == Symbol
          doExport(c, syms.symId, info)
          inc syms
      else:
        c.buildErr info, "not an identifier for `export`"
    else:
      c.buildErr info, "not an identifier for `export`"

  producesVoid c, info, it.typ

proc doExportExcept(c: var SemContext; moduleSym, sym: SymId; info: PackedLineInfo) =
  let name = pool.syms[sym]
  let suffix = extractModule(name)
  if c.processedModules[suffix] != moduleSym:
    # doesn't belong to exported module, no need to consider
    return
  var basename = ensureMove name
  extractBasename(basename)
  let strId = pool.strings.getOrIncl(basename)
  if moduleSym in c.exports:
    case c.exports[moduleSym].kind
    of ImportAll:
      c.exports[moduleSym] = ImportFilter(kind: ImportExcept, list: initHashSet[StrId]())
      c.exports[moduleSym].list.incl strId
    of FromImport:
      c.exports[moduleSym].list.excl strId
    of ImportExcept:
      c.exports[moduleSym].list.incl strId
  else:
    c.exports[moduleSym] = ImportFilter(kind: ImportExcept, list: initHashSet[StrId]())
    c.exports[moduleSym].list.incl strId

proc semExportExcept(c: var SemContext; it: var Item) =
  let info = it.n.info
  var x = it.n
  skip it.n
  inc x # skip the `exportexcept`

  let moduleSymStart = c.dest.len
  var m = Item(n: x, typ: c.types.autoType)
  semExpr c, m # get module sym
  x = m.n
  let moduleSym = findModuleSymbol(cursorAt(c.dest, moduleSymStart))
  endRead(c.dest)
  c.dest.shrink moduleSymStart
  if moduleSym == SymId(0):
    c.buildErr info, "expected module for `export except`"
    return

  while x.kind != ParRi:
    let info = x.info
    var symBuf = createTokenBuf(8)
    swap c.dest, symBuf
    semExportSymbol(c, x)
    swap c.dest, symBuf
    var syms = beginRead(symBuf)
    case syms.kind
    of Ident:
      c.buildErr info, "undeclared identifier"
    of Symbol:
      doExportExcept(c, moduleSym, syms.symId, info)
    of ParLe:
      case syms.exprKind
      of ErrX:
        c.dest.add symBuf
      of OchoiceX, CchoiceX:
        inc syms
        while syms.kind != ParRi:
          assert syms.kind == Symbol
          doExportExcept(c, moduleSym, syms.symId, info)
          inc syms
      else:
        c.buildErr info, "not an identifier for `export`"
    else:
      c.buildErr info, "not an identifier for `export`"

  producesVoid c, info, it.typ

#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Dependency analysis for Nimony.

#[

- Build the graph. Every node is a list of files representing the main source file plus its included files.
  - for this we also need the config so that the paths can be resolved properly
- Every node also has a list of dependencies. Every single dependency is a dependency to a modules's interface!

]#

import std/[os, tables, sets, syncio, assertions, strutils, times]
import semos, nifconfig, nimony_model, nifindexes
import ".." / gear2 / modnames, semdata

include nifprelude

type
  FilePair = object
    nimFile: string
    modname: string

proc indexFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".2.idx.nif"
proc parsedFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".1.nif"
proc depsFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".1.deps.nif"
proc deps2File(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".2.deps.nif"
proc semmedFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".2.nif"
proc nifcFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".c.nif"
proc cFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".c"
proc objFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".o"

# It turned out to be too annoying in practice to have the exe file in
# the current directory per default so we now put it into the nifcache too:
proc exeFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname.addFileExt ExeExt

proc resolveFileWrapper(paths: openArray[string]; origin: string; toResolve: string): string =
  result = resolveFile(paths, origin, toResolve)
  if not semos.fileExists(result) and toResolve.startsWith("std/"):
    result = resolveFile(paths, origin, toResolve.substr(4))

type
  Node = ref object
    files: seq[FilePair]
    deps: seq[FilePair]
    id, parent: int
    active: int
    isSystem: bool
    plugin: string

  Command* = enum
    DoCheck, # like `nim check`
    DoTranslate, # translate to C like "nim --compileOnly"
    DoCompile, # like `nim c` but with nifler
    DoRun # like `nim run`

  DepContext = object
    forceRebuild: bool
    cmd: Command
    nifler, nimsem: string
    config: NifConfig
    nodes: seq[Node]
    rootNode: Node
    includeStack: seq[string]
    processedModules: HashSet[string]
    moduleFlags: set[ModuleFlag]
    isGeneratingFinal: bool
    foundPlugins: HashSet[string]

proc toPair(c: DepContext; f: string): FilePair =
  FilePair(nimFile: f, modname: moduleSuffix(f, c.config.paths))

proc processDep(c: var DepContext; n: var Cursor; current: Node)
proc parseDeps(c: var DepContext; p: FilePair; current: Node)

proc processInclude(c: var DepContext; it: var Cursor; current: Node) =
  var files: seq[ImportedFilename] = @[]
  var x = it
  skip it
  inc x # skip the `include`
  while x.kind != ParRi:
    var hasError = false
    filenameVal(x, files, hasError, allowAs = false)

    if hasError:
      discard "ignore wrong `include` statement"
    else:
      for f1 in items(files):
        if f1.plugin.len > 0:
          discard "ignore plugin include file, will cause an error in sem.nim"
          continue
        let f2 = resolveFileWrapper(c.config.paths, current.files[current.active].nimFile, f1.path)
        # check for recursive include files:
        var isRecursive = false
        for a in c.includeStack:
          if a == f2:
            isRecursive = true
            break

        if not isRecursive and semos.fileExists(f2):
          let oldActive = current.active
          current.active = current.files.len
          current.files.add c.toPair(f2)
          parseDeps(c, c.toPair(f2), current)
          c.includeStack.add f2
          current.active = oldActive
          c.includeStack.setLen c.includeStack.len - 1
        else:
          discard "ignore recursive include"

proc wouldCreateCycle(c: var DepContext; current: Node; p: FilePair): bool =
  var it = current.id
  while it != -1:
    if c.nodes[it].files[0].modname == p.modname:
      return true
    it = c.nodes[it].parent
  return false

proc importSingleFile(c: var DepContext; f1: string; info: PackedLineInfo;
                      current: Node; isSystem: bool) =
  let f2 = resolveFileWrapper(c.config.paths, current.files[current.active].nimFile, f1)
  if not semos.fileExists(f2): return
  let p = c.toPair(f2)
  if not c.processedModules.containsOrIncl(p.modname):
    current.deps.add p
    var imported = Node(files: @[p], id: c.nodes.len, parent: current.id, isSystem: isSystem)
    c.nodes.add imported
    parseDeps c, p, imported
  else:
    # add the dependency anyway unless it creates a cycle:
    if wouldCreateCycle(c, current, p):
      discard "ignore cycle"
      echo "cycle detected: ", current.files[0].nimFile, " <-> ", p.nimFile
    else:
      current.deps.add p

proc processPluginImport(c: var DepContext; f: ImportedFilename; info: PackedLineInfo; current: Node) =
  let f2 = resolveFileWrapper(c.config.paths, current.files[current.active].nimFile, f.path)
  if not semos.fileExists(f2): return
  let p = c.toPair(f2)
  if not c.processedModules.containsOrIncl(p.modname):
    current.deps.add p
    c.nodes.add Node(files: @[p], id: c.nodes.len,
                     parent: current.id, isSystem: false, plugin: f.plugin)
    c.foundPlugins.incl f.plugin

proc processImport(c: var DepContext; it: var Cursor; current: Node) =
  let info = it.info
  var x = it
  skip it
  inc x # skip the `import`
  # ignore conditional imports:
  if x.stmtKind == WhenS: return
  while x.kind != ParRi:
    if x.kind == ParLe and x.exprKind == PragmaxX:
      inc x
      var y = x
      skip y
      if y.substructureKind == PragmasU:
        inc y
        if y.kind == Ident and pool.strings[y.litId] == "cyclic":
          continue

    var files: seq[ImportedFilename] = @[]
    var hasError = false
    filenameVal(x, files, hasError, allowAs = true)
    if hasError:
      discard "ignore wrong `import` statement"
    else:
      for f in files:
        if f.plugin.len == 0:
          importSingleFile c, f.path, info, current, false
        else:
          processPluginImport c, f, info, current

proc processSingleImport(c: var DepContext; it: var Cursor; current: Node) =
  # process `from import` and `import except` which have a single module expression
  let info = it.info
  var x = it
  skip it
  inc x # skip the tag
  # ignore conditional imports:
  if x.stmtKind == WhenS: return
  var files: seq[ImportedFilename] = @[]
  var hasError = false
  filenameVal(x, files, hasError, allowAs = true)
  if hasError:
    discard "ignore wrong `from` statement"
  else:
    for f in files:
      if f.plugin.len == 0:
        importSingleFile c, f.path, info, current, false
      else:
        processPluginImport c, f, info, current
      break

proc processDep(c: var DepContext; n: var Cursor; current: Node) =
  case stmtKind(n)
  of ImportS:
    processImport c, n, current
  of IncludeS:
    assert not c.isGeneratingFinal
    processInclude c, n, current
  of FromimportS, ImportexceptS:
    processSingleImport c, n, current
  of ExportS:
    discard "ignore `export` statement"
    skip n
  else:
    #echo "IGNORING ", toString(n, false)
    skip n

proc processDeps(c: var DepContext; n: Cursor; current: Node) =
  var n = n
  if n.kind == ParLe and pool.tags[n.tagId] == "stmts":
    inc n
    while n.kind != ParRi:
      processDep c, n, current

proc execNifler(c: var DepContext; f: FilePair) =
  let output = c.config.parsedFile(f)
  let depsFile = c.config.depsFile(f)
  if not c.forceRebuild and semos.fileExists(output) and
      semos.fileExists(f.nimFile) and getLastModificationTime(output) > getLastModificationTime(f.nimFile) and
      semos.fileExists(depsFile) and getLastModificationTime(depsFile) > getLastModificationTime(f.nimFile):
    discard "nothing to do"
  else:
    let cmd = quoteShell(c.nifler) & " --portablePaths --deps parse " & quoteShell(f.nimFile) & " " &
      quoteShell(output)
    exec cmd

proc importSystem(c: var DepContext; current: Node) =
  let p = c.toPair(stdlibFile("std/system.nim"))
  current.deps.add p
  if not c.processedModules.containsOrIncl(p.modname):
    #echo "NIFLING ", p.nimFile, " -> ", c.config.parsedFile(p)
    execNifler c, p
    var imported = Node(files: @[p], id: c.nodes.len, parent: current.id, isSystem: true)
    c.nodes.add imported
    parseDeps c, p, imported

proc parseDeps(c: var DepContext; p: FilePair; current: Node) =
  execNifler c, p

  let depsFile = if c.isGeneratingFinal: c.config.deps2File(p) else: c.config.depsFile(p)
  var stream = nifstreams.open(depsFile)
  try:
    discard processDirectives(stream.r)
    var buf = fromStream(stream)
    processDeps c, beginRead(buf), current
    if {SkipSystem, IsSystem} * c.moduleFlags == {} and not current.isSystem:
      importSystem c, current
  finally:
    nifstreams.close(stream)

type
  CFile = tuple
    name, obj, customArgs: string

proc rootPath(c: DepContext): string =
  # XXX: Relative paths in build files are relative to current working directory, not the location of the build file.
  result = absoluteParentDir(c.rootNode.files[0].nimFile)
  result = relativePath(result, os.getCurrentDir())

proc toBuildList(c: DepContext): seq[CFile] =
  result = @[]
  for v in c.nodes:
    let index = readIndex(c.config.indexFile(v.files[0]))
    for i in index.toBuild:
      let path = i[1]
      let obj = splitFile(path).name & ".o"
      let customArgs = i[2]
      result.add (path, obj, customArgs)

proc generateFinalBuildFile(c: DepContext; commandLineArgsNifc: string; passC, passL: string): string =
  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".final.build.nif"
  var b = nifbuilder.open(result)
  defer: b.close()

  b.addHeader()
  b.withTree "stmts":
    # Command definitions
    let nifc = findTool("nifc")
    let hexer = findTool("hexer")

    # Command for nifc (C code generation)
    b.withTree "cmd":
      b.addSymbolDef "nifc"
      b.addStrLit nifc
      b.addStrLit "c"
      b.addStrLit "--compileOnly"
      b.addKeyw "args"
      if commandLineArgsNifc.len > 0:
        for arg in commandLineArgsNifc.split(' '):
          if arg.len > 0:
            b.addStrLit arg
      b.addKeyw "input"

    # Command for hexer
    b.withTree "cmd":
      b.addSymbolDef "hexer"
      b.addStrLit hexer
      b.addStrLit "--bits:" & $c.config.bits
      b.withTree "input":
        b.addIntLit 0

    # Command for C compiler (object files)
    b.withTree "cmd":
      b.addSymbolDef "cc"
      b.addStrLit "gcc"  # Use gcc directly since environment handling is different
      b.addStrLit "-c"
      if passC.len > 0:
        for arg in passC.split(' '):
          if arg.len > 0:
            b.addStrLit arg
      b.addStrLit "-I" & rootPath(c)
      b.addKeyw "args"
      b.addKeyw "input"
      b.addStrLit "-o"
      b.addKeyw "output"

    # Command for linking
    if c.cmd in {DoCompile, DoRun}:
      b.withTree "cmd":
        b.addSymbolDef "link"
        b.addStrLit "gcc"
        b.addStrLit "-o"
        b.addKeyw "output"
        b.withTree "input":
          b.addIntLit 0
          b.addIntLit -1  # all inputs
        if passL.len > 0:
          for arg in passL.split(' '):
            if arg.len > 0:
              b.addStrLit arg

    # Build rules
    if c.cmd in {DoCompile, DoRun}:
      let buildList = toBuildList(c)

      # Link executable
      b.withTree "do":
        b.addIdent "link"
        # Input: all object files
        var objFiles = initHashSet[string]()
        for cfile in buildList:
          let obj = c.config.nifcachePath / cfile.obj
          if not objFiles.containsOrIncl(obj):
            b.withTree "input":
              b.addStrLit obj
        for v in c.nodes:
          let obj = c.config.objFile(v.files[0])
          if not objFiles.containsOrIncl(obj):
            b.withTree "input":
              b.addStrLit obj
        b.withTree "output":
          b.addStrLit c.config.exeFile(c.rootNode.files[0])

      objFiles.clear()
      # Build object files from C files with custom args
      for cfile in buildList:
        let obj = c.config.nifcachePath / cfile.obj
        if not objFiles.containsOrIncl(obj):
          b.withTree "do":
            b.addIdent "cc"
            b.withTree "input":
              b.addStrLit cfile.name
            b.withTree "args":
              b.addStrLit cfile.customArgs
            b.withTree "output":
              b.addStrLit obj

      for i, v in pairs c.nodes:
        let obj = c.config.objFile(v.files[0])
        if not objFiles.containsOrIncl(obj):
          b.withTree "do":
            b.addIdent "cc"
            b.withTree "input":
              b.addStrLit c.config.cFile(v.files[0])
            b.withTree "output":
              b.addStrLit obj

        # Build C files from .c.nif files
        b.withTree "do":
          b.addIdent "nifc"
          if i == 0:
            b.withTree "args":
              b.addStrLit "--isMain"
          b.withTree "input":
            b.addStrLit c.config.nifcFile(v.files[0])
          b.withTree "output":
            b.addStrLit c.config.cFile(v.files[0])

        # Build .c.nif files from .2.nif files
        b.withTree "do":
          b.addIdent "hexer"
          b.withTree "input":
            b.addStrLit c.config.semmedFile(v.files[0])
          b.withTree "input":
            b.addStrLit c.config.indexFile(v.files[0])
          b.withTree "output":
            b.addStrLit c.config.nifcFile(v.files[0])

proc cachedConfigFile(config: NifConfig): string =
  config.nifcachePath / "cachedconfigfile.txt"

proc generateFrontendBuildFile(c: DepContext; commandLineArgs: string): string =
  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".build.nif"
  var b = nifbuilder.open(result)
  defer: b.close()

  b.addHeader()
  b.withTree "stmts":
    # Command definitions
    b.withTree "cmd":
      b.addSymbolDef "nifler"
      b.addStrLit c.nifler
      b.addStrLit "--portablePaths"
      b.addStrLit "--deps"
      b.addStrLit "parse"
      b.addKeyw "input"
      b.addKeyw "output"

    b.withTree "cmd":
      b.addSymbolDef "nimsem"
      b.addStrLit c.nimsem
      if commandLineArgs.len > 0:
        for arg in commandLineArgs.split(' '):
          if arg.len > 0:
            b.addStrLit arg
      b.addStrLit "m"
      b.addKeyw "args"
      b.withTree "input":
        b.addIntLit 0  # main parsed file
      b.withTree "output":
        b.addIntLit 0  # semmed file output
      b.withTree "output":
        b.addIntLit 1  # index file output

    for plugin in c.foundPlugins:
      b.withTree "cmd":
        b.addSymbolDef plugin
        b.addStrLit plugin
        b.addKeyw "args"
        b.withTree "input":
          b.addIntLit 0  # main parsed file
        b.withTree "output":
          b.addIntLit 0  # semmed file output
        b.withTree "output":
          b.addIntLit 1  # index file output

    # Build rules for semantic checking
    var i = 0
    for v in c.nodes:
      b.withTree "do":
        # Choose the right command based on flags
        if v.plugin.len > 0:
          b.addIdent v.plugin
        else:
          b.addIdent "nimsem"
        b.withTree "args":
          if v.isSystem:
            b.addStrLit "--isSystem"
          elif i == 0:  # first node is main
            b.addStrLit "--isMain"

        # Input: parsed file
        var seenDeps = initHashSet[string]()
        for f in v.files:
          let pf = c.config.parsedFile(f)
          if not seenDeps.containsOrIncl(pf):
            b.withTree "input":
              b.addStrLit pf
        # Input: dependencies
        for f in v.deps:
          let idxFile = c.config.indexFile(f)
          if not seenDeps.containsOrIncl(idxFile):
            b.withTree "input":
              b.addStrLit idxFile
        # Input: cached config file
        b.withTree "input":
          b.addStrLit c.config.cachedConfigFile()
        # Outputs: semmed file and index file
        b.withTree "output":
          b.addStrLit c.config.semmedFile(v.files[0])
        b.withTree "output":
          b.addStrLit c.config.indexFile(v.files[0])
      inc i

    # Build rules for parsing
    var seenFiles = initHashSet[string]()
    for v in c.nodes:
      if v.plugin.len > 0:
        continue
      for i in 0..<v.files.len:
        let f = c.config.parsedFile(v.files[i])
        if not seenFiles.containsOrIncl(f):
          let nimFile = v.files[i].nimFile
          b.withTree "do":
            b.addIdent "nifler"
            b.withTree "input":
              b.addStrLit nimFile
            b.withTree "output":
              b.addStrLit f

proc generateCachedConfigFile(c: DepContext; passC, passL: string) =
  let path = c.config.cachedConfigFile()
  let configStr = c.config.getOptionsAsOneString() & " " & c.rootNode.files[0].nimFile &
                  " --passC:" & passC & " --passL:" & passL

  let needUpdate = if semos.fileExists(path) and not c.forceRebuild:
                     configStr != readFile path
                   else:
                     true
  if needUpdate:
    writeFile path, configStr

proc buildGraph*(config: sink NifConfig; project: string; forceRebuild, silentMake: bool;
    commandLineArgs, commandLineArgsNifc: string; moduleFlags: set[ModuleFlag]; cmd: Command;
    passC, passL: string) =
  let nifler = findTool("nifler")
  let nifmake = findTool("nifmake")

  if config.compat:
    let cfgNif = config.nifcachePath / moduleSuffix(project, []) & ".cfg.nif"
    exec quoteShell(nifler) & " config " & quoteShell(project) & " " &
      quoteShell(cfgNif)
    parseNifConfig cfgNif, config

  template initDepContext(isFinal: bool): DepContext =
    var c = DepContext(nifler: nifler, config: config, rootNode: nil, includeStack: @[],
      forceRebuild: forceRebuild, moduleFlags: moduleFlags, nimsem: findTool("nimsem"),
      cmd: cmd, isGeneratingFinal: isFinal)
    let p = c.toPair(project)
    c.rootNode = Node(files: @[p], id: 0, parent: -1, active: 0, isSystem: IsSystem in moduleFlags)
    c.nodes.add c.rootNode
    c.processedModules.incl p.modname
    parseDeps c, p, c.rootNode
    c

  var c = initDepContext(false)
  generateCachedConfigFile c, passC, passL
  let buildFilename = generateFrontendBuildFile(c, commandLineArgs)
  #echo "run with: nifmake run ", buildFilename
  when defined(windows):
    putEnv("CC", "gcc")
    putEnv("CXX", "g++")
  let nifmakeCommand = quoteShell(nifmake) &
    (if forceRebuild: " --force" else: "") &  # Use generic force flag
    " -j run "
  exec nifmakeCommand & quoteShell(buildFilename)

  # Parse `.2.deps.nif`.
  # It is generated by nimsem and doesn't contains modules imported under `when false:`.
  # https://github.com/nim-lang/nimony/issues/985
  c = initDepContext(true)
  let buildFinalFilename = generateFinalBuildFile(c, commandLineArgsNifc, passC, passL)
  exec nifmakeCommand & quoteShell(buildFinalFilename)
  if cmd == DoRun:
    exec c.config.exeFile(c.rootNode.files[0])

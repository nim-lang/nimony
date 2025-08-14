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
import ".." / lib / tooldirs
import ".." / models / nifindex_tags

include nifprelude

type
  FilePair = object
    nimFile: string
    modname: string

proc indexFile(config: NifConfig; f: FilePair; bundle: string): string =
  config.nifcachePath / bundle / f.modname & ".2.idx.nif"

proc parsedFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".1.nif"
proc depsFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".1.deps.nif"
proc deps2File(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".2.deps.nif"
proc semmedFile(config: NifConfig; f: FilePair; bundle: string): string =
  config.nifcachePath / bundle / f.modname & ".2.nif"
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
    deps: seq[int] # index into c.nodes
    id, parent: int
    active: int
    isSystem: bool
    plugin: string

  Command* = enum
    DoCheck, # like `nim check`
    DoTranslate, # translate to C like "nim --compileOnly"
    DoCompile, # like `nim c` but with nifler
    DoRun # like `nim run`

  CFile = object
    name, obj, customArgs: string

  DepContext = object
    forceRebuild: bool
    cmd: Command
    nifler, nimsem: string
    config: NifConfig
    nodes: seq[Node]
    rootNode: Node
    includeStack: seq[string]
    processedModules: Table[string, int] # modname -> index to c.nodes
    moduleFlags: set[ModuleFlag]
    isGeneratingFinal: bool
    foundPlugins: HashSet[string]
    toBuild: seq[CFile]
    passL: seq[string]

proc toPair(c: DepContext; f: string): FilePair =
  FilePair(nimFile: f, modname: moduleSuffix(f, c.config.paths))

proc processDep(c: var DepContext; n: var Cursor; current: Node)
proc traverseDeps(c: var DepContext; p: FilePair; current: Node)

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
          traverseDeps(c, c.toPair(f2), current)
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
  let existingNode = c.processedModules.getOrDefault(p.modname, -1)
  if existingNode == -1:
    var imported = Node(files: @[p], id: c.nodes.len, parent: current.id, isSystem: isSystem,
                        plugin: current.plugin)
    current.deps.add imported.id
    c.processedModules[p.modname] = imported.id
    c.nodes.add imported
    traverseDeps c, p, imported
  else:
    # add the dependency anyway unless it creates a cycle:
    if wouldCreateCycle(c, current, p):
      discard "ignore cycle"
      echo "cycle detected: ", current.files[0].nimFile, " <-> ", p.nimFile
    else:
      current.deps.add existingNode

proc processPluginImport(c: var DepContext; f: ImportedFilename; info: PackedLineInfo; current: Node) =
  let f2 = resolveFileWrapper(c.config.paths, current.files[current.active].nimFile, f.path)
  if not semos.fileExists(f2): return
  let p = c.toPair(f2)
  let existingNode = c.processedModules.getOrDefault(p.modname, -1)
  if existingNode == -1:
    var imported = Node(files: @[p], id: c.nodes.len,
                        parent: current.id, isSystem: false, plugin: f.plugin)
    current.deps.add imported.id
    c.processedModules[p.modname] = imported.id
    c.nodes.add imported
    c.foundPlugins.incl f.plugin
  else:
    current.deps.add existingNode

proc processImport(c: var DepContext; it: var Cursor; current: Node) =
  let info = it.info
  var x = it
  skip it
  inc x # skip the `import`
  # ignore conditional imports:
  if x.stmtKind == WhenS: return
  while x.kind != ParRi:
    if x.kind == ParLe and x.exprKind == PragmaxX:
      var y = x
      inc y
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

proc processBuild(c: var DepContext; it: var Cursor) =
  inc it
  while it.kind != ParRi:
    assert it.exprKind == TupX
    var x = it
    skip it
    inc x
    assert x.kind == StringLit
    let typ = pool.strings[x.litId]
    inc x
    assert x.kind == StringLit
    let path = pool.strings[x.litId]
    let obj = splitFile(path).name & ".o"
    inc x
    assert x.kind == StringLit
    let args = pool.strings[x.litId]
    inc x
    c.toBuild.add CFile(name: path, obj: obj, customArgs: args)
  inc it

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
  of NoStmt:
    if n.tagId == TagId(BuildIdx):
      processBuild c, n
    elif n.tagId == TagId(PassLP):
      inc n
      while n.kind != ParRi:
        assert n.kind == StringLit
        c.passL.add pool.strings[n.litId]
        inc n
    else:
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
  var existingNode = c.processedModules.getOrDefault(p.modname, -1)
  if existingNode == -1:
    #echo "NIFLING ", p.nimFile, " -> ", c.config.parsedFile(p)
    execNifler c, p
    var imported = Node(files: @[p], id: c.nodes.len, parent: current.id, isSystem: true)
    c.nodes.add imported
    c.processedModules[p.modname] = imported.id
    traverseDeps c, p, imported
    existingNode = imported.id
  current.deps.add existingNode

proc traverseDeps(c: var DepContext; p: FilePair; current: Node) =
  let depsFile: string
  if not c.isGeneratingFinal:
    execNifler c, p
    depsFile = c.config.depsFile(p)
  else:
    depsFile = c.config.deps2File(p)

  var stream = nifstreams.open(depsFile)
  try:
    discard processDirectives(stream.r)
    var buf = fromStream(stream)
    processDeps c, beginRead(buf), current
    if {SkipSystem, IsSystem} * c.moduleFlags == {} and not current.isSystem:
      importSystem c, current
  finally:
    nifstreams.close(stream)

proc rootPath(c: DepContext): string =
  # XXX: Relative paths in build files are relative to current working directory, not the location of the build file.
  result = absoluteParentDir(c.rootNode.files[0].nimFile)
  result = relativePath(result, os.getCurrentDir())

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
      b.addStrLit c.config.cc
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
        b.addStrLit c.config.linker
        b.addStrLit "-o"
        b.addKeyw "output"
        b.withTree "input":
          b.addIntLit 0
          b.addIntLit -1  # all inputs
        b.withTree "argsext":
          b.addStrLit ".linker.args"
        if passL.len > 0:
          for arg in passL.split(' '):
            if arg.len > 0:
              b.addStrLit arg
        for i in c.passL:
          b.addStrLit i

    # Build rules
    if c.cmd in {DoCompile, DoRun}:
      # Link executable
      b.withTree "do":
        b.addIdent "link"
        # Input: all object files
        var objFiles = initHashSet[string]()
        for cfile in c.toBuild:
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
      for cfile in c.toBuild:
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
            b.addStrLit c.config.semmedFile(v.files[0], v.plugin)
          b.withTree "input":
            b.addStrLit c.config.indexFile(v.files[0], v.plugin)
          b.withTree "output":
            b.addStrLit c.config.nifcFile(v.files[0])

proc cachedConfigFile(config: NifConfig): string =
  config.nifcachePath / "cachedconfigfile.txt"

proc generateSemInstructions(c: DepContext; v: Node; b: var Builder; isMain: bool) =
  b.withTree "do":
    b.addIdent "nimsem"
    b.withTree "args":
      if v.isSystem:
        b.addStrLit "--isSystem"
      elif isMain:
        b.addStrLit "--isMain"
    # Input: parsed file
    var seenDeps = initHashSet[string]()
    for f in v.files:
      let pf = c.config.parsedFile(f)
      if not seenDeps.containsOrIncl(pf):
        b.withTree "input":
          b.addStrLit pf
    # Input: dependencies
    for i in v.deps:
      let idxFile = c.config.indexFile(c.nodes[i].files[0], c.nodes[i].plugin)
      if not seenDeps.containsOrIncl(idxFile):
        b.withTree "input":
          b.addStrLit idxFile
    # Input: cached config file
    b.withTree "input":
      b.addStrLit c.config.cachedConfigFile()
    # Outputs: semmed file and index file
    b.withTree "output":
      b.addStrLit c.config.semmedFile(v.files[0], v.plugin)
    b.withTree "output":
      b.addStrLit c.config.indexFile(v.files[0], v.plugin)

proc generatePluginSemInstructions(c: DepContext; v: Node; b: var Builder) =
  #[ An import plugin fills `nimcache/<plugin>` for us. It is our job to
  generate index files for all `.nif` files in there. Both the frontend and
  the backend needs these files. But we want the index generation to happen
  in parallel. We cannot iterate over the files in the plugin directory as
  it is empty until the plugin has run. So unfortunately this logic lives in
  the v2 plugin.
  ]#
  b.withTree "do":
    b.addIdent v.plugin
    b.withTree "input":
      b.addStrLit v.files[0].nimFile
    b.withTree "output":
      b.addStrLit c.config.semmedFile(v.files[0], v.plugin)
    b.withTree "output":
      b.addStrLit c.config.indexFile(v.files[0], v.plugin)

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
        # index file output is not explicitly passed to the plugin!
        #b.withTree "output":
        #  b.addIntLit 1  # index file output

    # Build rules for semantic checking
    var i = 0
    for v in c.nodes:
      if v.plugin.len == 0:
        generateSemInstructions c, v, b, i == 0
      else:
        generatePluginSemInstructions c, v, b
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

proc initDepContext(config: sink NifConfig; project, nifler: string; isFinal, forceRebuild: bool; moduleFlags: set[ModuleFlag]; cmd: Command): DepContext =
  result = DepContext(nifler: nifler, config: config, rootNode: nil, includeStack: @[],
    forceRebuild: forceRebuild, moduleFlags: moduleFlags, nimsem: findTool("nimsem"),
    cmd: cmd, isGeneratingFinal: isFinal)
  let p = result.toPair(project)
  result.rootNode = Node(files: @[p], id: 0, parent: -1, active: 0, isSystem: IsSystem in moduleFlags)
  result.nodes.add result.rootNode
  result.processedModules[p.modname] = 0
  traverseDeps result, p, result.rootNode

proc buildGraph*(config: sink NifConfig; project: string; forceRebuild, silentMake: bool;
    commandLineArgs, commandLineArgsNifc: string; moduleFlags: set[ModuleFlag]; cmd: Command;
    passC, passL: string, executableArgs: string) =
  let nifler = findTool("nifler")
  let nifmake = findTool("nifmake")

  if config.compat:
    let cfgNif = config.nifcachePath / moduleSuffix(project, []) & ".cfg.nif"
    exec quoteShell(nifler) & " config " & quoteShell(project) & " " &
      quoteShell(cfgNif)
    parseNifConfig cfgNif, config

  var c = initDepContext(config, project, nifler, false, forceRebuild, moduleFlags, cmd)
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
  c = initDepContext(config, project, nifler, true, forceRebuild, moduleFlags, cmd)
  let buildFinalFilename = generateFinalBuildFile(c, commandLineArgsNifc, passC, passL)
  exec nifmakeCommand & quoteShell(buildFinalFilename)
  if cmd == DoRun:
    exec c.config.exeFile(c.rootNode.files[0]) & executableArgs

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
    filenameVal(x, files, hasError)

    if hasError:
      discard "ignore wrong `include` statement"
    else:
      for f1 in items(files):
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

proc processImport(c: var DepContext; it: var Cursor; current: Node) =
  let info = it.info
  var x = it
  skip it
  inc x # skip the `import`
  while x.kind != ParRi:
    if x.kind == ParLe and x == "pragmax":
      inc x
      var y = x
      skip y
      if y.substructureKind == PragmasU:
        inc y
        if y.kind == Ident and pool.strings[y.litId] == "cyclic":
          continue

    var files: seq[ImportedFilename] = @[]
    var hasError = false
    filenameVal(x, files, hasError)
    if hasError:
      discard "ignore wrong `import` statement"
    else:
      for f in files:
        importSingleFile c, f.path, info, current, false

proc processFrom(c: var DepContext; it: var Cursor; current: Node) =
  let info = it.info
  var x = it
  skip it
  inc x # skip the `from`
  var files: seq[ImportedFilename] = @[]
  var hasError = false
  filenameVal(x, files, hasError)
  if hasError:
    discard "ignore wrong `from` statement"
  else:
    for f in files:
      importSingleFile c, f.path, info, current, false
      break

proc processDep(c: var DepContext; n: var Cursor; current: Node) =
  case stmtKind(n)
  of ImportS:
    processImport c, n, current
  of IncludeS, ImportExceptS:
    processInclude c, n, current
  of FromimportS:
    processFrom c, n, current
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

proc execNifler(c: var DepContext; input, output: string) =
  if not c.forceRebuild and semos.fileExists(output) and semos.fileExists(input) and
      getLastModificationTime(output) > getLastModificationTime(input):
    discard "nothing to do"
  else:
    let cmd = quoteShell(c.nifler) & " --portablePaths --deps parse " & quoteShell(input) & " " &
      quoteShell(output)
    exec cmd

proc importSystem(c: var DepContext; current: Node) =
  let p = c.toPair(stdlibFile("std/system.nim"))
  current.deps.add p
  if not c.processedModules.containsOrIncl(p.modname):
    #echo "NIFLING ", p.nimFile, " -> ", c.config.parsedFile(p)
    execNifler c, p.nimFile, c.config.parsedFile(p)
    var imported = Node(files: @[p], id: c.nodes.len, parent: current.id, isSystem: true)
    c.nodes.add imported
    parseDeps c, p, imported

proc parseDeps(c: var DepContext; p: FilePair; current: Node) =
  execNifler c, p.nimFile, c.config.parsedFile(p)

  let depsFile = c.config.depsFile(p)
  var stream = nifstreams.open(depsFile)
  try:
    discard processDirectives(stream.r)
    var buf = fromStream(stream)
    processDeps c, beginRead(buf), current
    if {SkipSystem, IsSystem} * c.moduleFlags == {} and not current.isSystem:
      importSystem c, current
  finally:
    nifstreams.close(stream)

proc mescape(p: string): string =
  when defined(windows):
    result = p.replace("\\", "/")
  else:
    result = p.replace(":", "\\:") # Rule separators
  result = result.multiReplace({
    " ": "\\ ",   # Spaces
    "#": "\\#",   # Comments
    "$": "$$",    # Variables
    "(": "\\(",   # Function calls
    ")": "\\)",
    "*": "\\*",   # Wildcards
    "[": "\\[",   # Pattern matching
    "]": "\\]"
  })

const makefileHeader = """
# Auto-generated Makefile
.PHONY: all
.SECONDARY:
  """ # don't delete intermediate files

type
  CFile = tuple
    name, obj, customArgs: string

proc rootPath(c: DepContext): string =
  # XXX: Relative paths in makefile are relative to current working directory, not the location of the makefile.
  result = absoluteParentDir(c.rootNode.files[0].nimFile)
  result = relativePath(result, os.getCurrentDir())

proc toBuildList(c: DepContext): seq[CFile] =
  result = @[]
  for v in c.nodes:
    let index = readIndex(mescape(c.config.indexFile(v.files[0])))
    for i in index.toBuild:
      let path = i[1]
      let obj = splitFile(path).name & ".o"
      let customArgs = i[2]
      result.add (path, obj, customArgs)

proc generateFinalMakefile(c: DepContext; commandLineArgsNifc: string; passC, passL: string): string =
  var s = makefileHeader
  let dest =
    case c.cmd
    of DoCheck:
      c.config.indexFile(c.rootNode.files[0])
    of DoTranslate:
      ""
    of DoCompile, DoRun:
      c.config.exeFile(c.rootNode.files[0])

  # Absolute path of root node module
  s.add "\nROOT_PATH = " & rootPath(c)
  if passC.len != 0:
    s.add "\nCFLAGS += " & mescape(passC)
  s.add "\nall: " & mescape dest

  # The .exe file depends on all .o files:
  if c.cmd in {DoCompile, DoRun}:
    let buildList = toBuildList(c)

    s.add "\n" & mescape(c.config.exeFile(c.rootNode.files[0])) & ":"

    for cfile in buildList:
      s.add " " & mescape(c.config.nifcachePath / cfile.obj)

    for v in c.nodes:
      s.add " " & mescape(c.config.objFile(v.files[0]))
    s.add "\n\t$(CC) -o $@ $^"
    if passL.len != 0:
      s.add " " & mescape(passL)

    for cfile in buildList:
      s.add "\n" & mescape(c.config.nifcachePath / cfile.obj) & ": " & mescape(cfile.name) &
            "\n\t$(CC) -c $(CFLAGS) $(CPPFLAGS) " &
            mescape(cfile.customArgs) & " $< -o $@"

    # The .o files depend on all of their .c files:
    s.add "\n%.o: %.c\n\t$(CC) -c $(CFLAGS) -I$(ROOT_PATH) $(CPPFLAGS) $< -o $@"

    # entry point is special:
    let nifc = findTool("nifc")
    s.add "\n" & mescape(c.config.cFile(c.rootNode.files[0])) & ": " & mescape(c.config.nifcFile c.rootNode.files[0])
    s.add "\n\t" & mescape(nifc) & " c --compileOnly --isMain " & commandLineArgsNifc & " $<"

    # The .c files depend on their .c.nif files:
    s.add "\n%.c: %.c.nif\n\t" & mescape(nifc) & " c --compileOnly " & commandLineArgsNifc & " $<"

    # The .c.nif files depend on all of their .2.nif files:
    let hexer = findTool("hexer")
    s.add "\n%.c.nif: %.2.nif %.2.idx.nif\n\t" & mescape(hexer) & " --bits:" & $c.config.bits & " $<"

  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".final.makefile"
  writeFile result, s

proc cachedConfigFile(config: NifConfig): string =
  config.nifcachePath / "cachedconfigfile.txt"

proc generateFrontendMakefile(c: DepContext; commandLineArgs: string): string =
  var s = makefileHeader

  # every semchecked .nif file depends on all of its parsed.nif file
  # plus on the indexes of its imports:
  for v in c.nodes:
    s.add "\n" & mescape(c.config.indexFile(v.files[0])) & " " & mescape(c.config.semmedFile(v.files[0])) & ":"
    var seenDeps = initHashSet[string]()
    for f in v.files:
      let pf = c.config.parsedFile(f)
      if not seenDeps.containsOrIncl(pf):
        s.add " " & mescape(pf)
    for f in v.deps:
      let idxFile = c.config.indexFile(f)
      if not seenDeps.containsOrIncl(idxFile):
        s.add "  " & mescape(idxFile)
    s.add " " & c.config.cachedConfigFile()
    let args = commandLineArgs & (if v.isSystem: " --isSystem" else: "")
    s.add "\n\t" & mescape(c.nimsem) & " " & args & " m " & mescape(c.config.parsedFile(v.files[0])) & " " &
      mescape(c.config.semmedFile(v.files[0])) & " " & mescape(c.config.indexFile(v.files[0]))

  # every parsed.nif file is produced by a .nim file by the nifler tool:
  var seenFiles = initHashSet[string]()
  for v in c.nodes:
    for i in 0..<v.files.len:
      let f = c.config.parsedFile(v.files[i])
      if not seenFiles.containsOrIncl(f):
        let nimFile = v.files[i].nimFile
        s.add "\n" & mescape(f) & ": " & mescape(nimFile)
        s.add "\n\t" & mescape(c.nifler) & " --portablePaths --deps parse " & mescape(nimFile) & " " &
          mescape(f)

  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".makefile"
  writeFile result, s

proc generateCachedConfigFile(c: DepContext) =
  let path = c.config.cachedConfigFile()
  let configStr = c.config.getOptionsAsOneString() & " " & c.rootNode.files[0].nimFile
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

  if config.compat:
    let cfgNif = config.nifcachePath / moduleSuffix(project, []) & ".cfg.nif"
    exec quoteShell(nifler) & " config " & quoteShell(project) & " " &
      quoteShell(cfgNif)
    parseNifConfig cfgNif, config

  var c = DepContext(nifler: nifler, config: config, rootNode: nil, includeStack: @[],
    forceRebuild: forceRebuild, moduleFlags: moduleFlags, nimsem: findTool("nimsem"),
    cmd: cmd)
  let p = c.toPair(project)
  c.rootNode = Node(files: @[p], id: 0, parent: -1, active: 0, isSystem: IsSystem in moduleFlags)
  c.nodes.add c.rootNode
  c.processedModules.incl p.modname
  parseDeps c, p, c.rootNode
  generateCachedConfigFile c
  let makeFilename = generateFrontendMakefile(c, commandLineArgs)
  #echo "run with: make -f ", makeFilename
  when defined(windows):
    putEnv("CC", "gcc")
    putEnv("CXX", "g++")
  let makeCommand = "make" & (if silentMake: " -s" else: "") &
    (if forceRebuild: " -B" else: "") &
    " -f "
  exec makeCommand & quoteShell(makeFilename)

  let makeFinalFilename = generateFinalMakefile(c, commandLineArgsNifc, passC, passL)
  exec makeCommand & quoteShell(makeFinalFilename)
  if cmd == DoRun:
    exec c.config.exeFile(c.rootNode.files[0])

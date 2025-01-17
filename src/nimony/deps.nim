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
import semos, nifconfig, nimony_model
import ".." / gear2 / modnames, semdata

include nifprelude

type
  FilePair = object
    nimFile: string
    modname: string

proc indexFile(f: FilePair): string = "nifcache" / f.modname & ".2.idx.nif"
proc parsedFile(f: FilePair): string = "nifcache" / f.modname & ".1.nif"
proc depsFile(f: FilePair): string = "nifcache" / f.modname & ".1.deps.nif"
proc semmedFile(f: FilePair): string = "nifcache" / f.modname & ".2.nif"
proc nifcFile(f: FilePair): string = "nifcache" / f.modname & ".c.nif"
proc cFile(f: FilePair): string = "nifcache" / f.modname & ".c"
proc objFile(f: FilePair): string = "nifcache" / f.modname & ".o"

# It turned out to be too annoying in practice to have the exe file in
# the current directory per default so we now put it into the nifcache too:
proc exeFile(f: FilePair): string = "nifcache" / f.modname.addFileExt ExeExt

proc resolveFileWrapper(paths: openArray[string]; origin: string; toResolve: string): string =
  result = resolveFile(paths, origin, toResolve)
  if not fileExists(result) and toResolve.startsWith("std/"):
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

        if not isRecursive and fileExists(f2):
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
  if not fileExists(f2): return
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
      if y.substructureKind == PragmasS:
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
  of FromImportS:
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
  if not c.forceRebuild and fileExists(output) and fileExists(input) and
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
    #echo "NIFLING ", p.nimFile, " -> ", parsedFile(p)
    execNifler c, p.nimFile, parsedFile(p)
    var imported = Node(files: @[p], id: c.nodes.len, parent: current.id, isSystem: true)
    c.nodes.add imported
    parseDeps c, p, imported

proc parseDeps(c: var DepContext; p: FilePair; current: Node) =
  execNifler c, p.nimFile, parsedFile(p)

  let depsFile = depsFile(p)
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

proc generateFinalMakefile(c: DepContext): string =
  var s = makefileHeader
  let dest =
    case c.cmd
    of DoCheck:
      indexFile(c.rootNode.files[0])
    of DoTranslate:
      ""
    of DoCompile, DoRun:
      exeFile(c.rootNode.files[0])
  s.add "\nall: " & mescape dest

  # The .exe file depends on all .o files:
  if c.cmd in {DoCompile, DoRun}:
    s.add "\n" & mescape(exeFile(c.rootNode.files[0])) & ":"
    for v in c.nodes:
      s.add " " & mescape(objFile(v.files[0]))
    s.add "\n\t$(CC) -o $@ $^"

    # The .o files depend on all of their .c files:
    s.add "\n%.o: %.c\n\t$(CC) -c $(CFLAGS) $(CPPFLAGS) $< -o $@"

    # entry point is special:
    let nifc = findTool("nifc")
    s.add "\n" & mescape(cFile(c.rootNode.files[0])) & ": " & mescape(nifcFile c.rootNode.files[0])
    s.add "\n\t" & mescape(nifc) & " c --compileOnly --isMain $<"

    # The .c files depend on their .c.nif files:
    s.add "\n%.c: %.c.nif\n\t" & mescape(nifc) & " c --compileOnly $<"

    # The .c.nif files depend on all of their .2.nif files:
    let hexer = findTool("hexer")
    s.add "\n%.c.nif: %.2.nif %.2.idx.nif\n\t" & mescape(hexer) & " $<"

  result = "nifcache" / c.rootNode.files[0].modname & ".final.makefile"
  writeFile result, s

proc generateFrontendMakefile(c: DepContext; commandLineArgs: string): string =
  var s = makefileHeader

  # every semchecked .nif file depends on all of its parsed.nif file
  # plus on the indexes of its imports:
  for v in c.nodes:
    s.add "\n" & mescape(indexFile(v.files[0])) & " " & mescape(semmedFile(v.files[0])) & ":"
    var seenDeps = initHashSet[string]()
    for f in v.files:
      let pf = parsedFile(f)
      if not seenDeps.containsOrIncl(pf):
        s.add " " & mescape(pf)
    for f in v.deps:
      let idxFile = indexFile(f)
      if not seenDeps.containsOrIncl(idxFile):
        s.add "  " & mescape(idxFile)
    let args = commandLineArgs & (if v.isSystem: " --isSystem" else: "")
    s.add "\n\t" & mescape(c.nimsem) & " " & args & " m " & mescape(parsedFile(v.files[0])) & " " &
      mescape(semmedFile(v.files[0])) & " " & mescape(indexFile(v.files[0]))

  # every parsed.nif file is produced by a .nim file by the nifler tool:
  var seenFiles = initHashSet[string]()
  for v in c.nodes:
    for i in 0..<v.files.len:
      let f = parsedFile(v.files[i])
      if not seenFiles.containsOrIncl(f):
        let nimFile = v.files[i].nimFile
        s.add "\n" & mescape(f) & ": " & mescape(nimFile)
        s.add "\n\t" & mescape(c.nifler) & " --portablePaths --deps parse " & mescape(nimFile) & " " &
          mescape(f)

  result = "nifcache" / c.rootNode.files[0].modname & ".makefile"
  writeFile result, s

proc buildGraph*(config: sink NifConfig; project: string; compat, forceRebuild, silentMake: bool;
    commandLineArgs: string; moduleFlags: set[ModuleFlag]; cmd: Command) =
  let nifler = findTool("nifler")

  if compat:
    let cfgNif = "nifcache" / moduleSuffix(project, []) & ".cfg.nif"
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
  let makeFilename = generateFrontendMakefile(c, commandLineArgs)
  let makeFinalFilename = generateFinalMakefile(c)
  #echo "run with: make -f ", makeFilename
  when defined(windows):
    putEnv("CC", "gcc")
    putEnv("CXX", "g++")
  let makeCommand = "make" & (if silentMake: " -s" else: "") &
    (if forceRebuild: " -B" else: "") &
    " -f "
  exec makeCommand & quoteShell(makeFilename)
  exec makeCommand & quoteShell(makeFinalFilename)
  if cmd == DoRun:
    exec exeFile(c.rootNode.files[0])

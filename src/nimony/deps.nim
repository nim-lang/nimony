#[

- Build the graph. Every node is a list of files representing the main source file plus its included files.
  - for this we also need the config so that the paths can be resolved properly
- Every node also has a list of dependencies. Every single dependency is a dependency to a modules's interface!

]#

import std/[os, tables, sets, syncio, assertions, strutils]
import semos, nifconfig, nimony_model
import ".." / gear2 / modnames

include nifprelude

type
  FilePair = object
    nimFile: string
    modname: string

proc indexFile(f: FilePair): string = "nifcache" / f.modname & ".2.idx.nif"
proc parsedFile(f: FilePair): string = "nifcache" / f.modname & ".1.nif"
proc depsFile(f: FilePair): string = "nifcache" / f.modname & ".1.deps.nif"
proc semmedFile(f: FilePair): string = "nifcache" / f.modname & ".2.nif"

proc resolveFileWrapper(paths: openArray[string]; origin: string; toResolve: string): string =
  result = resolveFile(paths, origin, toResolve)
  if not fileExists(result) and toResolve.startsWith("std/"):
    result = resolveFile(paths, origin, toResolve.substr(4))

type
  Node = ref object
    files: seq[FilePair]
    deps: seq[FilePair]
    active: int

  DepContext = object
    nifler: string
    config: NifConfig
    nodes: seq[Node]
    rootNode: Node
    includeStack: seq[string]
    processedModules: HashSet[string]

proc toPair(c: DepContext; f: string): FilePair =
  FilePair(nimFile: f, modname: moduleSuffix(f, c.config.paths))

proc processDep(c: var DepContext; n: var Cursor; current: Node)
proc parseDeps(c: var DepContext; p: FilePair; current: Node)

proc processInclude(c: var DepContext; it: var Cursor; current: Node) =
  var files: seq[string] = @[]
  var hasError = false
  let info = it.info
  var x = it
  skip it
  inc x # skip the `include`
  filenameVal(x, files, hasError)

  if hasError:
    discard "ignore wrong `include` statement"
  else:
    for f1 in items(files):
      let f2 = resolveFileWrapper(c.config.paths, current.files[current.active].nimFile, f1)
      # check for recursive include files:
      var isRecursive = false
      for a in c.includeStack:
        if a == f2:
          isRecursive = true
          break

      if not isRecursive:
        let oldActive = current.active
        current.active = current.files.len
        current.files.add c.toPair(f2)
        var buf = parseFile(f2, c.config.paths)
        c.includeStack.add f2
        var n = cursorAt(buf, 0)
        processDep c, n, current
        current.active = oldActive
        c.includeStack.setLen c.includeStack.len - 1
      else:
        discard "ignore recursive include"

proc importSingleFile(c: var DepContext; f1: string; info: PackedLineInfo; current: Node) =
  let f2 = resolveFileWrapper(c.config.paths, current.files[current.active].nimFile, f1)
  let p = c.toPair(f2)
  current.deps.add p
  if not c.processedModules.containsOrIncl(p.modname):
    var imported = Node(files: @[p])
    c.nodes.add imported
    parseDeps c, p, imported

proc processImport(c: var DepContext; it: var Cursor; current: Node) =
  let info = it.info
  var x = it
  skip it
  inc x # skip the `import`

  if x.kind == ParLe and x == "pragmax":
    inc x
    var y = x
    skip y
    if y.substructureKind == PragmasS:
      inc y
      if y.kind == Ident and pool.strings[y.litId] == "cyclic":
        return

  var files: seq[string] = @[]
  var hasError = false
  filenameVal(x, files, hasError)
  if hasError:
    discard "ignore wrong `import` statement"
  else:
    for f in files:
      importSingleFile c, f, info, current

proc processDep(c: var DepContext; n: var Cursor; current: Node) =
  case stmtKind(n)
  of ImportS:
    processImport c, n, current
  of IncludeS:
    processInclude c, n, current
  else:
    skip n

proc processDeps(c: var DepContext; n: Cursor; current: Node) =
  var n = n
  if n.kind == ParLe and pool.tags[n.tagId] == "stmts":
    inc n
    while n.kind != ParRi:
      processDep c, n, current

proc parseDeps(c: var DepContext; p: FilePair; current: Node) =
  exec quoteShell(c.nifler) & " --portablePaths --deps parse " & quoteShell(p.nimFile) & " " &
    quoteShell(parsedFile(p))

  let depsFile = depsFile(p)
  var stream = nifstreams.open(depsFile)
  try:
    discard processDirectives(stream.r)
    var buf = fromStream(stream)
    processDeps c, beginRead(buf), current
  finally:
    nifstreams.close(stream)

proc nimexec*(cmd: string) =
  let t = findExe("nim")
  if t.len == 0:
    quit("FAILURE: cannot find nim.exe / nim binary")
  exec quoteShell(t) & " " & cmd

proc requiresTool*(tool, src: string; forceRebuild: bool) =
  let t = findTool(tool)
  if not fileExists(t) or forceRebuild:
    nimexec("c -d:release " & src)
    moveFile src.changeFileExt(ExeExt), t

proc generateMakefile(c: DepContext) =
  var s = ""
  s.add "# Auto-generated Makefile\n"
  s.add "\nall: " & semmedFile(c.rootNode.files[0])

  # every semchecked .nif file depends on all of its parsed.nif file
  # plus on the indexes of its imports:
  for v in c.nodes:
    s.add "\n" & semmedFile(v.files[0]) & ":"
    for f in v.files:
      s.add " " & parsedFile(f)
    for f in v.deps:
      s.add "  " & indexFile(f)
    s.add "\n\t nimsem m " & parsedFile(v.files[0]) & " " & semmedFile(v.files[0])

  # every parsed.nif file is produced by a .nim file by the nifler tool:
  for v in c.nodes:
    s.add "\n" & parsedFile(v.files[0]) & ": " & v.files[0].nimFile
    s.add "\n\t nifler --portablePaths --deps parse " & v.files[0].nimFile & " " & parsedFile(v.files[0])

  # every .idx.nif file depends on its semmed.nif file, but these cannot go out of sync
  # so we don't do anything here.
  writeFile "Makefile", s

proc buildGraph(project: string) =
  var config = NifConfig()
  config.bits = sizeof(int)*8

  let nifler = findTool("nifler")

  let cfgNif = "nifcache" / moduleSuffix(project, []) & ".cfg.nif"
  exec quoteShell(nifler) & " config " & quoteShell(project) & " " &
    quoteShell(cfgNif)
  parseNifConfig cfgNif, config

  var c = DepContext(nifler: nifler, config: config, rootNode: nil, includeStack: @[])
  let p = c.toPair(project)
  c.rootNode = Node(files: @[p])
  c.nodes.add c.rootNode
  c.processedModules.incl p.modname
  parseDeps c, p, c.rootNode
  generateMakefile c

when isMainModule:
  createDir("nifcache")
  requiresTool "nifler", "src/nifler/nifler.nim", false

  buildGraph paramStr(1)

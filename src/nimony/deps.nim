#[

- Build the graph. Every node is a list of files representing the main source file plus its included files.
  - for this we also need the config so that the paths can be resolved properly
- Every node also has a list of dependencies. Every single dependency is a dependency to a modules's interface!

]#

import std/[os, tables, sets, syncio]
import semos, nifconfig, nimony_model
import ".." / gear2 / modnames

include nifprelude

type
  Node = ref object
    files: seq[string]
    deps: seq[string]

  DepContext = object
    config: NifConfig
    nodes: Table[string, Node]
    rootNode: Node
    includeStack: seq[string]
    processedModules: HashSet[string]

proc processDep(c: var DepContext; n: var Cursor; current: Node)
proc parseDeps(c: var DepContext; src: string; current: Node)

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
      let f2 = resolveFile(c.config.paths, getFile(info), f1)
      # check for recursive include files:
      var isRecursive = false
      for a in c.includeStack:
        if a == f2:
          isRecursive = true
          break

      if not isRecursive:
        current.files.add f2
        var buf = parseFile(f2, c.config.paths)
        c.includeStack.add f2
        var n = cursorAt(buf, 0)
        processDep c, n, current
        c.includeStack.setLen c.includeStack.len - 1
      else:
        discard "ignore recursive include"

proc importSingleFile(c: var DepContext; f1, origin: string; info: PackedLineInfo; current: Node) =
  let f2 = resolveFile(c.config.paths, origin, f1)
  let suffix = moduleSuffix(f2, c.config.paths)
  current.deps.add f2
  if not c.processedModules.containsOrIncl(suffix):
    var imported = Node(files: @[f2])
    c.nodes[f2] = imported
    parseDeps c, f2, imported

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
    let origin = getFile(info)
    for f in files:
      importSingleFile c, f, origin, info, current

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
  var nested = 0
  while true:
    case n.kind
    of ParLe:
      inc nested
      if pool.tags[n.tagId] == "stmts":
        inc n
        processDep c, n, current
      else:
        inc n
    of ParRi:
      dec nested
      inc n
    else: inc n
    if nested == 0: break

proc parseDeps(c: var DepContext; src: string; current: Node) =
  let depsFile = src.changeFileExt(".deps.nif")
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

  # every .nif file is produced by a .nim file from the nifler tool:
  for k, v in c.nodes:
    s.add "\n" & k & ".nif:"
    for f in v.files:
      s.add "  " & f
    s.add "\n\t nifler --portablePaths --deps parse " & k & ".nim " & k & ".nif"

  # ever .nif file depends on the indexes of its imports:
  for k, v in c.nodes:
    s.add "\n" & k & ".nif:"
    for f in v.deps:
      s.add "  " & f & ".idx.nif"
    s.add "\n\t nimsem m " & k

  # every .idx.nif file depends on its .nif file:
  for k, v in c.nodes:
    for f in v.deps:
      s.add "\n" & f & ".idx.nif:"
      s.add "  " & f & ".nif"
      s.add "\n\t nimsem m " & f & ".idx.nif"

  s.add "\nall: " & c.rootNode.files[0] & ".nif"
  writeFile "Makefile", s

proc buildGraph(project: string) =
  var config = NifConfig()
  config.bits = sizeof(int)*8

  let nifler = findTool("nifler")
  let name = moduleSuffix(project, config.paths)
  let src = "nifcache" / name & ".1.nif"
  exec quoteShell(nifler) & " --portablePaths --deps parse " & quoteShell(project) & " " &
    quoteShell(src)

  var c = DepContext(config: config, rootNode: Node(files: @[project]), includeStack: @[])
  c.nodes[project] = c.rootNode
  parseDeps c, project, c.rootNode
  generateMakefile c

when isMainModule:
  createDir("nifcache")
  requiresTool "nifler", "src/nifler/nifler.nim", false

  buildGraph paramStr(1)

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

when defined(nimony):
  {.feature: "lenientnils".}
  {.feature: "untyped".}
import std/[os, tables, sets, syncio, hashes, assertions, strutils, times, formatfloat, dirs, paths]
import semos, nifconfig, nimony_model, semdata, langmodes, sharding
import ".." / gear2 / modnames
import ".." / lib / [tooldirs, platform, nifindexes, symparser, docpaths, argsfinder]
import ".." / models / nifindex_tags

include ".." / lib / nifprelude
include ".." / lib / compat2

type
  FilePair = object
    nimFile: string # can now also be a .nif file. This is used for the eval feature where Nimony
                    # calls itself for an extracted code snippet that must run at compile time.
    modname: string

proc indexFile(config: NifConfig; f: FilePair; bundle: string; preserveDocs = false): string =
  config.nifcachePath / bundle / f.modname & (if preserveDocs: ".sc.idx.nif" else: ".s.idx.nif")

proc parsedFile(config: NifConfig; f: FilePair; preserveDocs = false): string =
  ## `.p.nif` for normal builds, `.pc.nif` (parsed-with-comments) for `nimony doc`.
  ## Splitting the artifact keeps both cache populations valid simultaneously,
  ## so `nimony c` and `nimony doc` on the same project don't fight each other.
  config.nifcachePath / f.modname & (if preserveDocs: ".pc.nif" else: ".p.nif")
proc depsFile(config: NifConfig; f: FilePair; preserveDocs = false): string =
  config.nifcachePath / f.modname & (if preserveDocs: ".pc.deps.nif" else: ".p.deps.nif")
proc deps2File(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".s.deps.nif"
proc semmedFile(config: NifConfig; f: FilePair; bundle: string; preserveDocs = false): string =
  ## `.s.nif` for normal builds, `.sc.nif` (semmed-with-comments) for `nimony doc`.
  ## Mirrors the `.p.nif` / `.pc.nif` split so the doc and code-gen flows
  ## don't trample each other's post-sem artifact.
  config.nifcachePath / bundle / f.modname & (if preserveDocs: ".sc.nif" else: ".s.nif")
proc shardedSemmedFile(config: NifConfig; shardSuffix, bundle: string): string =
  ## `.s.nif` path for an individual shard (shard 0 is the base name, spillover
  ## shards are `<base>_<i>`). The single-shard case routes through
  ## `semmedFile` and stays byte-identical; only sharded backends call this.
  config.nifcachePath / bundle / shardSuffix & ".s.nif"
proc docOutDir(config: NifConfig): string =
  ## User-facing destination for `nimony doc`. Honors `--outdir:DIR`; default
  ## is `htmldocs/` (matches Nim's convention).
  if config.outDir.len > 0: config.outDir
  else: "htmldocs"
proc docRelpath(f: FilePair; projectRoot, stdlibRoot: string): string =
  ## Source-derived html relpath, shared by deps.nim (which declares the
  ## output) and dagon (which synthesises the cross-link URL). `nimFile` may
  ## have been recorded relative to the original cwd (when imports resolved
  ## paths against a non-cwd module) — absolutise so the root-prefix tests
  ## match. Slash-normalised so Windows builds produce the same artifacts as
  ## POSIX builds and so `deriveRelpath`'s prefix match agrees with
  ## already-normalised `projectRoot`/`stdlibRoot`.
  deriveRelpath(toUnixPath(toAbsolutePath(f.nimFile)), projectRoot, stdlibRoot)
proc docFile(config: NifConfig; f: FilePair; projectRoot, stdlibRoot: string): string =
  docOutDir(config) / docRelpath(f, projectRoot, stdlibRoot)
proc indexHtmlFile(config: NifConfig): string =
  docOutDir(config) / "theindex.html"
proc docIdxFile(config: NifConfig; f: FilePair): string =
  ## Build-cache sidecar; stays under `nifcachePath` even when the user-facing
  ## HTML is redirected via `--outdir`. Not user-relevant; uses the modname
  ## hash so it can't collide regardless of source layout.
  config.nifcachePath / "docs" / f.modname & ".docidx"
proc hexedFile(config: NifConfig; f: FilePair): string = config.nifcachePath / f.modname & ".x.nif"
proc shardedHexedFile(config: NifConfig; shardSuffix: string): string =
  config.nifcachePath / shardSuffix & ".x.nif"
proc shardedObjFile(config: NifConfig; shardSuffix, backendDir: string): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / shardSuffix & ".o"
proc shardedNifcFile(config: NifConfig; shardSuffix, backendDir: string): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / shardSuffix & ".c.nif"
proc shardedCFile(config: NifConfig; shardSuffix, backendDir: string): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / shardSuffix & ".c"
proc shardedLlFile(config: NifConfig; shardSuffix, backendDir: string): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / shardSuffix & ".ll"
proc shardedGenFile(config: NifConfig; shardSuffix, backendDir: string): string =
  case config.backend
  of backendC: config.shardedCFile(shardSuffix, backendDir)
  of backendLLVM: config.shardedLlFile(shardSuffix, backendDir)
proc nifcFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".c.nif"

proc cFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".c"
proc llFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".ll"
proc genFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  case config.backend
  of backendC: config.cFile(f, backendDir)
  of backendLLVM: config.llFile(f, backendDir)
proc objFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".o"

# It turned out to be too annoying in practice to have the exe file in
# the current directory per default so we now put it into the nifcache too:
# DCE and everything after is main-specific (different DCE outcomes); use backendDir.
proc exeFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  # `--out:PATH` / `--outdir:DIR` override the default
  # `<nimcache>/<backendDir>/<basename>.exe` location for executables.
  # `outDir` / `outFile` are populated by the CLI parser per Nim's
  # semantics (see `cli.nim`'s "out"/"outdir" handlers). Lib and
  # staticlib paths still derive from nimcache — extension fix-ups for
  # those are platform-specific and not in scope yet.
  let baseName = f.nimFile.splitFile.name
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  case config.appType
  of appConsole, appGui:
    if config.outFile.len > 0 or config.outDir.len > 0:
      let nameOnly = if config.outFile.len > 0: config.outFile else: baseName
      let withExt =
        if nameOnly.splitFile.ext.len > 0: nameOnly
        else: nameOnly.addFileExt(ExeExt)
      if config.outDir.len > 0: config.outDir / withExt
      else: withExt
    else:
      base / baseName.addFileExt(ExeExt)
  of appLib:
    if config.targetOS == osWindows:
      base / baseName.addFileExt("dll")
    elif config.targetOS in {osMacosx, osIos}:
      base / ("lib" & baseName & ".dylib")
    else:
      base / ("lib" & baseName & ".so")
  of appStaticLib:
    if config.targetOS == osWindows:
      base / baseName.addFileExt("lib")
    else:
      base / ("lib" & baseName & ".a")

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
    cyclicFiles: seq[int] ## indices into `files` that are cyclic module members (need separate outputs)
    shards: seq[string]
      ## Module suffixes this node produces. Defaults to `@[files[0].modname]`
      ## for non-sharded modules (the historical 1:1 mapping). Sem.nim's
      ## post-sem sharder writes a `<modname>.s.shards.txt` manifest listing
      ## the shards it emitted; `populateShardsFromManifest` reads it
      ## between the frontend and backend nifmake runs. Backend build rules
      ## iterate this seq so a sharded module gets N parallel
      ## hexer/nifc/cc invocations and the linker collects all N objects.

  Command* = enum
    DoCheck, # like `nim check`
    DoTranslate, # translate to C like "nim --compileOnly"
    DoCompile, # like `nim c` but with nifler
    DoRun, # like `nim run`
    DoDoc # like `nim doc`: front-end then dagon backend

  BuildFlag* = enum
    ForceRebuild   ## passes `--force` to nifmake (rebuild all)
    SilentMake     ## suppress make output
    Profile        ## ask nifmake to print its timing profile
    Report         ## ask nifmake to print machine-readable invocation counts
    Stats          ## after build, print total LOC + module count across the dep graph

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
    passC: seq[string]

proc toPair(c: DepContext; f: string): FilePair =
  if f.endsWith(".nif"):
    # For .p.nif files (e.g. from compile-time eval snippets), extract the
    # module suffix directly from the filename rather than recomputing it:
    FilePair(nimFile: f, modname: extractModuleSuffix(f))
  else:
    FilePair(nimFile: f, modname: moduleSuffix(f, c.config.paths))

proc processDep(c: var DepContext; n: var Cursor; current: Node)
proc traverseDeps(c: var DepContext; p: FilePair; current: Node)

proc processInclude(c: var DepContext; it: var Cursor; current: Node) =
  var files: seq[ImportedFilename] = @[]
  var x = it
  skip it
  x.into:  # (include …)
    while x.hasMore:
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
                        plugin: current.plugin, shards: @[p.modname])
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
                        parent: current.id, isSystem: false, plugin: f.plugin,
                        shards: @[p.modname])
    current.deps.add imported.id
    c.processedModules[p.modname] = imported.id
    c.nodes.add imported
    c.foundPlugins.incl f.plugin
  else:
    current.deps.add existingNode

proc importCyclicModule(c: var DepContext; f1: string; info: PackedLineInfo;
                        current: Node) =
  ## Merge a cyclic import target into the current node's cycle group.
  let f2 = resolveFileWrapper(c.config.paths, current.files[current.active].nimFile, f1)
  if not semos.fileExists(f2): return
  let p = c.toPair(f2)
  if c.processedModules.hasKey(p.modname):
    return # already part of this or another node
  # Add to current node as a cyclic group member:
  let idx = current.files.len
  current.files.add p
  current.cyclicFiles.add idx
  c.processedModules[p.modname] = current.id
  # Traverse the cyclic module's deps as part of this node:
  let oldActive = current.active
  current.active = idx
  traverseDeps c, p, current
  current.active = oldActive

proc evalDepCond(config: NifConfig; n: Cursor): bool =
  ## Evaluate a `when`-condition expression carried by a `(when ...)` import
  ## marker. Recognises `defined(IDENT)`, boolean `not`/`and`/`or`, the bool
  ## literals `true`/`false`, and falls back to *true* for anything more
  ## complex — the conservative choice, since a false negative here removes
  ## a valid dep from the build graph while a false positive only schedules
  ## a file that the semantic checker will then ignore.
  var n = n
  if n.kind == ParLe:
    case n.exprKind
    of CallX, CmdX, CallstrlitX, InfixX, PrefixX:
      inc n
      if n.kind != Ident:
        return true
      let head = pool.strings[n.litId]
      inc n
      case head
      of "defined":
        if n.kind == Ident:
          result = config.isDefined(pool.strings[n.litId])
        elif n.kind == Symbol:
          var name = pool.syms[n.symId]
          extractBasename(name)
          result = config.isDefined(name)
        else:
          result = true
      of "not":
        result = not evalDepCond(config, n)
      of "and":
        result = evalDepCond(config, n)
        skip n
        if result: result = evalDepCond(config, n)
      of "or":
        result = evalDepCond(config, n)
        skip n
        if not result: result = evalDepCond(config, n)
      else:
        result = true  # unknown call — assume true
    of NotX:
      inc n
      result = not evalDepCond(config, n)
    of AndX:
      inc n
      result = evalDepCond(config, n)
      skip n
      if result: result = evalDepCond(config, n)
    of OrX:
      inc n
      result = evalDepCond(config, n)
      skip n
      if not result: result = evalDepCond(config, n)
    of ParX:
      # `(par X)` is just parenthesised grouping — descend into the body.
      # Without this, `not (a or b)` evaluates `not (par ...)` as the
      # conservative-true fallback, and the negation flips to false,
      # silently dropping the conditional `import` from the build graph.
      inc n
      result = evalDepCond(config, n)
    else:
      result = true  # unknown shape — assume true
  elif n.kind == Ident:
    case pool.strings[n.litId]
    of "true": result = true
    of "false": result = false
    else: result = true  # bare identifier we cannot evaluate — assume true
  else:
    result = true

proc whenMarkerHolds(c: DepContext; x: Cursor): bool =
  ## `(when COND COND ...)` — implicit AND across the children. All must hold
  ## for the import to be live. An empty `(when)` (legacy form, no children)
  ## is treated as live so older deps files keep working.
  assert x.kind == ParLe and x.stmtKind == WhenS
  var inner = x
  inner.into WhenS:
    while inner.hasMore:
      if not evalDepCond(c.config, inner):
        while inner.hasMore: skip inner  # mop-up before early-exit (return bypasses epilogue)
        return false
      skip inner
  result = true

proc processImport(c: var DepContext; it: var Cursor; current: Node) =
  let info = it.info
  var x = it
  skip it
  x.into:  # (import …)
    # Conditional imports carry a `(when COND...)` marker child. If we can
    # statically prove the condition is false against the active set of
    # `defined(...)` symbols, skip the import entirely. Otherwise step over
    # the marker and process the import normally — the conservative direction
    # for cross-compilation and for conditions we cannot evaluate.
    if x.stmtKind == WhenS:
      if not whenMarkerHolds(c, x):
        return
      skip x, SkipCond
    while x.hasMore:
      var isCyclic = false
      if x.kind == ParLe and x.exprKind == PragmaxX:
        var y = x
        inc y
        skip y
        if y.substructureKind == PragmasU:
          inc y
          if y.kind == Ident and pool.strings[y.litId] == "cyclic":
            isCyclic = true

      var files: seq[ImportedFilename] = @[]
      var hasError = false
      if isCyclic:
        # Manually parse the pragmax: enter it, parse the inner filename, skip the pragma
        x.into PragmaxX:
          filenameVal(x, files, hasError, allowAs = false)
          skip x, SkipPragmas # skip (pragmas cyclic)
          while x.hasMore: skip x, SkipFull
      else:
        filenameVal(x, files, hasError, allowAs = true)
      if hasError:
        discard "ignore wrong `import` statement"
      elif isCyclic:
        for f in files:
          importCyclicModule c, f.path, info, current
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
  var files: seq[ImportedFilename] = @[]
  var hasError = false
  x.into:  # (fromimport …) / (importexcept …)
    if x.stmtKind == WhenS:
      if not whenMarkerHolds(c, x):
        return
      skip x, SkipCond  # step past conditional marker, same as processImport
    filenameVal(x, files, hasError, allowAs = true)
    while x.hasMore: skip x  # (the rest of the children are the included/excluded names; processed elsewhere)
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
  it.into:  # (build …)
    while it.hasMore:
      assert it.exprKind == TupX
      var x = it
      skip it
      x.into TupX:
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
        while x.hasMore: skip x
        c.toBuild.add CFile(name: path, obj: obj, customArgs: args)

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
      n.into:  # (passL …)
        while n.hasMore:
          assert n.kind == StringLit
          c.passL.add pool.strings[n.litId]
          inc n
    elif n.tagId == TagId(PassCP):
      n.into:  # (passC …)
        while n.hasMore:
          assert n.kind == StringLit
          c.passC.add pool.strings[n.litId]
          inc n
    else:
      skip n
  else:
    #echo "IGNORING ", toString(n, false)
    skip n

proc processDeps(c: var DepContext; n: Cursor; current: Node) =
  var n = n
  if n.kind == ParLe and pool.tags[n.tagId] == "stmts":
    n.into:
      while n.hasMore:
        processDep c, n, current

proc getLastModTime(path: string): int64 =
  ## `getLastModificationTime` raises on transient I/O errors. We only use
  ## the result for staleness comparisons, so any failure should fall through
  ## to "rebuild needed" — returning -1 makes that automatic: `-1 > anything`
  ## is false (so we don't skip rebuilds), and `-1 == -1` (when both paths
  ## fail) is also not `>`, so we still rebuild.
  try:
    when defined(nimony):
      result = getLastModificationTime(path)
    else:
      result = times.toUnix(getLastModificationTime(path))
  except:
    result = -1'i64

proc execNifler(c: var DepContext; f: FilePair) =
  # File can be a .nif file, if so, we don't need to run nifler.
  if f.nimFile.endsWith(".nif"):
    return
  let preserveDocs = c.cmd == DoDoc
  let output = c.config.parsedFile(f, preserveDocs)
  let depsFile = c.config.depsFile(f, preserveDocs)
  let srcTime = getLastModTime(f.nimFile)
  if not c.forceRebuild and semos.fileExists(output) and
      semos.fileExists(f.nimFile) and getLastModTime(output) > srcTime and
      semos.fileExists(depsFile) and getLastModTime(depsFile) > srcTime:
    discard "nothing to do"
  else:
    let docsFlag = if preserveDocs: " --docs" else: ""
    let cmd = quoteShell(c.nifler) & " --portablePaths --deps" & docsFlag & " parse " &
      quoteShell(f.nimFile) & " " & quoteShell(output)
    exec cmd

proc importSystem(c: var DepContext; current: Node) =
  let p = c.toPair(stdlibFile("std/system.nim"))
  var existingNode = c.processedModules.getOrDefault(p.modname, -1)
  if existingNode == -1:
    #echo "NIFLING ", p.nimFile, " -> ", c.config.parsedFile(p)
    execNifler c, p
    var imported = Node(files: @[p], id: c.nodes.len, parent: current.id, isSystem: true,
                        shards: @[p.modname])
    c.nodes.add imported
    c.processedModules[p.modname] = imported.id
    traverseDeps c, p, imported
    existingNode = imported.id
  current.deps.add existingNode

proc traverseDeps(c: var DepContext; p: FilePair; current: Node) =
  let depsFile: string
  if not c.isGeneratingFinal:
    execNifler c, p
    depsFile = c.config.depsFile(p, c.cmd == DoDoc)
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
  result = onRaiseQuit relativePath(result, onRaiseQuit os.getCurrentDir())

proc defineNiflerCmd(b: var Builder; nifler: string; preserveDocs = false) =
  b.withTree "cmd":
    b.addSymbolDef "nifler"
    b.addStrLit nifler
    b.addStrLit "--portablePaths"
    b.addStrLit "--deps"
    if preserveDocs:
      b.addStrLit "--docs"
    b.addStrLit "parse"
    b.addKeyw "input"
    b.addKeyw "output"

proc defineHexerCmds(b: var Builder; hexer: string; bits: int; bigEndian: bool) =
  let cpuFlag = if bigEndian: "--cpu:be" else: "--cpu:le"
  b.withTree "cmd":
    b.addSymbolDef "hexer"
    b.addStrLit hexer
    b.addStrLit "c"
    b.addStrLit "--bits:" & $bits
    b.addStrLit cpuFlag
    b.addKeyw "args"
    b.withTree "input":
      b.addIntLit 0

  b.withTree "cmd":
    b.addSymbolDef "dce"
    b.addStrLit hexer
    b.addStrLit "d"
    b.addStrLit "--bits:" & $bits
    b.addStrLit cpuFlag
    b.addKeyw "args"
    b.withTree "input":
      b.addIntLit 0
      b.addIntLit -1  # all inputs

  # Split DCE: liveness phase (single, fast) + per-module emit (parallel).
  # `dceLive` reads every module's .dce.nif analysis, computes the global
  # live set + generic-resolve table, writes a shared .live.nif.
  b.withTree "cmd":
    b.addSymbolDef "dceLive"
    b.addStrLit hexer
    b.addStrLit "dl"
    b.addStrLit "--bits:" & $bits
    b.addStrLit cpuFlag
    b.addKeyw "args"
    b.withTree "input":
      b.addIntLit 0
      b.addIntLit -1
    b.addKeyw "output"

  # `dceEmit` rewrites one .x.nif into .c.nif using the shared .live.nif.
  # Independent across modules, so nifmake can run them in parallel.
  # Output path is derived from `--outdir` + input modname (mirrors how
  # `hexer c` derives outputs), so no explicit output slot here.
  b.withTree "cmd":
    b.addSymbolDef "dceEmit"
    b.addStrLit hexer
    b.addStrLit "de"
    b.addStrLit "--bits:" & $bits
    b.addStrLit cpuFlag
    b.addKeyw "args"
    b.withTree "input":
      b.addIntLit 0  # M.x.nif
      b.addIntLit 1  # main.live.nif


proc generateDocBuildFile(c: DepContext): string =
  ## Doc backend: each per-module `dagon module` rule produces both a `.html`
  ## and a `.docidx` sidecar; the trailing `dagon link` task gathers all the
  ## sidecars into the global `theindex.html`. The HTML lands at a friendly
  ## directory-mirrored path (`htmldocs/std/system.html` etc.); the `.docidx`
  ## stays under nimcache to avoid user-visible churn.
  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".doc.build.nif"
  var b = nifbuilder.open(result)
  defer: b.close()

  # Slash-normalise so the build file (and the args we pass to dagon) is
  # byte-identical across OSes. `deriveRelpath` does prefix matching on
  # roots, so callers downstream must agree on the separator.
  let projectRoot = toUnixPath(absoluteParentDir(c.rootNode.files[0].nimFile))
  let stdlibRoot = toUnixPath(stdlibDir())
  let rootFlags = "--projectRoot:" & projectRoot & " --stdlibRoot:" & stdlibRoot

  b.addHeader()
  b.withTree "stmts":
    let dagon = findTool("dagon")

    let outdir = docOutDir(c.config)
    b.withTree "cmd":
      b.addSymbolDef "dagon"
      b.addStrLit dagon
      b.addStrLit "--projectRoot:" & projectRoot
      b.addStrLit "--stdlibRoot:" & stdlibRoot
      b.addStrLit "--outdir:" & outdir
      b.addStrLit "module"
      b.addKeyw "args"

    b.withTree "cmd":
      b.addSymbolDef "doclink"
      b.addStrLit dagon
      b.addStrLit "--projectRoot:" & projectRoot
      b.addStrLit "--stdlibRoot:" & stdlibRoot
      b.addStrLit "--outdir:" & outdir
      b.addStrLit "link"
      b.addKeyw "args"

    for v in c.nodes:
      if v.plugin.len > 0: continue
      let semFile = c.config.semmedFile(v.files[0], v.plugin, preserveDocs = true)
      let htmlOut = c.config.docFile(v.files[0], projectRoot, stdlibRoot)
      let idxOut = c.config.docIdxFile(v.files[0])
      b.withTree "do":
        b.addIdent "dagon"
        b.withTree "args":
          b.addStrLit semFile
          b.addStrLit htmlOut
          b.addStrLit idxOut
        b.withTree "input":
          b.addStrLit semFile
        b.withTree "output":
          b.addStrLit htmlOut
        b.withTree "output":
          b.addStrLit idxOut

    let indexOut = c.config.indexHtmlFile()
    b.withTree "do":
      b.addIdent "doclink"
      b.withTree "args":
        b.addStrLit indexOut
        for v in c.nodes:
          if v.plugin.len > 0: continue
          b.addStrLit c.config.docIdxFile(v.files[0])
      for v in c.nodes:
        if v.plugin.len > 0: continue
        b.withTree "input":
          b.addStrLit c.config.docIdxFile(v.files[0])
      b.withTree "output":
        b.addStrLit indexOut
  discard rootFlags  # silence unused warning if logging is later removed

# Per-shard helper: produces the canonical `.x.nif` and `.dce.nif`
# paths for one shard. Shard 0 of the root module is backend-specific
# (the --isMain build sits in `backendDir/`). Every other shard
# (whether sibling of root or any non-root module's only shard) lands
# in the shared nifcache so non-main compilations don't see the
# --isMain artifact.
proc shardXAndDce(c: DepContext; nodeIdx, shardIdx: int; shard, backendDir: string):
    (string, string) =
  if nodeIdx == 0 and shardIdx == 0:
    (backendDir / shard & ".x.nif", backendDir / shard & ".dce.nif")
  else:
    (c.config.nifcachePath / shard & ".x.nif",
      c.config.nifcachePath / shard & ".dce.nif")

proc generateFinalBuildFile(c: DepContext; commandLineArgsNifc: string; passC, passL: string): string =
  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".final.build.nif"
  var b = nifbuilder.open(result)
  defer: b.close()

  b.addHeader()
  b.withTree "stmts":
    # Command definitions
    let nifc = findTool("nifc")
    let hexer = findTool("hexer")

    # Command for nifc (code generation)
    b.withTree "cmd":
      b.addSymbolDef "nifc"
      b.addStrLit nifc
      b.addStrLit $c.config.backend
      b.addStrLit "--compileOnly"
      b.addStrLit "--bits:" & $c.config.bits
      b.addKeyw "args"
      if commandLineArgsNifc.len > 0:
        for arg in commandLineArgsNifc.split(' '):
          if arg.len > 0:
            b.addStrLit arg
      b.addKeyw "input"

    # Command for hexer
    defineHexerCmds(b, hexer, c.config.bits, platform.CPU[c.config.targetCPU].endian == bigEndian)

    # Command for C/LLVM compiler (object files)
    b.withTree "cmd":
      b.addSymbolDef "cc"
      if c.config.backend == backendLLVM:
        b.addStrLit "clang"
      else:
        b.addStrLit c.config.cc
      b.addStrLit "-c"
      # Suppress visibility-attribute warnings from mimalloc etc. (GCC/Clang)
      b.addStrLit "-Wno-attributes"
      # Note on TLS for clang/Windows: clang emits native PE TLS by default,
      # which is what we want — `__thread` access compiles to a single
      # `gs:0x58` load instead of a `__emutls_get_address` call. ld.bfd
      # (mingw-w64's default linker) mishandles this and produces binaries
      # that segfault on first TLS access; we paper over that at link time
      # by switching to LLD, see the link cmd below. No `-femulated-tls`
      # here.
      # Add -fPIC for shared libraries
      if c.config.appType == appLib:
        b.addStrLit "-fPIC"
      if passC.len > 0:
        for arg in passC.split(' '):
          if arg.len > 0:
            b.addStrLit arg
      for i in c.passC:
        b.addStrLit i
      if c.config.backend == backendC:
        b.addStrLit "-I" & rootPath(c)
      b.addKeyw "args"
      b.addKeyw "input"
      b.addStrLit "-o"
      b.addKeyw "output"

    # Command for linking/archiving
    if c.cmd in {DoCompile, DoRun}:
      case c.config.appType
      of appStaticLib:
        # Static library: use ar to create archive
        b.withTree "cmd":
          b.addSymbolDef "link"
          b.addStrLit "ar"
          b.addStrLit "rcs"
          b.addKeyw "output"
          b.withTree "input":
            b.addIntLit 0
            b.addIntLit -1  # all inputs
      of appLib:
        # Dynamic library: use linker with -shared
        b.withTree "cmd":
          b.addSymbolDef "link"
          b.addStrLit c.config.linker
          b.addStrLit "-shared"
          b.addStrLit "-o"
          b.addKeyw "output"
          b.withTree "input":
            b.addIntLit 0
            b.addIntLit -1  # all inputs
          b.withTree "argsext":
            b.addStrLit ".linker.args"
          # See cc command above: clang's native PE TLS is broken when laid
          # out by ld.bfd. LLD lays out the .tls$ section the way the loader
          # actually expects, so native TLS survives to runtime — and we
          # keep the fast `gs:0x58` path instead of falling back to
          # emulated-TLS function calls.
          if extractCCKey(c.config.linker) == "clang" and c.config.targetOS == osWindows:
            b.addStrLit "-fuse-ld=lld"
          if passL.len > 0:
            for arg in passL.split(' '):
              if arg.len > 0:
                b.addStrLit arg
          for i in c.passL:
            b.addStrLit i
      of appConsole, appGui:
        # Executable: use linker normally
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
          if extractCCKey(c.config.linker) == "clang" and c.config.targetOS == osWindows:
            b.addStrLit "-fuse-ld=lld"
          if passL.len > 0:
            for arg in passL.split(' '):
              if arg.len > 0:
                b.addStrLit arg
          for i in c.passL:
            b.addStrLit i

    # Build rules
    if c.cmd in {DoCompile, DoRun}:
      let backend = c.rootNode.files[0].modname  # DCE and after are main-specific
      let backendDir = c.config.nifcachePath / backend
      let liveFile = backendDir / c.rootNode.files[0].modname & ".live.nif"

      # Split DCE — phase 1: collect every module's per-shard .dce.nif
      # analyses, compute the global live set + generic-instance resolve
      # table, write the shared <main>.live.nif. Single small serial node.
      b.withTree "do":
        b.addIdent "dceLive"
        for i, n in pairs c.nodes:
          for shardIdx, shard in pairs n.shards:
            let (_, dceFile) = shardXAndDce(c, i, shardIdx, shard, backendDir)
            b.withTree "input":
              b.addStrLit dceFile
        b.withTree "output":
          b.addStrLit liveFile

      # Split DCE — phase 2: per-shard emit. Each `(do dceEmit ...)` is
      # independent (all share live.nif as a read-only input), so nifmake
      # parallelises them across cores. Each shard gets its own .c.nif.
      for i, n in pairs c.nodes:
        for shardIdx, shard in pairs n.shards:
          let (xFile, _) = shardXAndDce(c, i, shardIdx, shard, backendDir)
          b.withTree "do":
            b.addIdent "dceEmit"
            b.withTree "args":
              b.addStrLit "--outdir:" & backendDir
            b.withTree "input":
              b.addStrLit xFile
            b.withTree "input":
              b.addStrLit liveFile
            b.withTree "output":
              b.addStrLit c.config.shardedNifcFile(shard, backend)


      # Link executable: gather every shard's object file.
      b.withTree "do":
        b.addIdent "link"
        var objFiles = initHashSet[string]()
        for cfile in c.toBuild:
          let obj = c.config.nifcachePath / backend / cfile.obj
          if not objFiles.containsOrIncl(obj):
            b.withTree "input":
              b.addStrLit obj
        for v in c.nodes:
          for shard in v.shards:
            let obj = c.config.shardedObjFile(shard, backend)
            if not objFiles.containsOrIncl(obj):
              b.withTree "input":
                b.addStrLit obj
        b.withTree "output":
          b.addStrLit c.config.exeFile(c.rootNode.files[0], backend)

      objFiles = initHashSet[string]()
      # Build object files from C files with custom args
      for cfile in c.toBuild:
        let obj = c.config.nifcachePath / backend / cfile.obj
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
        for shardIdx, shard in pairs v.shards:
          let obj = c.config.shardedObjFile(shard, backend)
          if not objFiles.containsOrIncl(obj):
            b.withTree "do":
              b.addIdent "cc"
              b.withTree "input":
                b.addStrLit c.config.shardedGenFile(shard, backend)
              b.withTree "output":
                b.addStrLit obj

          # Build C/LLVM IR files from .c.nif files. --isMain is exclusive
          # to the root module's shard 0 — that's the one that generates the
          # `main()` entry point. Sibling shards of the root module are
          # plain non-main translation units.
          b.withTree "do":
            b.addIdent "nifc"
            b.withTree "args":
              b.addStrLit "--nimcache:" & backendDir
            if i == 0 and shardIdx == 0:
              b.withTree "args":
                b.addStrLit "--isMain"
            b.withTree "input":
              b.addStrLit c.config.shardedNifcFile(shard, backend)
            b.withTree "output":
              b.addStrLit c.config.shardedGenFile(shard, backend)

          # Build .x.nif files from .s.nif files via hexer.
          # Same `--isMain` rule as nifc: only root-module shard 0.
          b.withTree "do":
            b.addIdent "hexer"
            if i == 0 and shardIdx == 0:
              b.withTree "args":
                b.addStrLit "--isMain"
              b.withTree "args":
                b.addStrLit "--app:" & $c.config.appType
              b.withTree "args":
                b.addStrLit "--outdir:" & backendDir
            b.withTree "input":
              b.addStrLit c.config.shardedSemmedFile(shard, v.plugin)
            # Cross-module hexer dep: imports' `.s.idx.nif` (the umbrella
            # index, unchanged across sharding — per user 2026-05-20 it
            # carries just the interface checksum). One `.s.idx.nif` per
            # imported logical module, not per shard.
            var seenImports = initHashSet[string]()
            for depIdx in v.deps:
              let idxFile = c.config.indexFile(c.nodes[depIdx].files[0], c.nodes[depIdx].plugin)
              if not seenImports.containsOrIncl(idxFile):
                b.withTree "input":
                  b.addStrLit idxFile
            let (xOut, dceOut) = shardXAndDce(c, i, shardIdx, shard, backendDir)
            b.withTree "output":
              b.addStrLit xOut
            # `.dce.nif` emitted alongside `.x.nif` by `bin/hexer c`. Listed
            # here so nifmake tracks it as a real artifact and orders
            # `dceLive` after every per-shard hexer.
            b.withTree "output":
              b.addStrLit dceOut

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
      # Module files are passed as args (primary first, then cyclic members)
      b.addStrLit c.config.parsedFile(v.files[0], c.cmd == DoDoc)
      for idx in v.cyclicFiles:
        b.addStrLit c.config.parsedFile(v.files[idx], c.cmd == DoDoc)
    # Input: parsed file
    var seenDeps = initHashSet[string]()
    for f in v.files:
      let pf = c.config.parsedFile(f, c.cmd == DoDoc)
      if not seenDeps.containsOrIncl(pf):
        b.withTree "input":
          b.addStrLit pf
    # Input: dependencies
    for i in v.deps:
      let idxFile = c.config.indexFile(c.nodes[i].files[0], c.nodes[i].plugin, c.cmd == DoDoc)
      if not seenDeps.containsOrIncl(idxFile):
        b.withTree "input":
          b.addStrLit idxFile
    # Input: cached config file
    b.withTree "input":
      b.addStrLit c.config.cachedConfigFile()
    # Outputs: semmed file and index file for primary module
    let docMode = c.cmd == DoDoc
    b.withTree "output":
      b.addStrLit c.config.semmedFile(v.files[0], v.plugin, docMode)
    b.withTree "output":
      b.addStrLit c.config.indexFile(v.files[0], v.plugin, docMode)
    # Outputs for cyclic group members:
    for idx in v.cyclicFiles:
      b.withTree "output":
        b.addStrLit c.config.semmedFile(v.files[idx], v.plugin, docMode)
      b.withTree "output":
        b.addStrLit c.config.indexFile(v.files[idx], v.plugin, docMode)

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

proc generateFrontendBuildFile(c: DepContext; commandLineArgs: string; cmd: Command): string =
  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".build.nif"
  var b = nifbuilder.open(result)
  defer: b.close()

  b.addHeader()
  b.withTree "stmts":
    # Command definitions
    defineNiflerCmd(b, c.nifler, preserveDocs = c.cmd == DoDoc)

    b.withTree "cmd":
      b.addSymbolDef "nimsem"
      b.addStrLit c.nimsem
      if c.config.baseDir.len > 0:
        b.addStrLit "--base:" & quoteShell(c.config.baseDir)
      if commandLineArgs.len > 0:
        for arg in commandLineArgs.split(' '):
          if arg.len > 0:
            b.addStrLit arg
      b.addStrLit "m"
      b.addKeyw "args"
      # Module files are passed via (args) in each (do nimsem) block

    if cmd == DoCheck:
      b.withTree "cmd":
        b.addSymbolDef "idetools"
        b.addStrLit c.nimsem
        if c.config.baseDir.len > 0:
          b.addStrLit "--base:" & quoteShell(c.config.baseDir)
        if commandLineArgs.len > 0:
          for arg in commandLineArgs.split(' '):
            if arg.len > 0:
              b.addStrLit arg
        b.addStrLit "idetools"
        b.addKeyw "args"
        b.withTree "input":
          b.addIntLit 0
          b.addIntLit -1 # all inputs

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
        let f = c.config.parsedFile(v.files[i], c.cmd == DoDoc)
        if not seenFiles.containsOrIncl(f):
          let nimFile = v.files[i].nimFile
          if nimFile.endsWith(".nif"):
            continue
          b.withTree "do":
            b.addIdent "nifler"
            b.withTree "input":
              b.addStrLit nimFile
            b.withTree "output":
              b.addStrLit f

    if cmd == DoCheck and c.config.toTrack.mode != TrackNone:
      b.withTree "do":
        b.addIdent "idetools"
        for v in c.nodes:
          let s = c.config.semmedFile(v.files[0], v.plugin)
          b.withTree "input":
            b.addStrLit s

proc generateCachedConfigFile(c: DepContext; passC, passL: string) =
  let path = c.config.cachedConfigFile()
  let configStr = c.config.getOptionsAsOneString() & " " & c.rootNode.files[0].nimFile &
                  " --passC:" & passC & " --passL:" & passL

  let needUpdate = if semos.fileExists(path) and not c.forceRebuild:
                     configStr != onRaiseQuit(readFile(path))
                   else:
                     true
  if needUpdate:
    onRaiseQuit writeFile(path, configStr)

proc initDepContext(config: sink NifConfig; project, nifler: string; isFinal, forceRebuild: bool; moduleFlags: set[ModuleFlag]; cmd: Command): DepContext =
  result = DepContext(nifler: nifler, config: config, rootNode: nil, includeStack: @[],
    forceRebuild: forceRebuild, moduleFlags: moduleFlags, nimsem: findTool("nimsem"),
    cmd: cmd, isGeneratingFinal: isFinal)
  let p = result.toPair(project)
  let root = Node(files: @[p], id: 0, parent: -1, active: 0, isSystem: IsSystem in moduleFlags,
                  shards: @[p.modname])
  result.rootNode = root
  result.nodes.add root
  result.processedModules[p.modname] = 0
  traverseDeps result, p, root

proc buildGraphForEval*(config: NifConfig; mainNifFile: string; dependencyNifFiles: seq[string];
    flags: set[BuildFlag]; moduleFlags: set[ModuleFlag]) =
  ## Build graph starting from already-processed .nif files instead of .nim files
  const requiredStdlibModules = [
    "std/writenif.nim", "std/syncio.nim", "std/math.nim", "std/formatfloat.nim"
  ]
  const requiredObjFiles = ["static.o"]

  # Generate a simplified build file that works with .nif files
  let buildFile = config.nifcachePath / splitModulePath(mainNifFile).name & ".exec.build.nif"
  var b = nifbuilder.open(buildFile)

  b.addHeader()
  b.withTree "stmts":
    # Command definitions (reuse existing logic)
    defineNiflerCmd(b, findTool("nifler"))

    b.withTree "cmd":
      b.addSymbolDef "nimsem"
      b.addStrLit findTool("nimsem")
      if config.baseDir.len > 0:
        b.addStrLit "--base:" & quoteShell(config.baseDir)
      # Match the OUTER frontend build file's `--cc:VAL` so nifmake's
      # per-cmd staleness check sees the same argv on both sides — without
      # this the static-eval helper's nifmake decides the existing
      # sysvq0asl.s.nif is stale and tries to rewrite it, and on Windows
      # the open fails because the outer (paused) nimsem still has the
      # file mmap'd via nifreader.
      if config.ccKey.len > 0:
        b.addStrLit "--cc:" & quoteShell(config.cc)
      b.addStrLit "m"
      b.addKeyw "args"
      b.withTree "input":
        b.addIntLit 0  # main parsed file

    b.withTree "cmd":
      b.addSymbolDef "nifc"
      b.addStrLit findTool("nifc")
      b.addStrLit "c"
      b.addStrLit "--compileOnly"
      b.addKeyw "args"
      b.addKeyw "input"

    defineHexerCmds(b, findTool("hexer"), config.bits, platform.CPU[config.targetCPU].endian == bigEndian)

    b.withTree "cmd":
      b.addSymbolDef "cc"
      b.addStrLit config.cc
      b.addStrLit "-c"
      b.addStrLit "-Wno-attributes"
      if config.baseDir.len > 0:
        b.addStrLit "-I" & config.baseDir
      b.addKeyw "args"
      b.addKeyw "input"
      b.addStrLit "-o"
      b.addKeyw "output"

    b.withTree "cmd":
      b.addSymbolDef "link"
      b.addStrLit config.linker
      b.addStrLit "-o"
      b.addKeyw "output"
      b.withTree "input":
        b.addIntLit 0
        b.addIntLit -1  # all inputs
      b.withTree "argsext":
        b.addStrLit ".linker.args"
      # Clang on MinGW: native PE TLS code-gen + LLD lays out `.tls$` so the
      # loader sees it correctly; ld.bfd does not, leading to startup segfaults.
      if extractCCKey(config.linker) == "clang" and config.targetOS == osWindows:
        b.addStrLit "-fuse-ld=lld"

    # Collect all .nif files for DCE analysis
    var allNifFiles: seq[string] = @[]

    for module in requiredStdlibModules:
      let writenifNimFile = stdlibFile(module)
      let writenifSuffix = moduleSuffix(writenifNimFile, config.paths)
      allNifFiles.add(writenifSuffix)
      let writenifNifFile = config.nifcachePath / writenifSuffix & ".p.nif"
      let writenifSemmedFile = config.nifcachePath / writenifSuffix & ".s.nif"
      let writenifHexedFile = config.nifcachePath / writenifSuffix & ".x.nif"

      # Process writenif.nim with nifler to generate .nif file
      b.withTree "do":
        b.addIdent "nifler"
        b.withTree "input":
          b.addStrLit writenifNimFile
        b.withTree "output":
          b.addStrLit writenifNifFile

      # Process writenif .nif file with nimsem for semantic analysis
      b.withTree "do":
        b.addIdent "nimsem"
        b.withTree "input":
          b.addStrLit writenifNifFile
        b.withTree "output":
          b.addStrLit writenifSemmedFile

      b.withTree "do":
        b.addIdent "hexer"
        b.withTree "input":
          b.addStrLit writenifSemmedFile
        b.withTree "output":
          b.addStrLit writenifHexedFile


    var allRequiredStdlibModules = initHashSet[string]()
    for f in allNifFiles:
      allRequiredStdlibModules.incl f

    for depNifFile in dependencyNifFiles:
      let depName = depNifFile.splitModulePath.name
      if depName in allRequiredStdlibModules:
        continue
      allNifFiles.add(depName)
      let depHexedFile = config.nifcachePath / depName & ".s.nif"

      # Process dependency .nif file with hexer first
      b.withTree "do":
        b.addIdent "hexer"
        b.withTree "input":
          b.addStrLit depNifFile
        b.withTree "output":
          b.addStrLit depHexedFile

    # Build rules for main file
    let mainName = mainNifFile.splitModulePath.name
    allNifFiles.add(mainName)
    let mainHexedFile = config.nifcachePath / mainName & ".x.nif"

    # Process main .nif file with hexer first
    b.withTree "do":
      b.addIdent "hexer"
      b.withTree "args":
        b.addStrLit "--isMain"
      b.withTree "args":
        b.addStrLit "--app:" & $config.appType
      b.withTree "input":
        b.addStrLit mainNifFile
      b.withTree "output":
        b.addStrLit mainHexedFile

    b.withTree "do":
      b.addIdent "dce"
      for nifFile in allNifFiles:
        b.withTree "input":
          b.addStrLit config.nifcachePath / nifFile & ".x.nif"
        b.withTree "output":
          b.addStrLit config.nifcachePath / nifFile & ".c.nif"

    var objFiles: seq[string] = @[]
    for objFile in requiredObjFiles: objFiles.add(config.nifcachePath / objFile)
    for i, nifFile in pairs allNifFiles:
      b.withTree "do":
        b.addIdent "nifc"
        if i == 0:
          b.withTree "args":
            b.addStrLit "--isMain"
        b.withTree "input":
          b.addStrLit config.nifcachePath / nifFile & ".c.nif"
        b.withTree "output":
          b.addStrLit config.nifcachePath / nifFile & ".c"

      let objFile = config.nifcachePath / nifFile & ".o"
      b.withTree "do":
        b.addIdent "cc"
        b.withTree "input":
          b.addStrLit config.nifcachePath / nifFile & ".c"
        b.withTree "output":
          b.addStrLit objFile
      objFiles.add(objFile)

    # Link all object files to create executable
    let exeFile = config.nifcachePath / "main" & (when defined(windows): ".exe" else: "")
    b.withTree "do":
      b.addIdent "link"
      for objFile in objFiles:
        b.withTree "input":
          b.addStrLit objFile
      b.withTree "output":
        b.addStrLit exeFile
  b.close()

  # Execute the build using nifmake
  let nifmakeCmd = quoteShell(findTool("nifmake")) &
    (if ForceRebuild in flags: " --force" else: "") &
    " --base:" & quoteShell(config.baseDir) &
    " -j run " & quoteShell(buildFile)
  exec(nifmakeCmd)
  exec(exeFile)

proc buildGraph*(config: sink NifConfig; project: string;
    flags: set[BuildFlag];
    commandLineArgs, commandLineArgsNifc: string; moduleFlags: set[ModuleFlag]; cmd: Command;
    passC, passL: string, executableArgs: string) =
  let nifler = findTool("nifler")
  let nifmake = findTool("nifmake")
  let forceRebuild = ForceRebuild in flags

  if config.compat:
    let cfgNif = config.nifcachePath / moduleSuffix(project, []) & ".cfg.nif"
    exec quoteShell(nifler) & " config " & quoteShell(project) & " " &
      quoteShell(cfgNif)
    parseNifConfig cfgNif, config

  var c = initDepContext(config, project, nifler, false, forceRebuild, moduleFlags, cmd)
  generateCachedConfigFile c, passC, passL
  let buildFilename = generateFrontendBuildFile(c, commandLineArgs, cmd)
  #echo "run with: nifmake run ", buildFilename
  when defined(windows) and not defined(nimony):
    putEnv("CC", "gcc")
    putEnv("CXX", "g++")
  let nifmakeCommand = quoteShell(nifmake) &
    (if forceRebuild: " --force" else: "") &  # Use generic force flag
    (if Profile in flags: " --profile" else: "") &
    (if Report in flags: " --report" else: "") &
    " --base:" & quoteShell(config.baseDir) &
    " -j run "
  exec nifmakeCommand & quoteShell(buildFilename)

  if cmd == DoDoc:
    c = initDepContext(config, project, nifler, true, forceRebuild, moduleFlags, cmd)
    let docCacheDir = c.config.nifcachePath / "docs"
    let docOut = docOutDir(c.config)
    let projectRoot = toUnixPath(absoluteParentDir(c.rootNode.files[0].nimFile))
    let stdlibRoot = toUnixPath(stdlibDir())
    onRaiseQuit createDir(path(docCacheDir))
    onRaiseQuit createDir(path(docOut))
    # Pre-create the per-module subdirectories under outdir. dagon writes to
    # `<outdir>/<relpath>` and won't auto-mkdir intermediate components.
    for v in c.nodes:
      if v.plugin.len > 0: continue
      let relp = deriveRelpath(v.files[0].nimFile, projectRoot, stdlibRoot)
      let parent = docOut / parentDir(relp)
      if parent.len > 0 and parent != docOut:
        onRaiseQuit createDir(path(parent))
    let buildDocFilename = generateDocBuildFile(c)
    exec nifmakeCommand & quoteShell(buildDocFilename)
    return

  if cmd != DoCheck:
    # Parse `.s.deps.nif`.
    # It is generated by nimsem and doesn't contains modules imported under `when false:`.
    # https://github.com/nim-lang/nimony/issues/985
    c = initDepContext(config, project, nifler, true, forceRebuild, moduleFlags, cmd)
    let backend = c.config.nifcachePath / c.rootNode.files[0].modname
    onRaiseQuit createDir(path(backend))
    # Sharding pass — runs inline between the frontend nifmake (which has
    # already produced the umbrella `.s.nif` per module) and the backend
    # nifmake. For modules whose source basename matches the gate
    # (currently only sem.nim, see `sharding.shouldShard`), partitions
    # the umbrella into `<modname>_<i>.s.nif` files; the backend build
    # rules iterate `Node.shards` so each shard is a parallel
    # hexer/dceEmit/nifc/cc invocation. The umbrella file is preserved
    # untouched so cross-module / cross-shard symbol lookups via
    # `programs.load` keep working without any sharding awareness.
    for n in c.nodes:
      if n.plugin.len > 0: continue
      if n.files.len == 0: continue
      if not sharding.shouldShard(n.files[0].nimFile): continue
      let umbrellaPath = c.config.semmedFile(n.files[0], n.plugin)
      if not semos.fileExists(umbrellaPath): continue
      n.shards = sharding.shardSemFile(umbrellaPath, n.files[0].modname)
    let buildFinalFilename = generateFinalBuildFile(c, commandLineArgsNifc, passC, passL)
    # Linkers (gcc/clang/ld/ar) don't auto-create the output directory.
    # When the user passes `--out:bin/foo` or `--outdir:bin`, materialise
    # `bin/` here. Nim does the same in `prepareToWriteOutput`.
    let exeOutPath = c.config.exeFile(c.rootNode.files[0], c.rootNode.files[0].modname)
    let exeOutDir = exeOutPath.parentDir
    if exeOutDir.len > 0:
      onRaiseQuit createDir(path(exeOutDir))
    exec nifmakeCommand & quoteShell(buildFinalFilename)

  if Stats in flags:
    # Walk every source module in the dep graph and sum line counts. Counting
    # `\n` bytes in each `.nim` is cheap (sub-millisecond per file at this
    # scale); no caching needed since this only fires under `--stats`.
    var totalLines = 0
    var totalBytes = 0
    var nimFiles = 0
    var seen = initHashSet[string]()
    for v in c.nodes:
      if v.plugin.len > 0: continue
      for f in v.files:
        if not f.nimFile.endsWith(".nim"): continue
        if seen.containsOrIncl(f.nimFile): continue
        if not semos.fileExists(f.nimFile): continue
        try:
          let s = readFile(f.nimFile)
          inc nimFiles
          totalBytes += s.len
          # Count newlines; treat a file with no trailing newline as
          # contributing one extra line for its last content line.
          var n = 0
          for ch in s:
            if ch == '\n': inc n
          if s.len > 0 and s[^1] != '\n': inc n
          totalLines += n
        except:
          discard
    echo "[stats] ", nimFiles, " modules, ",
         totalLines, " LOC, ", totalBytes, " bytes"

  if cmd != DoCheck:
    if cmd == DoRun:
      let backend = c.rootNode.files[0].modname
      exec c.config.exeFile(c.rootNode.files[0], backend) & executableArgs

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
import semos, nifconfig, nimony_model, semdata, langmodes
import ".." / gear2 / modnames
import ".." / lib / [tooldirs, platform, nifindexes, symparser, docpaths, argsfinder]
import ".." / models / nifindex_tags

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / nifreader as rd
from ".." / lib / nifcoreparse import parse

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
proc lengcFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".c.nif"
proc optimizedFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  ## Shoggoth rewrites `<modname>.c.nif` into this; `lengc` consumes
  ## it instead. The extra extension keeps the input intact and still extracts
  ## to the same `modname` (splitModulePath stops at the first dot), so lengc's
  ## derived output filename is unchanged.
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".oc.nif"

proc cFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".c"
proc llFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".ll"
proc asmFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  ## arkham rewrites `<modname>.c.nif` (or `.oc.nif`) into this typed asm-NIF.
  ## nifasm consumes the main module's file (by path) and resolves the dependent
  ## modules from disk by suffix, trying `<suffix>.asm.nif` then `<suffix>.nif`
  ## (see nifasm's `openForeignModule`). Each carries an embedded `(.index)` so
  ## no reindex pass is needed.
  let base = if backendDir.len > 0: config.nifcachePath / backendDir else: config.nifcachePath
  base / f.modname & ".asm.nif"
proc genFile(config: NifConfig; f: FilePair; backendDir: string = ""): string =
  case config.backend
  of backendC: config.cFile(f, backendDir)
  of backendLLVM: config.llFile(f, backendDir)
  of backendNative: config.asmFile(f, backendDir)
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

  BackendTool = object
    ## A `{.build(builder, tool[, args[, linkflags]]).}` custom-backend routing
    ## entry: the module `modFile` has its Leng IR piped through `toolName` (a
    ## program built by `builder` from `toolSrc`). Built like a plugin (on demand)
    ## but scheduled like a tool (a node in the nifmake DAG).
    builder: string    ## generic builder command, e.g. "nimony c" / "nim c"
    toolSrc: string    ## the tool's Nim source path
    toolName: string   ## derived exe basename (the nifmake command name)
    args: string       ## extra args forwarded to the tool invocation
    modFile: FilePair  ## the module whose `.c.nif` is routed through the tool
    linkFlags: string  ## optional per-file link flags scoped to this module's
                       ## object in the link manifest ("" = none)

  Bundle = object
    ## A `{.bundle(builder, tool[, args]).}` custom-linker entry: when any module
    ## supplies one it overrides the final link step. `toolName` (built by
    ## `builder` from `toolSrc`) is handed the project link manifest + `args`.
    builder: string
    toolSrc: string
    toolName: string
    args: string

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
    backendTools: seq[BackendTool]
    bundles: seq[Bundle]
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
  if n.isTagLit:
    case n.exprKind
    of CallX, CmdX, CallstrlitX, InfixX, PrefixX:
      inc n
      if not n.isIdent:
        return true
      let head = pool.strings[n.litId]
      inc n
      case head
      of "defined":
        if n.isIdent:
          result = config.isDefined(pool.strings[n.litId])
        elif n.isSymbol:
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
  elif n.isIdent:
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
  assert x.isTagLit and x.stmtKind == WhenS
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
      if x.isTagLit and x.exprKind == PragmaxX:
        var y = x
        inc y
        skip y
        if y.substructureKind == PragmasU:
          inc y
          if y.isIdent and pool.strings[y.litId] == "cyclic":
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

proc processBuild(c: var DepContext; it: var Cursor; current: Node) =
  it.into:  # (build …)
    while it.hasMore:
      assert it.exprKind == TupX
      var x = it
      skip it
      x.into TupX:
        assert x.isStringLit
        let typ = pool.strings[x.litId]
        inc x
        assert x.isStringLit
        let path = pool.strings[x.litId]
        inc x
        assert x.isStringLit
        let args = pool.strings[x.litId]
        inc x
        var linkFlags = ""        # optional 4th field (`.build` per-file link flags)
        if x.isStringLit:
          linkFlags = pool.strings[x.litId]
          inc x
        while x.hasMore: skip x
        if typ in ["C", "ObjC", "Cpp", "ObjCpp"]:
          # `.compile` foreign source -> object file, linked into the program.
          # The first field is a C-family language (set by `addBuildTarget`).
          let obj = splitFile(path).name & ".o"
          c.toBuild.add CFile(name: path, obj: obj, customArgs: args)
        else:
          # `{.build(builder, tool, args[, linkflags]).}` — a custom backend routes
          # THIS module's Leng IR through `tool`. The first field is the generic
          # builder command (e.g. `"nimony c"`), distinct from a `.compile`
          # language token above.
          c.backendTools.add BackendTool(builder: typ, toolSrc: path,
            toolName: splitFile(path).name, args: args, modFile: current.files[0],
            linkFlags: linkFlags)

proc processBundle(c: var DepContext; it: var Cursor) =
  ## Read a `(bundle (tup builder tool args)…)` node (`.bundle` pragma): a custom
  ## linker tool that overrides the final link step.
  it.into:  # (bundle …)
    while it.hasMore:
      assert it.exprKind == TupX
      var x = it
      skip it
      x.into TupX:
        assert x.isStringLit
        let builder = pool.strings[x.litId]
        inc x
        assert x.isStringLit
        let path = pool.strings[x.litId]
        inc x
        var args = ""
        if x.isStringLit:
          args = pool.strings[x.litId]
          inc x
        while x.hasMore: skip x
        c.bundles.add Bundle(builder: builder, toolSrc: path,
          toolName: splitFile(path).name, args: args)

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
      processBuild c, n, current
    elif n.tagId == TagId(BundleIdx):
      processBundle c, n
    elif n.tagId == TagId(PassLP):
      n.into:  # (passL …)
        while n.hasMore:
          assert n.isStringLit
          # A single `{.passL.}` value may hold several flags (e.g.
          # `-framework Foundation`). nifmake quotes each StringLit as ONE argv
          # token, so split into whitespace-separated flags here — otherwise the
          # linker sees `-framework Foundation` as a single unknown argument.
          for flag in splitWhitespace(pool.strings[n.litId]):
            c.passL.add flag
          inc n
    elif n.tagId == TagId(PassCP):
      n.into:  # (passC …)
        while n.hasMore:
          assert n.isStringLit
          for flag in splitWhitespace(pool.strings[n.litId]):
            c.passC.add flag
          inc n
    else:
      skip n
  else:
    #echo "IGNORING ", toString(n, false)
    skip n

proc processDeps(c: var DepContext; n: Cursor; current: Node) =
  var n = n
  if n.isTagLit and pool.tags[n.tagId] == "stmts":
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
    depsFile = c.config.depsFile(p, c.cmd == DoDoc)
  else:
    depsFile = c.config.deps2File(p)

  var r = rd.open(depsFile)
  var buf = createTokenBuf()
  parse(r, buf)
  rd.close(r)
  processDeps c, beginRead(buf), current
  if {SkipSystem, IsSystem} * c.moduleFlags == {} and not current.isSystem:
    importSystem c, current
proc rootPath(c: DepContext): string =
  # XXX: Relative paths in build files are relative to current working directory, not the location of the build file.
  result = absoluteParentDir(c.rootNode.files[0].nimFile)
  result = onRaiseQuit relativePath(result, onRaiseQuit os.getCurrentDir())

proc sharedObjDir(): string =
  ## Project-wide cache for object files produced from `{.build("C", ...).}`
  ## pragmas (currently just `vendor/mimalloc/src/static.c`). These TUs don't
  ## depend on per-project state, so compiling them once and reusing the .o
  ## across nimcaches saves ~4-5 s per cold build on Windows.
  result = getCacheDir("nimony") / "nimcache_static"

proc sharedObjFile(cfile: CFile): string =
  sharedObjDir() / cfile.obj

proc emitFrontendArgs(b: var Builder; baseDir, commandLineArgs: string) =
  ## Emit the shared `--base:` plus the forwarded `commandLineArgs` for a
  ## frontend tool command (`nimsem`/`idetools`), de-duplicating as we go.
  ## `--base:` is always written explicitly from `baseDir`, so any `--base:`
  ## already present in `commandLineArgs` is dropped, and other exact-duplicate
  ## args are collapsed. This matters for the nested sub-compiles that
  ## compile-time evaluation spawns (`semos.runProgram`/`prepareEval`,
  ## `macro_plugin`): those thread the outer `--base:`/`--nimcache:` back in via
  ## `commandLineArgs`, which would otherwise emit `--base:X --base:X …
  ## --nimcache:X … --nimcache:X` — an argv that no longer matches the outer
  ## build's command for the same module.
  var seen: seq[string] = @[]
  if baseDir.len > 0:
    let baseArg = "--base:" & quoteShell(baseDir)
    b.addStrLit baseArg
    seen.add baseArg
  for arg in commandLineArgs.split(' '):
    if arg.len > 0 and not arg.startsWith("--base:") and arg notin seen:
      b.addStrLit arg
      seen.add arg

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

proc defineHexerCmds(b: var Builder; hexer: string; bits: int; bigEndian: bool; checkFlags: string) =
  let cpuFlag = if bigEndian: "--cpu:be" else: "--cpu:le"
  b.withTree "cmd":
    b.addSymbolDef "hexer"
    b.addStrLit hexer
    b.addStrLit "c"
    b.addStrLit "--bits:" & $bits
    b.addStrLit cpuFlag
    # Forward the active check modes so nifcgen injects only the requested
    # runtime checks (e.g. `--boundchecks:off` ⇒ no `nimUcheckB` in `(at …)`).
    # A bare `--flags` means "no checks" (e.g. `-d:danger`): with a trailing
    # `--flags:` parseopt would consume the next argument as its value.
    b.addStrLit (if checkFlags.len > 0: "--flags:" & checkFlags else: "--flags")
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

proc wantTool(name, src, builder, nifcachePath: string;
              toolExe, toolBuild, toolBuilderCmd, builderCmdName: var Table[string, string]) =
  ## Register a backend tool (a routing tool or a custom linker) for build/command
  ## emission: reuse a prebuilt `bin/` copy if present, else compile it from
  ## source on demand. Mutates the shared tables `generateFinalBuildFile` walks to
  ## emit the builder commands, routing commands, and tool-build nodes.
  if toolExe.hasKey(name): return
  let found = findTool(name)
  if semos.fileExists(found):
    toolExe[name] = found                      # prebuilt / known -> use as-is
  else:
    toolExe[name] = nifcachePath / name.addFileExt(ExeExt)
    toolBuild[name] = src
    toolBuilderCmd[name] = builder
    if not builderCmdName.hasKey(builder):
      builderCmdName[builder] = "builderCmd" & $builderCmdName.len

type
  ManifestFile = object
    ## One `(file …)` entry of the link manifest. `flags` are link flags scoped to
    ## THIS file (from a `.build` module's 4th slot), passed to the linker next to
    ## it — distinct from the manifest's global `(flags …)` (`passL`).
    path: string
    kind: string          ## "obj" | "artifact"
    flags: seq[string]

proc writeLinkManifest(path, exe, apptype: string;
                       files: seq[ManifestFile]; flags: seq[string]): string =
  ## Write the manifest NIF a linker (the default `niflink`, or a `{.bundle.}`
  ## tool) consumes: every project artifact (objects + routed backend outputs)
  ## with optional per-file link flags, the app-type, and the global link flags.
  ## The linker reads this and links/bundles/filters as it sees fit (e.g. link the
  ## `obj`s, embed or ignore the backend `artifact`s).
  ##
  ## Written `OnlyIfChanged`: the manifest is an input of the link nifmake node,
  ## so rewriting it with a fresh mtime on every `nimony c` would re-fire the
  ## link even on a no-op build. Preserving the mtime when the bytes are
  ## identical keeps the backend incremental.
  var b = nifbuilder.open(path, writeMode = OnlyIfChanged)
  b.addHeader()
  b.withTree "link":
    b.withTree "apptype":
      b.addStrLit apptype
    b.withTree "output":
      b.addStrLit exe
    for f in files:
      b.withTree "file":
        b.addStrLit f.path
        b.withTree "kind":
          b.addStrLit f.kind
        if f.flags.len > 0:
          b.withTree "flags":
            for fl in f.flags:
              b.addStrLit fl
    if flags.len > 0:
      b.withTree "flags":
        for f in flags:
          b.addStrLit f
  b.close()
  result = path

proc generateFinalBuildFile(c: DepContext; commandLineArgsLengc: string; passC, passL: string): string =
  result = c.config.nifcachePath / c.rootNode.files[0].modname & ".final.build.nif"
  var b = nifbuilder.open(result)
  defer: b.close()

  b.addHeader()
  b.withTree "stmts":
    # Command definitions
    let lengc = findTool("lengc")
    let hexer = findTool("hexer")
    # The experimental Shoggoth optimizer runs only when optimization is
    # actually requested (`--opt:speed` / `--opt:size`); default/debug builds
    # are byte-for-byte unaffected.
    let useOptimizer = c.config.optLevel in {optSpeed, optSize}
    let native = c.config.backend == backendNative
    # A native program that uses the `.compile`/`{.build…}` pragma (in ANY module)
    # is finished by the system linker, so nifasm's relocatable object can be
    # combined with the foreign `.o`s (e.g. Objective-C) and any frameworks. Plain
    # native programs keep the static, libc-free "nifasm is the linker" path.
    # (Written as plain `var`s rather than `and`/`if`-expression `let`s: the
    # self-hosted compiler's initialization analysis can't yet prove the temp
    # such expressions lower to is always assigned.)
    var nativeSysLink = false
    if native and c.toBuild.len > 0: nativeSysLink = true
    # The foreign objects and frameworks need a real driver; clang knows how to
    # compile `.m`/`.c`, pull in libobjc, resolve `-framework`, and supply the crt.
    var sysLinker = c.config.linker
    if sysLinker.len == 0: sysLinker = "clang"
    var shoggoth = ""
    if useOptimizer:
      shoggoth = findTool("shoggoth")

    if native:
      # Native backend: arkham (Leng -> typed asm-NIF) replaces lengc. Output is
      # passed as the single token `-o:<path>` (the colon form; `addFilename`
      # concatenates the `-o:` prefix directly onto the output path).
      b.withTree "cmd":
        b.addSymbolDef "arkham"
        b.addStrLit findTool("arkham")
        b.addStrLit "-a:" & c.config.arkhamArch
        b.withTree "output":
          b.addStrLit "-o:"
        b.addKeyw "input"
    else:
      # Command for lengc (code generation)
      b.withTree "cmd":
        b.addSymbolDef "lengc"
        b.addStrLit lengc
        b.addStrLit $c.config.backend
        b.addStrLit "--compileOnly"
        b.addStrLit "--bits:" & $c.config.bits
        b.addKeyw "args"
        if commandLineArgsLengc.len > 0:
          for arg in commandLineArgsLengc.split(' '):
            if arg.len > 0:
              b.addStrLit arg
        b.addKeyw "input"

    # Command for the tree optimizer: `shoggoth c <input.c.nif> <output.oc.nif>`.
    if useOptimizer:
      b.withTree "cmd":
        b.addSymbolDef "optimize"
        b.addStrLit shoggoth
        b.addStrLit "c"
        b.addKeyw "input"
        b.addKeyw "output"

    # Command for hexer
    defineHexerCmds(b, hexer, c.config.bits, platform.CPU[c.config.targetCPU].endian == bigEndian, c.config.checkFlags)

    # Command for C/LLVM compiler (object files)
    b.withTree "cmd":
      b.addSymbolDef "cc"
      if c.config.backend == backendLLVM:
        b.addStrLit "clang"
      elif nativeSysLink:
        # Compiles the `.compile`d TUs (e.g. Objective-C `.m`); same driver that
        # links them, so the toolchain/ABI matches.
        b.addStrLit sysLinker
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
      # Optimization level. Even the default ("debug") gets -O1: in
      # practice it produces code that's just as easy to step through
      # as -O0, while letting the C compiler skip the truly silly
      # codegen patterns (per-statement spills, dead stores, etc.).
      case c.config.optLevel
      of optDebug: b.addStrLit "-O1"
      of optNone:  b.addStrLit "-O0"
      of optSize:  b.addStrLit "-Os"
      of optSpeed: b.addStrLit "-O3"
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

    # Commands for custom backends (`{.build(builder, tool[, args]).}`): each
    # module routes its Leng IR through `tool`, an external program compiled on
    # demand by the *generic* `builder` command (e.g. `"nimony c"`, `"nim c"`,
    # `"nimony c --path:…"`). Nothing about the builder or tool names is
    # hardcoded: the builder's first token is the program (resolved via
    # `findTool`, so `nimony` -> `bin/`, `nim` -> PATH), the rest is passed
    # verbatim, and the tool exe is written via `-o:` (accepted by both nim and
    # nimony). A tool already present in `bin/` (a known name) is used as-is.
    var backendToolExe = initTable[string, string]()      # toolName -> exe path
    var backendToolBuild = initTable[string, string]()    # toolName -> source (only if we build it)
    var backendToolBuilderCmd = initTable[string, string]() # toolName -> builder command string
    var builderCmdName = initTable[string, string]()      # builder string -> nifmake cmd name
    var customLinkerName = ""   # "" = no custom linker; else overrides the link step
    var customLinkerArgs = ""
    for bt in c.backendTools:
      wantTool(bt.toolName, bt.toolSrc, bt.builder, c.config.nifcachePath,
               backendToolExe, backendToolBuild, backendToolBuilderCmd, builderCmdName)
    # A `{.bundle.}` module overrides the link step; the first one wins.
    for bn in c.bundles:
      if customLinkerName.len == 0:
        customLinkerName = bn.toolName
        customLinkerArgs = bn.args
        wantTool(bn.toolName, bn.toolSrc, bn.builder, c.config.nifcachePath,
                 backendToolExe, backendToolBuild, backendToolBuilderCmd, builderCmdName)
    if c.backendTools.len > 0 or c.bundles.len > 0:
      # One build command per distinct builder string: `<prog> <rest…> -o:<exe> <src>`.
      for builder, cmdName in builderCmdName:
        let toks = builder.splitWhitespace
        b.withTree "cmd":
          b.addSymbolDef cmdName
          b.addStrLit toks[0]                 # program; expandCommand resolves via findTool
          for i in 1 ..< toks.len:            # subcommand + flags, verbatim
            b.addStrLit toks[i]
          b.withTree "output":
            b.addStrLit "-o:"
          b.addKeyw "input"
      # Routing command per distinct tool: `<toolExe> <args> <module.c.nif> <out>`.
      # The tool exe is also a `(do …)` input (so nifmake builds it first), but is
      # referenced by index `(input 0 0)` so only the Leng IR reaches the cmdline.
      for tn, exe in backendToolExe:
        b.withTree "cmd":
          b.addSymbolDef tn
          b.addStrLit exe
          b.addKeyw "args"
          b.withTree "input":
            b.addIntLit 0
            b.addIntLit 0
          b.addKeyw "output"

    # Command for linking/archiving
    if c.cmd in {DoCompile, DoRun} and nativeSysLink:
      # Two commands: nifasm emits a relocatable object (it still bundles every
      # module's `.asm.nif`, reachable from the main one via `(input 0 0)`), and
      # the system linker combines that object with the `.compile`d foreign `.o`s
      # and any `-framework`/`-l` flags (passL) into the final executable.
      b.withTree "cmd":
        b.addSymbolDef "nifasmObj"
        b.addStrLit findTool("nifasm")
        b.addStrLit "--emit-obj"
        b.withTree "output":
          b.addStrLit "-o:"
        b.withTree "input":
          b.addIntLit 0
          b.addIntLit 0  # only the main module's .asm.nif on the command line
      b.withTree "cmd":
        b.addSymbolDef "link"
        b.addStrLit sysLinker
        b.addStrLit "-o"
        b.addKeyw "output"
        b.withTree "input":
          b.addIntLit 0
          b.addIntLit -1  # nifasm object + every `.compile` object
        b.withTree "argsext":
          b.addStrLit ".linker.args"
        if passL.len > 0:
          for arg in passL.split(' '):
            if arg.len > 0:
              b.addStrLit arg
        for i in c.passL:
          b.addStrLit i
    elif c.cmd in {DoCompile, DoRun} and native:
      # nifasm is the native linker: it takes the *main* module's `.asm.nif`
      # (`(input 0 0)`) and pulls the dependent `.asm.nif` modules from disk by
      # suffix via the shared NIF module loader. `-o:` is the colon form.
      b.withTree "cmd":
        b.addSymbolDef "link"
        b.addStrLit findTool("nifasm")
        b.withTree "output":
          b.addStrLit "-o:"
        b.withTree "input":
          b.addIntLit 0
          b.addIntLit 0  # only the main module's .asm.nif on the command line
    elif c.cmd in {DoCompile, DoRun}:
      # Plain C/LLVM backend: `niflink` is the default linker. It reads the link
      # manifest (input 0) — every object, the app-type, and the link flags — and
      # compiles/links/archives itself, so the old per-app-type `ar` / `-shared` /
      # exe branches collapse into this one node. (A `{.build(…, linker).}` module
      # overrides it with its own tool.)
      b.withTree "cmd":
        b.addSymbolDef "link"
        b.addStrLit findTool("niflink")
        b.withTree "input":
          b.addIntLit 0
          b.addIntLit 0  # only the manifest reaches niflink's command line
        b.addKeyw "output"

    # Build rules
    if c.cmd in {DoCompile, DoRun}:
      let backend = c.rootNode.files[0].modname  # DCE and after are main-specific
      let backendDir = c.config.nifcachePath / backend
      let liveFile = backendDir / c.rootNode.files[0].modname & ".live.nif"

      # Split DCE — phase 1: collect every module's .dce.nif analysis,
      # compute the global live set + generic-instance resolve table,
      # write the shared <main>.live.nif. Single small serial node.
      b.withTree "do":
        b.addIdent "dceLive"
        for i, n in pairs c.nodes:
          # The .dce.nif sits next to its corresponding .x.nif.
          var dceFile = ""
          if i == 0:
            dceFile = backendDir / n.files[0].modname & ".dce.nif"
          else:
            dceFile = c.config.nifcachePath / n.files[0].modname & ".dce.nif"
          b.withTree "input":
            b.addStrLit dceFile
        b.withTree "output":
          b.addStrLit liveFile

      # Split DCE — phase 2: per-module emit. Each `(do dceEmit ...)` is
      # independent (all share live.nif as a read-only input), so nifmake
      # parallelises them across cores.
      for i, n in pairs c.nodes:
        b.withTree "do":
          b.addIdent "dceEmit"
          b.withTree "args":
            b.addStrLit "--outdir:" & backendDir
          b.withTree "input":
            # Root module's .x.nif is backend-specific (--isMain).
            if i == 0:
              b.addStrLit backendDir / n.files[0].modname & ".x.nif"
            else:
              b.addStrLit c.config.hexedFile(n.files[0])
          b.withTree "input":
            b.addStrLit liveFile
          b.withTree "output":
            b.addStrLit c.config.lengcFile(n.files[0], backend)

      # Custom-backend nodes: build each tool once (depth 0) — routing tools AND
      # the custom linker (every entry in `backendToolBuild`) — then route every
      # `.build` module's Leng IR through its routing tool (depth 1, parallel).
      # nifmake orders the tool build before its uses via the exe output->input
      # edge.
      for toolName, src in backendToolBuild:
        b.withTree "do":
          # `getOrDefault` (non-raising): every key is guaranteed present (the
          # tool was just registered via `wantTool`), so the `.raises` `[]` —
          # which would escape this proc's `defer` and force it to be `.raises` —
          # is avoided. Same for the other tool-table lookups below.
          b.addIdent builderCmdName.getOrDefault(backendToolBuilderCmd.getOrDefault(toolName))
          b.withTree "input":
            b.addStrLit src
          b.withTree "output":
            b.addStrLit backendToolExe.getOrDefault(toolName)
      for bt in c.backendTools:
        let lengcInput = c.config.lengcFile(bt.modFile, backend)
        let artifact = lengcInput & "." & bt.toolName & ".out.nif"
        b.withTree "do":
          b.addIdent bt.toolName
          b.withTree "args":
            for flag in splitWhitespace(bt.args):
              b.addStrLit flag
          b.withTree "input":
            b.addStrLit lengcInput
          b.withTree "input":
            b.addStrLit backendToolExe.getOrDefault(bt.toolName)
          b.withTree "output":
            b.addStrLit artifact

      # Link executable
      var objFiles = initHashSet[string]()
      if customLinkerName.len > 0 or (not native and not nativeSysLink):
        # Manifest-based link. The plain C/LLVM backend links through the default
        # `link` command (== `niflink`); a `{.bundle.}` module overrides it with
        # its own tool. Either way the linker is handed a manifest NIF describing
        # every project artifact + the app-type, and reads only that (`(input 0
        # 0)`); the objects/artifacts are listed as inputs purely to order them
        # before the link runs.
        # Plain `var` rather than an `if`-expression `let`: the self-hosted
        # compiler's initialization analysis can't prove the temp such an
        # expression lowers to is always assigned (see the similar note above).
        var linkNode = "link"
        if customLinkerName.len > 0: linkNode = customLinkerName
        let exe = c.config.exeFile(c.rootNode.files[0], backend)
        var objs: seq[string] = @[]
        if not native:
          # Dedup by path: a `.compile` shared object (e.g. mimalloc's
          # `nimcache_static/static.o`) can be contributed by several modules'
          # `toBuild`, and linking the same `.o` twice yields duplicate-symbol
          # errors. The manifest niflink actually links is built from `objs`, so
          # the dedup must happen here (not only on the DO-node ordering inputs).
          var seenObjs = initHashSet[string]()
          for cfile in c.toBuild:
            let o = sharedObjFile(cfile)
            if not seenObjs.containsOrIncl(o): objs.add o
          for v in c.nodes:
            let o = c.config.objFile(v.files[0], backend)
            if not seenObjs.containsOrIncl(o): objs.add o
        var artifacts: seq[string] = @[]
        for bt in c.backendTools:
          artifacts.add c.config.lengcFile(bt.modFile, backend) & "." & bt.toolName & ".out.nif"
        # Manifest entries: each object plus, for a `.build` module, its 4th-slot
        # per-file link flags scoped to that object; routed backend outputs follow
        # as `artifact` entries (ignored by a plain C linker, embeddable by a
        # custom one).
        var mfiles: seq[ManifestFile] = @[]
        for o in objs:
          var ff: seq[string] = @[]
          for bt in c.backendTools:
            if bt.linkFlags.len > 0 and c.config.objFile(bt.modFile, backend) == o:
              for fl in splitWhitespace(bt.linkFlags): ff.add fl
          mfiles.add ManifestFile(path: o, kind: "obj", flags: ff)
        for a in artifacts:
          mfiles.add ManifestFile(path: a, kind: "artifact", flags: @[])
        var flags: seq[string] = @[]
        if passL.len > 0:
          for f in passL.split(' '):
            if f.len > 0: flags.add f
        for f in c.passL: flags.add f
        let manifest = backendDir / (c.rootNode.files[0].modname & ".linkmanifest.nif")
        discard writeLinkManifest(manifest, exe, $c.config.appType, mfiles, flags)
        b.withTree "do":
          b.addIdent linkNode
          if customLinkerName.len > 0 and customLinkerArgs.len > 0:
            b.withTree "args":
              for a in splitWhitespace(customLinkerArgs):
                b.addStrLit a
          b.withTree "input":                 # input 0: the manifest the linker reads
            b.addStrLit manifest
          for o in objs:                       # ordering: objects must be built first
            if not objFiles.containsOrIncl(o):
              b.withTree "input":
                b.addStrLit o
          for a in artifacts:                  # ordering: routed backend artifacts
            b.withTree "input":
              b.addStrLit a
          if customLinkerName.len > 0:          # ordering: a custom linker is built first
            b.withTree "input":
              b.addStrLit backendToolExe.getOrDefault(customLinkerName)
          b.withTree "output":
            b.addStrLit exe
      elif nativeSysLink:
        # First nifasm bundles every module's `.asm.nif` into one relocatable
        # object (it reads only the main one's path and pulls the rest by suffix;
        # the others are listed purely to order them before nifasm runs).
        let nativeObj = c.config.objFile(c.rootNode.files[0], backend)
        b.withTree "do":
          b.addIdent "nifasmObj"
          var asmInputs = initHashSet[string]()
          let mainAsm = c.config.asmFile(c.rootNode.files[0], backend)
          b.withTree "input":
            b.addStrLit mainAsm
          asmInputs.incl mainAsm
          for v in c.nodes:
            let a = c.config.asmFile(v.files[0], backend)
            if not asmInputs.containsOrIncl(a):
              b.withTree "input":
                b.addStrLit a
          b.withTree "output":
            b.addStrLit nativeObj
        # Then the system linker combines that object with the `.compile` objects.
        b.withTree "do":
          b.addIdent "link"
          b.withTree "input":
            b.addStrLit nativeObj
          objFiles.incl nativeObj
          for cfile in c.toBuild:
            let obj = sharedObjFile(cfile)
            if not objFiles.containsOrIncl(obj):
              b.withTree "input":
                b.addStrLit obj
          b.withTree "output":
            b.addStrLit c.config.exeFile(c.rootNode.files[0], backend)
      else:
        # Native backend (no custom linker): nifasm links from the *main*
        # module's `.asm.nif` (input[0]) and discovers the dependent modules by
        # suffix on disk. List every module's `.asm.nif` as an input (root first)
        # so they are all built before nifasm runs, even though only input[0]
        # reaches its command line (see the `link` cmd's `(input 0 0)`).
        b.withTree "do":
          b.addIdent "link"
          let mainAsm = c.config.asmFile(c.rootNode.files[0], backend)
          b.withTree "input":
            b.addStrLit mainAsm
          objFiles.incl mainAsm
          for v in c.nodes:
            let a = c.config.asmFile(v.files[0], backend)
            if not objFiles.containsOrIncl(a):
              b.withTree "input":
                b.addStrLit a
          b.withTree "output":
            b.addStrLit c.config.exeFile(c.rootNode.files[0], backend)

      objFiles = initHashSet[string]()
      # Build object files from `.compile`d source files with custom args. Outputs
      # land in `<nimony-root>/nimcache_static/` so the same .o is reused across
      # projects — these TUs (mimalloc's `static.c`, or a user's Objective-C
      # source) don't depend on the user's project. A plain native build has no
      # such TUs (arkham/nifasm go straight to machine code); only a native build
      # that uses the `.compile` pragma (`nativeSysLink`) compiles them.
      if (not native) or nativeSysLink:
        for cfile in c.toBuild:
          let obj = sharedObjFile(cfile)
          if not objFiles.containsOrIncl(obj):
            b.withTree "do":
              b.addIdent "cc"
              b.withTree "input":
                b.addStrLit cfile.name
              b.withTree "args":
                # `customArgs` is a free-form string holding several flags
                # (e.g. `-DMI_STATS=1 -I.../mimalloc/include`). nifmake quotes
                # each StringLit as one shell argument, so emit one StringLit
                # per whitespace-separated flag — otherwise the whole string is
                # passed as a single malformed argument and the `-I` is lost.
                for flag in splitWhitespace(cfile.customArgs):
                  b.addStrLit flag
              b.withTree "output":
                b.addStrLit obj

      for i, v in pairs c.nodes:
        if not native:
          let obj = c.config.objFile(v.files[0], backend)
          if not objFiles.containsOrIncl(obj):
            b.withTree "do":
              b.addIdent "cc"
              b.withTree "input":
                b.addStrLit c.config.genFile(v.files[0], backend)
              b.withTree "output":
                b.addStrLit obj

        # Optionally run Shoggoth on the DCE'd `.c.nif`, producing `.oc.nif`;
        # the codegen (lengc or arkham) consumes that. Skipped entirely unless
        # `useOptimizer`, in which case the plain `.c.nif` is read.
        var lengcInput: string
        if useOptimizer:
          let optimized = c.config.optimizedFile(v.files[0], backend)
          b.withTree "do":
            b.addIdent "optimize"
            b.withTree "input":
              b.addStrLit c.config.lengcFile(v.files[0], backend)
            b.withTree "output":
              b.addStrLit optimized
          lengcInput = optimized
        else:
          lengcInput = c.config.lengcFile(v.files[0], backend)

        if native:
          # arkham: per-module Leng -> typed asm-NIF. arkham additionally loads
          # imported modules' `.c.nif` on demand (cross-module type/sig
          # resolution), so list those as dependency inputs to order them before
          # this module's codegen. Only input[0] (this module's lengcInput)
          # reaches arkham's command line (the `arkham` cmd uses `(input)`).
          b.withTree "do":
            b.addIdent "arkham"
            b.withTree "input":
              b.addStrLit lengcInput
            var seenDeps = initHashSet[string]()
            for depIdx in v.deps:
              let depNif = c.config.lengcFile(c.nodes[depIdx].files[0], backend)
              if not seenDeps.containsOrIncl(depNif):
                b.withTree "input":
                  b.addStrLit depNif
            b.withTree "output":
              b.addStrLit c.config.asmFile(v.files[0], backend)
        else:
          # Build C/LLVM IR files from .c.nif files
          b.withTree "do":
            b.addIdent "lengc"
            b.withTree "args":
              b.addStrLit "--nimcache:" & backendDir
            if i == 0:
              b.withTree "args":
                b.addStrLit "--isMain"
            b.withTree "input":
              b.addStrLit lengcInput
            b.withTree "output":
              b.addStrLit c.config.genFile(v.files[0], backend)

        # Build .x.nif files from .s.nif files via hexer.
        # For the root module (i==0) the output is backend-specific so that
        # its --isMain version does not overwrite the shared .x.nif that other
        # compilations produce when this module is a non-main dependency.
        b.withTree "do":
          b.addIdent "hexer"
          if i == 0:
            b.withTree "args":
              b.addStrLit "--isMain"
            b.withTree "args":
              b.addStrLit "--app:" & $c.config.appType
            b.withTree "args":
              b.addStrLit "--outdir:" & backendDir
          b.withTree "input":
            b.addStrLit c.config.semmedFile(v.files[0], v.plugin)
          # Cross-module hexer dep: imports' `.s.idx.nif` carries both the
          # interface checksum and inline-proc body hashes (see
          # `processForChecksum`'s inline path). Listing imports' `.s.idx.nif`
          # — and *not* the bulkier `.s.nif` — gives finer-grained incremental:
          # a non-inline private body change in import A keeps A's
          # `.s.idx.nif` byte-identical (mtime preserved), so B's hexer
          # doesn't rerun. Same-module `.s.idx.nif` is intentionally omitted
          # — hexer reads its own embedded index out of `.s.nif`.
          var seenImports = initHashSet[string]()
          for depIdx in v.deps:
            let idxFile = c.config.indexFile(c.nodes[depIdx].files[0], c.nodes[depIdx].plugin)
            if not seenImports.containsOrIncl(idxFile):
              b.withTree "input":
                b.addStrLit idxFile
          b.withTree "output":
            if i == 0:
              b.addStrLit backendDir / v.files[0].modname & ".x.nif"
            else:
              b.addStrLit c.config.hexedFile(v.files[0])
          # `.dce.nif` is emitted alongside `.x.nif` by `bin/hexer c`. It
          # is consumed only by the split-DCE `dceLive` node, but listing
          # it here lets nifmake track it as a real artifact and order
          # `dceLive` after every per-module hexer.
          b.withTree "output":
            if i == 0:
              b.addStrLit backendDir / v.files[0].modname & ".dce.nif"
            else:
              b.addStrLit c.config.nifcachePath / v.files[0].modname & ".dce.nif"

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
      emitFrontendArgs(b, c.config.baseDir, commandLineArgs)
      b.addStrLit "m"
      b.addKeyw "args"
      # Module files are passed via (args) in each (do nimsem) block

    if cmd == DoCheck:
      b.withTree "cmd":
        b.addSymbolDef "idetools"
        b.addStrLit c.nimsem
        emitFrontendArgs(b, c.config.baseDir, commandLineArgs)
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
  let root = Node(files: @[p], id: 0, parent: -1, active: 0, isSystem: IsSystem in moduleFlags)
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
      b.addSymbolDef "lengc"
      b.addStrLit findTool("lengc")
      b.addStrLit "c"
      b.addStrLit "--compileOnly"
      b.addKeyw "args"
      b.addKeyw "input"

    defineHexerCmds(b, findTool("hexer"), config.bits, platform.CPU[config.targetCPU].endian == bigEndian, config.checkFlags)

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
        b.addIdent "lengc"
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

proc progArg(flags: set[BuildFlag]; lo, hi: int): string =
  # nifmake itself routes the bar (terminal-only); we only suppress it where
  # nimony asked for silence or machine-readable output.
  if SilentMake in flags or Report in flags: ""
  else: "--progress:" & $lo & ":" & $hi & " "

proc buildGraph*(config: sink NifConfig; project: string;
    flags: set[BuildFlag];
    commandLineArgs, commandLineArgsLengc: string; moduleFlags: set[ModuleFlag]; cmd: Command;
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

  # `nimony c` drives nifmake once for the frontend and once more for the
  # backend (or docs); `DoCheck` stops after the frontend. Hand each invocation
  # a slice of the 0..100% range so nifmake's live bar reads as one continuous
  # indicator across the separate processes instead of restarting per phase.
  let twoPhase = cmd != DoCheck

  exec nifmakeCommand & progArg(flags, 0, if twoPhase: 50 else: 100) & quoteShell(buildFilename)

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
    exec nifmakeCommand & progArg(flags, 50, 100) & quoteShell(buildDocFilename)
    return

  if cmd != DoCheck:
    # Parse `.s.deps.nif`.
    # It is generated by nimsem and doesn't contains modules imported under `when false:`.
    # https://github.com/nim-lang/nimony/issues/985
    c = initDepContext(config, project, nifler, true, forceRebuild, moduleFlags, cmd)
    let backend = c.config.nifcachePath / c.rootNode.files[0].modname
    onRaiseQuit createDir(path(backend))
    onRaiseQuit createDir(path(sharedObjDir()))
    let buildFinalFilename = generateFinalBuildFile(c, commandLineArgsLengc, passC, passL)
    # second (backend) phase: 50..100%
    # Linkers (gcc/clang/ld/ar) don't auto-create the output directory.
    # When the user passes `--out:bin/foo` or `--outdir:bin`, materialise
    # `bin/` here. Nim does the same in `prepareToWriteOutput`.
    let exeOutPath = c.config.exeFile(c.rootNode.files[0], c.rootNode.files[0].modname)
    let exeOutDir = exeOutPath.parentDir
    if exeOutDir.len > 0:
      onRaiseQuit createDir(path(exeOutDir))
    exec nifmakeCommand & progArg(flags, 50, 100) & quoteShell(buildFinalFilename)

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

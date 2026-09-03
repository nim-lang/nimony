#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Path handling and `exec` like features as `sem.nim` needs it.

from std / strutils import multiReplace, startsWith
import std / [tables, sets, os, envvars, syncio, formatfloat, assertions, dirs, paths, times]
from std / osproc import execCmdEx

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [nifchecksums, nifindexes, tooldirs, argsfinder, symparser, vfs]
import ".." / lib / nifreader as rd
from ".." / lib / nifcoreparse import parse
# qualified-only: the private-pool plugin-input copy must not fight the
# global-pool overloads nifprelude puts in scope
from ".." / lib / nifcore import nil
from ".." / lib / bif import storeToString, isBifFile, UnusedNameTag, DependencyTag

import nimony_model, symtabs, builtintypes, decls, asthelpers,
  programs, sigmatch, magics, reporters, nifconfig,
  semdata

import ".." / gear2 / modnames

proc nimonyDir(): string =
  ## The project root for stdlib resolution. `bin*` (not just `bin`) is
  ## matched so the boot bootstrap can stage toolchains under sibling
  ## directories like `bin0`, `bin1`, `bin2` and still find `lib/` next to
  ## them.
  let appDir = getAppDir()
  let (head, tail) = splitPath(appDir)
  if tail.startsWith("bin"):
    result = head
  else:
    result = appDir

proc stdlibDir*(): string =
  result = nimonyDir() / "lib"

proc setupPaths*(config: var NifConfig) =
  config.paths.add stdlibDir()
  let pathsFile = findArgs(config.baseDir, "nimony.paths")
  processPathsFile pathsFile, config.paths
  #echo getAppFilename(), "CONFIG.BASEDIR: ", config.baseDir, " CONFIG.PATHS: ", config.paths

proc stdlibFile*(f: string): string =
  result = stdlibDir() / f

proc compilerDir*(): string =
  let appDir = getAppDir()
  let (head, tail) = splitPath(appDir)
  if tail.startsWith("bin"):
    return head
  else: return tail

proc absoluteParentDir*(f: string): string =
  result = ""  # Nim's ProveInit doesn't see `quit` as noreturn across try/except
  try:
    result = f.absolutePath().parentDir()
  except:
    quit "FAILURE: cannot resolve absolute path for " & f

proc fileExists*(f: string): bool {.inline.} =
  ## Re-export of `os.fileExists` under the `semos` qualifier so callers can
  ## use `semos.fileExists` without importing `os` themselves.
  result = os.fileExists(f)

proc toAbsolutePath*(f: string): string =
  result = ""  # Nim's ProveInit doesn't see `quit` as noreturn across try/except
  if f.isAbsolute: return f
  try:
    result = os.absolutePath(f)
  except:
    quit "FAILURE: cannot resolve absolute path for " & f

proc toAbsolutePath*(f: string, dir: string): string =
  if f.isAbsolute: return f
  result = normalizedPath(dir / f)

proc toRelativePath*(f: string, dir: string): string =
  result = ""  # Nim's ProveInit doesn't see `quit` as noreturn across try/except
  if not f.isAbsolute: return f
  try:
    result = f.relativePath(dir)
  except:
    quit "FAILURE: cannot compute relative path " & f & " against " & dir

proc joinPath*(head, tail: string): string = head / tail

proc exec*(cmd: string) =
  if execShellCmd(cmd) != 0: quit("FAILURE: " & cmd)

proc nimexec(cmd: string) =
  let t = findExe("nim")
  if t.len == 0:
    quit("FAILURE: cannot find nim.exe / nim binary")
  exec quoteShell(t) & " " & cmd

proc requiresTool*(tool, src: string; forceRebuild: bool) =
  let t = findTool(tool)
  # XXX: hack for more convenient development
  if not os.fileExists(t) or forceRebuild:
    let src = compilerDir() / src
    let args = # compiler bin path
      when not defined(debug):
        "c -d:release --outdir:" & binDir()
      else: "c --outdir:" & binDir()
    # compile required tool
    nimexec(args & "  " & src)

proc resolveFile*(paths: openArray[string]; origin: string; toResolve: string): string =
  let nimFile = toResolve.addFileExt(".nim")
  #if toResolve.startsWith("std/") or toResolve.startsWith("ext/"):
  #  result = stdFile nimFile
  if toResolve.isAbsolute:
    result = nimFile
  elif toResolve.len > 0 and toResolve[0] == '$':
    var key = ""
    var i = 1
    while i < toResolve.len:
      if toResolve[i] in {'/', '\\'}:
        break
      key.add toResolve[i]
      inc i
    let val = getEnv(key)
    if val.len == 0:
      result = nimFile
    else:
      result = val / nimFile.substr(i)
  else:
    result = splitFile(origin).dir / nimFile
    var i = 0
    while not os.fileExists(result) and i < paths.len:
      result = paths[i] / nimFile
      inc i

type
  ImportedFilename* = object
    path*: string ## stringified path from AST that has to be resolved
    name*: string ## extracted module name to define a sym for in `import`
    plugin*: string ## plugin name if any (usually empty)
    isSystem*: bool

proc moduleNameFromPath*(path: string): string =
  result = splitFile(path).name

proc filenameVal*(n: var Cursor; res: var seq[ImportedFilename]; hasError: var bool; allowAs: bool) =
  case n.kind
  of StrLit:
    let s = pool.strings[n.strId]
    # string literal could contain a path or .nim extension:
    let name = moduleNameFromPath(s)
    res.add ImportedFilename(path: s, name: name)
    inc n
  of Ident:
    let s = pool.strings[n.strId]
    res.add ImportedFilename(path: s, name: s)
    inc n
  of Symbol:
    var s = pool.syms[n.symId]
    extractBasename s
    res.add ImportedFilename(path: s, name: s)
    inc n
  of TagLit:
    case exprKind(n)
    of OchoiceX, CchoiceX:
      n.peekInto:
        if not n.hasMore:
          hasError = true
        else:
          filenameVal(n, res, hasError, allowAs)
    of QuotedX:
      let s = pool.strings[takeUnquoted(n)]
      res.add ImportedFilename(path: s, name: s)
    of CallX, InfixX:
      var x = n
      skip n # ensure we skipped it completely
      x = sub(x)
      let opId = takeIdent(x)
      if opId == StrId(0):
        hasError = true
        return
      let op = pool.strings[opId]
      if op == "as":
        if not allowAs:
          hasError = true
          return
        if not x.hasMore:
          hasError = true
          return
        var rhs = x
        skip rhs # skip lhs
        if not rhs.hasMore:
          hasError = true
          return
        let aliasId = takeIdent(rhs)
        if aliasId == StrId(0):
          hasError = true
          return
        let alias = pool.strings[aliasId]
        var prefix: seq[ImportedFilename] = @[]
        filenameVal(x, prefix, hasError, allowAs = false)
        if rhs.hasMore or prefix.len == 0:
          hasError = true
        for pre in mitems(prefix):
          res.add ImportedFilename(path: pre.path, name: alias)
      else: # any operator, could restrict to slash-like
        var prefix: seq[ImportedFilename] = @[]
        filenameVal(x, prefix, hasError, allowAs = false)
        var suffix: seq[ImportedFilename] = @[]
        filenameVal(x, suffix, hasError, allowAs = allowAs)
        if x.hasMore or prefix.len == 0 or suffix.len == 0:
          hasError = true
        for pre in mitems(prefix):
          for suf in mitems(suffix):
            res.add ImportedFilename(path: pre.path & op & suf.path, name: suf.name, plugin: suf.plugin)
    of PrefixX:
      var x = n
      skip n # ensure we skipped it completely
      x = sub(x)
      let opId = takeIdent(x)
      if opId == StrId(0):
        hasError = true
        return
      let op = pool.strings[opId] # any operator, could restrict to slash-like
      var suffix: seq[ImportedFilename] = @[]
      filenameVal(x, suffix, hasError, allowAs = allowAs)
      if x.hasMore or suffix.len == 0:
        hasError = true
      for suf in mitems(suffix):
        res.add ImportedFilename(path: op & suf.path, name: suf.name, plugin: suf.plugin)
    of ParX, TupX, BracketX:
      n.into:
        if not n.hasMore:
          hasError = true
        else:
          while n.hasMore:
            filenameVal(n, res, hasError, allowAs)
    of AconstrX, TupconstrX:
      n.into:
        skip n, SkipType  # type
        if not n.hasMore:
          hasError = true
        else:
          while n.hasMore:
            filenameVal(n, res, hasError, allowAs)
    of PragmaxX:
      # `import (m) {.plugin: "…".}`. Dependency scanning runs before sem, so
      # the pragma is still the parser's `(pragmas (kv plugin "…"))` -- with
      # `plugin` an Ident -- and not the resolved `(plugin …)` that
      # `templates.nim` reads off a semchecked routine.
      #
      # Every level is entered with `into` and drained before it is left. The
      # hand-written descent this replaces walked in with bare `inc`s and then
      # stepped over the `kv`'s close, which under nifcore is not a token:
      # the scope simply ends, `hasMore` is `rem > 0`, and `inc` asserts
      # `rem != 0`. So `if not n.hasMore: inc n` -- a classic `skipParRi` --
      # crashed the compiler on every well-formed import plugin (#2408).
      let start = res.len
      var success = false
      n.into:                                  # (pragmax …)
        if not n.hasMore:
          hasError = true
        else:
          filenameVal(n, res, hasError, allowAs)
          if n.substructureKind == PragmasU:
            n.into:                            # (pragmas …)
              if n.substructureKind == KvU:
                n.into:                        # (kv plugin "…")
                  if n.isIdent and pool.strings[n.strId] == "plugin":
                    inc n
                    if n.isStringLit:
                      for i in start ..< res.len:
                        res[i].plugin = pool.strings[n.strId]
                        success = true
                      skip n, SkipValue        # the plugin path
                      if n.hasMore: hasError = true
                  while n.hasMore: skip n
              while n.hasMore: skip n
          while n.hasMore: skip n
      if not success:
        hasError = true
    else:
      hasError = true
      skip n
  else:
    hasError = true
    skip n

proc replaceSubs*(fmt, currentFile: string; config: NifConfig): string =
  # Unpack Current File to Absolute
  let nifcache = config.nifcachePath
  var path = currentFile
  try:
    path = absolutePath(currentFile)
  except:
    discard "keep the input as-is"
  if os.fileExists(path):
    path = parentDir(path)
  # Replace matches with paths
  path = fmt.multiReplace([
    ("${path}", path),
    ("${nifcache}", nifcache)])
  result = path.normalizedPath()

# ------------------ include/import handling ------------------------

proc lastModTimeOrStale(path: string): int64 =
  ## `getLastModificationTime` raises on transient I/O errors. The result is
  ## only used for staleness comparisons, so any failure must fall through to
  ## "regenerate": -1 makes that automatic, since `-1 > anything` is false.
  ## Mirrors `deps.getLastModTime`.
  try:
    when defined(nimony):
      result = getLastModificationTime(path)
    else:
      result = times.toUnix(getLastModificationTime(path))
  except:
    result = -1'i64

proc parseFile*(nimFile: string; paths: openArray[string], nifcachePath: string): TokenBuf =
  let nifler = findTool("nifler")
  let name = moduleSuffix(nimFile, paths)
  let src = nifcachePath / name & ".p.nif"
  let depsFile = nifcachePath / name & ".p.deps.nif"
  # `include`d files are part of the dependency graph, so the driver's own
  # dep scan (`deps.execNifler`) has already parsed this file into the very
  # same `.p.nif` — with the identical command line — before it ever spawned
  # us. Re-running nifler would rewrite a byte-identical artifact: on
  # `nimony n bug.nim` that was 21 of 47 nifler processes, one per module
  # `system.nim` includes, and at ~2.7ms per spawn it is nearly all process
  # overhead. Reuse the artifact under the same freshness rule `execNifler`
  # uses, so the two agree on when a re-parse is actually needed.
  let srcTime = lastModTimeOrStale(nimFile)
  if fileExists(src) and fileExists(nimFile) and lastModTimeOrStale(src) > srcTime and
      fileExists(depsFile) and lastModTimeOrStale(depsFile) > srcTime:
    discard "already parsed by the dep scan"
  else:
    exec quoteShell(nifler) & " --portablePaths --deps parse " & quoteShell(nimFile) & " " &
      quoteShell(src)

  var r = rd.open(src)
  result = createTokenBuf()
  parse(r, result, denseLineInfo = true)
  rd.close(r)
proc getFile*(info: NifLineInfo): string =
  let fid = info.file
  if fid.isValid:
    result = realFile(pool.filenames[fid])
  else:
    result = ""

proc selfExec*(c: var SemContext; file: string; moreArgs: string) =
  let nimonyExe = findTool("nimony")
  exec quoteShell(nimonyExe) & c.commandLineArgs & moreArgs & " --ischild m " & quoteShell(file)
  #exec os.getAppFilename() & c.commandLineArgs & moreArgs & " --ischild m " & quoteShell(file)

# ------------------ plugin handling --------------------------

proc makePluginCache(dir: string) =
  try:
    when defined(nimony):
      createDir(path(dir))
    else:
      createDir(Path(dir))
  except:
    quit "FAILURE: cannot create directory " & dir

proc pluginCompileCmd(config: NifConfig; cacheDir: string): string =
  ## The invocation a plugin sub-compile shares with the sem-only run the
  ## validator needs: the cache, the search paths and the stdlib-configuration
  ## opt-outs. The caller appends the command and the file.
  #
  # `--nimcache:<cacheDir>` keeps the sub-compile's intermediate NIF artefacts
  # in a per-plugin scratch dir so parallel test workers don't fight over
  # `nimcache/` entries.
  #
  # Forward outer user search paths so plugin self-compilation computes the
  # same module identities for user modules. Internal Nimony library paths are
  # supplied below and deliberately not forwarded from the caller's path file.
  # Do not forward the raw command line: it can contain `--base`, which would
  # make plugin child compiles read caller-local nimony.paths files.
  #
  # `-d:nimonyPlugin` marks the sub-compile as the build OF a plugin. The
  # dependency scanner then schedules no plugin builds of its own: a plugin is
  # a leaf of the build graph. Without this, a plugin whose source imports the
  # module declaring it (`lib/std/deps/smartcli.nim` imports `std/smartcli`)
  # would need itself built first, and the nested builds never bottom out.
  let nimonyExe = findTool("nimony")
  let pluginDir = nimonyDir() / "src/nimony/lib"
  let srcLibPath = nimonyDir() / "src" / "lib"
  result = quoteShell(nimonyExe) &
    " --nimcache:" & quoteShell(cacheDir) &
    " -d:nimonyPlugin" &
    " --path:" & quoteShell(srcLibPath) &
    " --path:" & quoteShell(pluginDir)
  for path in config.paths:
    if path != stdlibDir() and path != pluginDir and path != srcLibPath:
      result.add " --path:"
      result.add quoteShell(path)
  # Forward the stdlib-configuration opt-outs so the plugin is built against
  # the same stdlib variant as the module that uses it (nim-lang/nimony#2155's
  # follow-up: `-d:useLibc` did not reach plugins). An explicit allow-list, not
  # raw command-line forwarding, for the same reason `--base` is not forwarded
  # above. The derived defines (`nimNativeAlloc`/`nimNativeIo`) are NOT
  # forwarded: the child re-derives them from these opt-outs.
  for d in ["useLibc", "useLibcIo", "useMimalloc"]:
    if config.isDefined(d):
      result.add " -d:"
      result.add d

proc runValidatorOnPlugin(config: NifConfig; nf, checkCache: string) =
  ## Run the plugin validator on `nf` before compiling it. Skipped when
  ## --novalidate was passed or when the validator binary is not available
  ## (a fresh clone before `hastur build validator` has run).
  ##
  ## The validator reads the *semchecked* module, so the plugin is first
  ## semchecked into a scratch cache of its own -- `check`, so the sub-compile
  ## stops after sem and never reaches code generation.
  ##
  ## A failing sem run is not reported here. It means the plugin does not
  ## compile, and the build that follows says so with the real diagnostics.
  if config.noValidate: return
  let v = findTool("validator")
  if not os.fileExists(v):
    echo "warning: validator binary not found at ", v,
         "; skipping plugin validation (build it with `hastur build validator` ",
         "or pass --novalidate to silence this)"
    return
  makePluginCache checkCache
  let checkCmd = pluginCompileCmd(config, checkCache) & " check " & quoteShell(nf)
  # Captured, not inherited: on failure the build below reports the same
  # diagnostics, and printing them twice would only obscure them.
  var checkCode = 0
  try:
    # Positional, not `.exitCode`: nimony's own `execCmdEx` returns an unnamed
    # tuple, and this module is compiled by nimony when it compiles itself.
    checkCode = int(execCmdEx(checkCmd)[1])
  except:
    checkCode = -1
  if checkCode != 0: return
  exec quoteShell(v) & " --nimcache:" & quoteShell(checkCache) & " " & quoteShell(nf)

proc buildPluginInto(config: NifConfig; nf, exefile, scratch: string) =
  ## Build a plugin's `.nim` source `nf` as the executable `exefile`. Plugins
  ## import `lib/plugins.nim` and are compiled by Nimony itself. The
  ## sub-compiles work in `<scratch>_d` (the build) and `<scratch>_v` (the
  ## validator's sem-only run); the caller owns these directories.
  ##
  ## The link target is a temporary sibling, never `exefile` itself, and it is
  ## moved into place as one operation once it is complete. Nothing ever opens
  ## the live path for writing: a process executing the old plugin keeps its
  ## inode, and one starting afterwards sees a complete file.
  runValidatorOnPlugin(config, nf, scratch & "_v")
  let cacheDir = scratch & "_d"
  makePluginCache cacheDir
  let staging = atomicTempPath(exefile)
  var cmd = pluginCompileCmd(config, cacheDir)
  cmd.add " -o:"
  cmd.add quoteShell(staging)
  cmd.add " c "
  cmd.add quoteShell(nf)
  exec cmd
  if not vfsMoveInto(staging, exefile):
    vfsRemove staging   # no `.tmp.NNN` litter
    quit "FAILURE: cannot install plugin executable " & exefile

proc buildPlugin*(config: NifConfig; nf, exefile: string) =
  ## `nimsem plugin <nf> <exefile>`: the build-graph way to get a plugin.
  ## The dependency scanner lists every `{.plugin.}` a module declares, so
  ## nifmake builds the executable once, as a node of its own, before any of
  ## the nimsem runs that may execute it. Being the only builder by
  ## construction, this path may keep incremental caches next to the exe.
  buildPluginInto(config, nf, exefile, exefile)

proc ensurePlugin(config: NifConfig; nf, exefile: string) =
  ## The lazy fallback for a plugin the build graph did not provide: a
  ## `{.plugin.}` pragma nifler could not see, or a hand-driven `nimsem m`.
  ## Several nimsem processes may get here for the same plugin at once, so it
  ## shares nothing that could be corrupted: the sub-compiles run in throwaway
  ## per-process caches, and the executable is installed atomically. The
  ## worst case of a collision is a redundant compile.
  if not needsRecompile(nf, exefile): return
  let scratch = atomicTempPath(exefile)
  buildPluginInto(config, nf, exefile, scratch)
  vfsRemoveTree scratch & "_d"
  vfsRemoveTree scratch & "_v"

proc writeFileIfChanged(file, content: string) {.canRaise.} =
  if os.fileExists(file) and readFile(file) == content:
    # do not touch the timestamp
    discard "nothing to do here"
  else:
    writeFile file, content

const pluginTempBase = "tmp"

proc bifPluginInput(input: var TokenBuf; firstName: string): string =
  ## Serializes a plugin input as in-memory `.bif` bytes: a private-pool copy
  ## of `input` behind a leading `(unusedname firstName)` tree — the binary
  ## carrier of what the text protocol's `.unusedname` directive transported
  ## (bif has no directive channel; `plugins.loadPluginTree` peels the tree
  ## off). The private pool matters twice over: the cache filename is a
  ## checksum of these bytes and the global pool's ids depend on compile
  ## history, and storing a global-pool buffer would write that whole pool.
  var buf = nifcore.createTokenBuf(input.len + 4)
  nifcore.openTag(buf, registerTag(buf.tags, UnusedNameTag))
  nifcore.addSymUse(buf, firstName)
  nifcore.closeTag(buf)
  var n = beginRead(input)
  while n.hasMore:
    nifcore.addSubtree(buf, n)
    skip n
  endRead(n)
  result = storeToString(buf)

proc registerGeneratedSymbols(c: var SemContext; firstDisamb: int;
                              nextName: string) =
  if nextName.len == 0:
    return

  var nextBase = ""
  var nextDisamb = 0
  assert splitLocalSymName(nextName, nextBase, nextDisamb) and
    nextBase == pluginTempBase and nextDisamb >= firstDisamb,
    "invalid .unusedname returned by plugin"

  for disamb in firstDisamb ..< nextDisamb:
    let name = pluginTempBase & "." & $disamb
    c.freshSyms.incl pool.syms.getOrIncl(name)

  if nextDisamb > firstDisamb:
    c.locals[pluginTempBase] = nextDisamb - 1

# ── Plugin output ───────────────────────────────────────────────────────────
#
# A plugin's output is memoized under `checksum(input)`. That is exact for a
# plugin that computes from its input alone and wrong for one that also reads a
# file — a schema, a template, a table of constants: the file changes, the
# input does not, and the memo is served forever (nim-lang/nimony#1378).
# `plugins.dependsOn` reports such reads, and the paths travel back as a leading
# `(dependency …)` tree beside the `(unusedname …)` gensym hint. Both are
# sidecars: peeled off here, never part of the tree that is sem-checked. The
# same list, read out of a cached output, decides whether the memo still holds.

type
  PluginOutput = object
    buf: TokenBuf       ## the whole output; a bif carries private pools, text
                        ## is parsed into the global ones
    nextName: string    ## the gensym hint, or ""
    deps: seq[string]   ## the `(dependency …)` paths

proc isSidecar(o: PluginOutput; n: Cursor): bool =
  n.kind == TagLit and
    tagName(o.buf.tags, n.cursorTagId) in [UnusedNameTag, DependencyTag]

proc peelSidecars(o: var PluginOutput) =
  ## Reads the leading sidecar trees into `nextName` and `deps`.
  if o.buf.len > 0:
    var n = beginRead(o.buf)
    while n.hasMore and o.isSidecar(n):
      let tag = tagName(o.buf.tags, n.cursorTagId)
      n.into:
        while n.hasMore:
          if tag == UnusedNameTag and n.kind == Symbol:
            o.nextName = symName(n)
          elif tag == DependencyTag and n.kind == StrLit:
            o.deps.add strVal(n)
          skip n
    endRead(n)

proc loadPluginOutput(outputFile: string; info: NifLineInfo): PluginOutput =
  result = PluginOutput(nextName: "", deps: @[])
  if isBifFile(outputFile):
    # Binary output: the tokens carry absolute line infos, so no parentSeed
    # resolution is needed, and the gensym hint is the `(unusedname X)` tree.
    var m = bif.load(outputFile)
    result.buf = move m.buf
  else:
    # Text output (hand-written or third-party plugins): the gensym hint is
    # the `.unusedname` directive. `parse` reads ONE tree per call, so each
    # sidecar costs a call of its own before the output proper is reached.
    result.buf = createTokenBuf(30)
    var r = rd.open(outputFile)
    result.nextName = rd.firstUnusedName(r)
    var lastWasSidecar = true
    while lastWasSidecar:
      let start = result.buf.len
      # seed the parse with the invocation site's absolute info: text plugin
      # output copies the (file-less, relative) infos of its input, so without
      # an anchor they resolve to NoFile and diagnostics print as `???`
      parse(r, result.buf, parentSeed = info, denseLineInfo = true)
      lastWasSidecar = result.buf.len > start and
        result.isSidecar(cursorAt(result.buf, start))
    rd.close(r)
  peelSidecars result

proc addPluginBody(dest: var TokenBuf; o: var PluginOutput) =
  ## Appends the output proper, everything after the sidecars, to `dest`.
  ## `addSubtree` re-interns a bif's private-pool content into the global pools.
  if o.buf.len > 0:
    var n = beginRead(o.buf)
    while n.hasMore and o.isSidecar(n):
      skip n
    while n.hasMore:
      addSubtree(dest, n)
      skip n
    endRead(n)

proc memoIsStale(outputFile: string; deps: seq[string]): bool =
  ## True when a file the cached output depended on has changed or vanished
  ## since it was written. A vanished file forces exactly one rerun: the plugin
  ## no longer finds it and so no longer reports it. `vfsMtime` rather than
  ## `lastModTimeOrStale`, which is whole seconds on host Nim: a data file
  ## edited in the same second as the output would tie and read as unchanged.
  ## nifmake compares nanoseconds for the same reason.
  result = false
  let written = vfsMtime(outputFile)
  for d in deps:
    if not vfsExists(d) or vfsMtime(d) > written:
      result = true

proc execPlugin(pluginExe, inputFile, outputFile, inputFileB: string) =
  var cmd = quoteShell(pluginExe) & " " & quoteShell(inputFile) & " " & quoteShell(outputFile)
  if inputFileB.len > 0:
    cmd &= " "
    cmd &= quoteShell(inputFileB)
  exec cmd

proc runPlugin*(c: var SemContext; dest: var TokenBuf; info: NifLineInfo;
                pluginName: string; input: var TokenBuf;
                additionalInput: var TokenBuf) =
  ## Runs a plugin with a gensym hint and registers every generated local
  ## symbol as fresh for subsequent semantic checking. The inputs are written
  ## as binary `.bif` behind the unchanged `.in.nif`/`.types.nif` names — the
  ## plugin-side loader sniffs the header (and still accepts text, so
  ## hand-written inputs keep working). Inspect one with `niftools bif2nif`.
  let firstDisamb = c.locals.getOrDefault(pluginTempBase, -1) + 1
  let firstName = pluginTempBase & "." & $firstDisamb
  let pluginInput = bifPluginInput(input, firstName)
  let pluginAdditionalInput =
    if additionalInput.len > 0: bifPluginInput(additionalInput, firstName)
    else: ""

  let p = splitFile(pluginName)
  let checksumA =
    if pluginAdditionalInput.len > 0:
      "_" & computeChecksum(pluginAdditionalInput)
    else:
      ""
  let basename = c.g.config.nifcachePath / p.name & "_" &
    computeChecksum(pluginInput) & checksumA
  let inputFile = basename & ".in.nif"
  let outputFile = basename & ".out.nif"
  let inputFileB = basename & ".types.nif"
  let pluginExe = c.g.config.nifcachePath / p.name.addFileExt(ExeExt)

  let nf = resolveFile(c.g.config.paths, getFile(info), pluginName)
  ensurePlugin(c.g.config, nf, pluginExe)

  try:
    writeFileIfChanged(inputFile, pluginInput)
    if pluginAdditionalInput.len > 0:
      writeFileIfChanged(inputFileB, pluginAdditionalInput)
  except:
    quit "FAILURE: cannot write plugin input file " & inputFile

  let typesFile = if pluginAdditionalInput.len > 0: inputFileB else: ""
  if needsRecompile(pluginExe, outputFile):
    execPlugin pluginExe, inputFile, outputFile, typesFile
  var output = loadPluginOutput(outputFile, info)
  if memoIsStale(outputFile, output.deps):
    execPlugin pluginExe, inputFile, outputFile, typesFile
    output = loadPluginOutput(outputFile, info)
  for d in output.deps:
    recordFileDep c, d
  addPluginBody dest, output
  registerGeneratedSymbols(c, firstDisamb, output.nextName)

proc runPlugin*(c: var SemContext; dest: var TokenBuf; info: NifLineInfo;
                pluginName: string; input: var TokenBuf) =
  ## Single-input form (template/for-loop/module plugins; type plugins pass
  ## their triggering type definitions as `additionalInput`).
  var noAdditional = nifcore.createTokenBuf(1)
  runPlugin(c, dest, info, pluginName, input, noAdditional)

proc runProgram(file: string; nimcachePath: string; usedModules: HashSet[string];
                commandLineArgs: string;
                sourceDir = ""): tuple[output: string, exitCode: int] =
  # Compile the .p.nif through the full pipeline, then run the resulting
  # binary. Compilation must keep the outer cwd (nimcache paths are relative
  # to the invoking compile). Only the execution step uses `workingDir` so
  # relative paths like `doc/version.md` resolve next to the caller module.
  let nimonyExe = findTool("nimony")
  let compileCmd = quoteShell(nimonyExe) & commandLineArgs &
    " --nimcache:" & quoteShell(nimcachePath) &
    " s " & quoteShell(file)
  try:
    result = execCmdEx(compileCmd)
  except:
    result = (output: "failed to run: " & compileCmd, exitCode: -1)
  if result.exitCode != 0: return

  let modname = extractModuleSuffix(file)
  let exe = nimcachePath / modname / splitFile(file).name.addFileExt(ExeExt)
  # The child may start in `sourceDir`; keep the exe path absolute so it
  # still resolves against the outer compile's cwd, not the module dir.
  var exeToRun = exe
  if sourceDir.len > 0:
    try:
      exeToRun = os.absolutePath(exe)
    except:
      return (output: "failed to resolve exe path: " & exe, exitCode: -1)
  let runCmd = quoteShell(exeToRun)
  try:
    result = execCmdEx(runCmd, workingDir = sourceDir)
  except:
    result = (output: "failed to run: " & runCmd, exitCode: -1)

const
  writeNifModuleSuffix* = "wriwhv7qv"

proc prepareEval*(c: var SemContext): string =
  if not c.checkedForWriteNifModule:
    c.checkedForWriteNifModule = true
    if not os.fileExists(c.g.config.nifcachePath / writeNifModuleSuffix & ".s.nif"):
      # precompile the module.
      # Forward the outer compile's CLI args (notably `--cc`) so the
      # inner nimony emits a build file whose `nimsem` cmd-line MATCHES
      # what the outer build file uses. Otherwise nifmake's per-cmd
      # staleness check sees a different argv for `nimsem ... m
      # sysvq0asl.p.nif`, decides the existing `sysvq0asl.s.nif` is
      # stale, and tries to overwrite it — which on Windows fails because
      # the outer nimsem (currently paused waiting on this exec) still
      # has it mmap'd. The outer's args live on `c.commandLineArgs`.
      let nimonyExe = findTool("nimony")
      var cmd = quoteShell(nimonyExe) & c.commandLineArgs &
        " --nimcache:" & quoteShell(c.g.config.nifcachePath) &
        " c " & quoteShell(stdlibFile("std/writenif.nim"))
      try:
        let (output, exitCode) = execCmdEx(cmd)
        if exitCode != 0:
          return ensureMove(output)
      except:
        return "failed to run: " & cmd
  return ""

proc runEval*(c: var SemContext; dest: var TokenBuf; srcName: string; src: TokenBuf;
               usedModules: HashSet[string]; sourceDir = ""): string =
  ## Returns an error message if the evaluation failed, "" on success.
  let progfile = c.g.config.nifcachePath / srcName.addFileExt(".p.nif")
  try:
    writeFileAndIndex(progfile, src)

    # Write the .p.deps.nif file so that `nimony s` can find the imports.
    # Always write — `nimony s` opens this unconditionally, so an empty
    # `(stmts)` is needed when the original module has no imports.
    var deps = createTokenBuf(c.importSnippets.len + 4)
    deps.addParLe StmtsS, NoLineInfo
    if c.importSnippets.len > 0:
      deps.add c.importSnippets
    deps.addParRi()
    let depsFile = c.g.config.nifcachePath / srcName & ".p.deps.nif"
    writeFile(depsFile, toString(deps, true))
    let (output, exitCode) = runProgram(progfile, c.g.config.nifcachePath, usedModules,
                                        c.commandLineArgs, sourceDir)
    if exitCode != 0:
      result = ensureMove(output)
    else:
      let outfile = c.g.config.nifcachePath / srcName.addFileExt(".out.nif")
      var r = rd.open(outfile)
      parse(r, dest)
      rd.close(r)
      result = ""  # success: caller interprets "" as no error
  except:
    result = "I/O error while evaluating " & srcName

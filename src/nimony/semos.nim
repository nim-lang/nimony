#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Path handling and `exec` like features as `sem.nim` needs it.

from std / strutils import multiReplace, split, strip, startsWith
import std / [tables, sets, os, envvars, syncio, formatfloat, assertions, dirs, paths]
from std / osproc import execCmdEx

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / [nifchecksums, nifindexes, tooldirs, argsfinder, symparser]
import ".." / lib / nifreader as rd

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
  of StringLit:
    let s = pool.strings[n.litId]
    # string literal could contain a path or .nim extension:
    let name = moduleNameFromPath(s)
    res.add ImportedFilename(path: s, name: name)
    inc n
  of Ident:
    let s = pool.strings[n.litId]
    res.add ImportedFilename(path: s, name: s)
    inc n
  of Symbol:
    var s = pool.syms[n.symId]
    extractBasename s
    res.add ImportedFilename(path: s, name: s)
    inc n
  of ParLe:
    case exprKind(n)
    of OchoiceX, CchoiceX:
      inc n
      if n.kind == ParRi:
        hasError = true
      else:
        filenameVal(n, res, hasError, allowAs)
      while n.hasMore: skip n
      consumeParRi n
    of QuotedX:
      let s = pool.strings[takeUnquoted(n)]
      res.add ImportedFilename(path: s, name: s)
    of CallX, InfixX:
      var x = n
      skip n # ensure we skipped it completely
      inc x
      let opId = takeIdent(x)
      if opId == StrId(0):
        hasError = true
        return
      let op = pool.strings[opId]
      if op == "as":
        if not allowAs:
          hasError = true
          return
        if x.kind == ParRi:
          hasError = true
          return
        var rhs = x
        skip rhs # skip lhs
        if rhs.kind == ParRi:
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
      inc x
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
      let orig = n
      inc n
      let start = res.len
      if n.kind == ParRi:
        hasError = true
      else:
        filenameVal(n, res, hasError, allowAs)
        var success = false
        if n.substructureKind == PragmasU:
          inc n
          if n.substructureKind == KvU:
            inc n
            if n.kind == Ident and pool.strings[n.litId] == "plugin":
              inc n
              if n.kind == StringLit:
                for i in start ..< res.len:
                  res[i].plugin = pool.strings[n.litId]
                  success = true
                inc n
                if n.kind == ParRi: inc n
                else: hasError = true
        if not success:
          n = orig
          skip n
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

proc parseFile*(nimFile: string; paths: openArray[string], nifcachePath: string): TokenBuf =
  let nifler = findTool("nifler")
  let name = moduleSuffix(nimFile, paths)
  let src = nifcachePath / name & ".p.nif"
  exec quoteShell(nifler) & " --portablePaths --deps parse " & quoteShell(nimFile) & " " &
    quoteShell(src)

  var stream = nifstreams.open(src)
  try:
    discard processDirectives(stream.r)
    result = fromStream(stream)
  finally:
    nifstreams.close(stream)

proc getFile*(info: PackedLineInfo): string =
  let fid = unpack(pool.man, info).file
  if fid.isValid:
    result = pool.files[fid]
  else:
    result = ""

proc selfExec*(c: var SemContext; file: string; moreArgs: string) =
  let nimonyExe = findTool("nimony")
  exec quoteShell(nimonyExe) & c.commandLineArgs & moreArgs & " --ischild m " & quoteShell(file)
  #exec os.getAppFilename() & c.commandLineArgs & moreArgs & " --ischild m " & quoteShell(file)

# ------------------ plugin handling --------------------------

proc runValidatorOnPlugin(c: var SemContext; nf: string) =
  ## Run the plugin validator on `nf` before compiling it. Skipped when
  ## --novalidate was passed or when the validator binary is not available
  ## (a fresh clone before `hastur build validator` has run).
  if c.g.config.noValidate: return
  let v = findTool("validator")
  if not os.fileExists(v):
    echo "warning: validator binary not found at ", v,
         "; skipping plugin validation (build it with `hastur build validator` ",
         "or pass --novalidate to silence this)"
    return
  exec quoteShell(v) & " " & quoteShell(nf)

proc compilePlugin(c: var SemContext; info: PackedLineInfo; nf, exefile: string) =
  ## Build a plugin's `.nim` source as an executable. Plugins import
  ## `lib/plugins.nim` and are compiled by Nimony itself.
  runValidatorOnPlugin(c, nf)
  let pluginDir = nimonyDir() / "src/nimony/lib"
  let pluginCache = exefile & "_d"
  try:
    when defined(nimony):
      createDir(path(pluginCache))
    else:
      createDir(Path(pluginCache))
  except:
    quit "FAILURE: cannot create directory " & pluginCache
  # `--nimcache:<pluginCache>` keeps the sub-compile's intermediate NIF
  # artefacts in a per-plugin scratch dir so parallel test workers don't
  # fight over `nimcache/` entries.
  #
  # Forward outer user search paths so plugin self-compilation computes the
  # same module identities for user modules. Internal Nimony library paths are
  # supplied below and deliberately not forwarded from the caller's path file.
  # Do not forward the raw command line: it can contain `--base`, which would
  # make plugin child compiles read caller-local nimony.paths files.
  let nimonyExe = findTool("nimony")
  let srcLibPath = nimonyDir() / "src" / "lib"
  # Compiled via the libc-free NATIVE backend (`--native c`): the plugin builds from
  # `.nim` source into its own per-plugin cache, so there's no config-mix (unlike macro
  # plugins). Two native-backend bugs previously blocked this, both now fixed in
  # ../nativenif: (1) a nifasm frame-layout bug where outgoing stack-arg slots aliased
  # the caller's locals (fixed-frame arg-area reservation was gated to AArch64 only);
  # (2) an arkham codegen bug where a narrowing `cast[uintN](v)` used as an arithmetic
  # operand narrowed the live source variable `v` in place — this corrupted the Ryu
  # float formatter, so `tnifoverloads` (emits `addFloatLit`) printed garbage floats.
  var cmd = quoteShell(nimonyExe) &
    " --nimcache:" & quoteShell(pluginCache) &
    " --path:" & quoteShell(srcLibPath) &
    " --path:" & quoteShell(pluginDir)
  for path in c.g.config.paths:
    if path != stdlibDir() and path != pluginDir and path != srcLibPath:
      cmd.add " --path:"
      cmd.add quoteShell(path)
  cmd.add " -o:"
  cmd.add quoteShell(exefile)
  cmd.add " --native c "
  cmd.add quoteShell(nf)
  exec cmd

proc writeFileIfChanged(file, content: string) {.canRaise.} =
  if os.fileExists(file) and readFile(file) == content:
    # do not touch the timestamp
    discard "nothing to do here"
  else:
    writeFile file, content

const pluginTempBase = "tmp"

proc addUnusedName(input, name: string): string =
  result = "(.unusedname " & name & ")\n"
  result.add input

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

proc runPlugin*(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo;
                pluginName: string; input: string; additionalInput = "") =
  ## Runs a plugin with a NIF `.unusedname` hint and registers every generated
  ## local symbol as fresh for subsequent semantic checking.
  let firstDisamb = c.locals.getOrDefault(pluginTempBase, -1) + 1
  let firstName = pluginTempBase & "." & $firstDisamb
  let pluginInput = addUnusedName(input, firstName)
  let pluginAdditionalInput =
    if additionalInput.len > 0: addUnusedName(additionalInput, firstName)
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
  if needsRecompile(nf, pluginExe):
    compilePlugin(c, info, nf, pluginExe)

  try:
    writeFileIfChanged(inputFile, pluginInput)
    if pluginAdditionalInput.len > 0:
      writeFileIfChanged(inputFileB, pluginAdditionalInput)
  except:
    quit "FAILURE: cannot write plugin input file " & inputFile

  if needsRecompile(pluginExe, outputFile):
    var cmd = quoteShell(pluginExe) & " " & quoteShell(inputFile) & " " & quoteShell(outputFile)
    if pluginAdditionalInput.len > 0:
      cmd &= " "
      cmd &= quoteShell(inputFileB)
    exec cmd
  var s = nifstreams.open(outputFile)
  var nextName = ""
  try:
    nextName = rd.firstUnusedName(s.r)
    parse s, dest, NoLineInfo
  finally:
    close s
  registerGeneratedSymbols(c, firstDisamb, nextName)

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
    writeFile deps, depsFile

    let (output, exitCode) = runProgram(progfile, c.g.config.nifcachePath, usedModules,
                                        c.commandLineArgs, sourceDir)
    if exitCode != 0:
      result = ensureMove(output)
    else:
      let outfile = c.g.config.nifcachePath / srcName.addFileExt(".out.nif")
      var s = nifstreams.open(outfile)
      try:
        parse s, dest, NoLineInfo
      finally:
        close s
      result = ""  # success: caller interprets "" as no error
  except:
    result = "I/O error while evaluating " & srcName

## Hastur - Tester tool for Nimony and its related subsystems (Leng etc).
## (c) 2024-2025 Andreas Rumpf

when defined(windows):
  when defined(gcc):
    when defined(x86):
      {.link: "../icons/hastur.res".}
    else:
      {.link: "../icons/hastur_icon.o".}

import std / [syncio, assertions, parseopt, strutils, times, os, osproc, algorithm, typedthreads, locks]
when defined(windows):
  import std/winlean
else:
  import std/posix

import lib / [nifindexes, lineinfos, argsfinder]
import gear2 / modnames

const
  Version = "0.6.0"
  Usage = "hastur - tester tool for Nimony Version " & Version & """

  (c) 2024-2026 Andreas Rumpf
Usage:
  hastur [options] [command] [arguments]

Commands:
  build [all|nimony|nifler|hexer|lengc|shoggoth|nifmake|nj|vl|validator|dagon|pnak|arkham|nifasm|native]   build selected tools (default: all).
  tiers                compile every module on the bootstrap list with nimony.
  boot [options]       Self-host the *full* nimony toolchain (nimony,
                       nimsem, hexer). `bin0/` is a fresh copy of the
                       host-Nim-built toolchain; `binN/` is `binN-1/`'s
                       nimony recompiling all three from source. Runs a
                       fixed number of self-compile passes and leaves the
                       results in place — nothing is installed back to
                       `bin/`. Extra args are forwarded to every
                       `nimony c` invocation.
  selfcheck            full compiler regression check: rebuilds the nimony
                       toolchain (nimony+nimsem+hexer share `programs.nim`),
                       runs `tiers`, then `boot --valgrind`. Use this
                       after touching any module the compiler itself imports.
  <dir>                run the test tree rooted at <dir> (see Files below):
                       each directory is either a setup.nim custom runner or
                       the built-in nimony runner, recursively. This is the
                       general entry point — point it at any suite.
  all                  run the whole suite: `<tests>` + `<examples>`.
  native               run the curated native-backend regression set through
                       `nimony n` (arkham + nifasm, from the sibling
                       `../nativenif` checkout). See `NativeTestDirs`/`Files`.
  lengc                 run Leng tests.
  test <file>/<dir>    run a single test <file>, or a flat <dir> of tests.
  bug [file]           build nimony+hexer and compile <file> to fill nimcache/.
                       If no file is provided `bug.nim` is used.
  rep                  repeat the last failing tool command from the session.
  record <file> <tout> track the results to make it part of the test suite.
  install              write activation script(s) at the project root that
                       prepend the toolchain dirs to `$PATH` for the current
                       shell. On Windows, also download MinGW+LLVM (gcc,
                       clang, lld) and Nim's DLL deps into `external/`.
  clean                remove all generated files.
  sync [new-branch]    delete current branch and pull the latest
                       changes from remote. Optionally creates a new branch.

Arguments are forwarded to the Nimony compiler.

Options:
  --overwrite           overwrite the selected test results
  --ast                 track the contents of the AST too
  --codegen             track the contents of the code generator too
  --version             show the version
  --help                show this help
  --forward:OPTION      pass an option to the Nimony compiler
  --release             build in release mode
  --jobs:N|auto         run up to N tests in parallel (auto = #cores)
  --cachedir:PATH       use PATH instead of `nimcache/` for intermediates
  --bindir:PATH         resolve the toolchain (nimony, lengc, …) from
                        directory PATH instead of hastur's own directory
                        (implies --no-build). Binaries not found there are
                        looked up on `$PATH`.
  --no-build            skip the setup.hastur prep step during the tree walk
  --valgrind            for `boot`: build with -DMI_TRACK_VALGRIND=1 so
                        mimalloc plays nicely with valgrind, then run a
                        valgrind smoke test on the bootstrapped binary.

Files (per test directory, all optional):
  setup.nim             a custom runner program that owns this directory and
                        its subtree: hastur compiles+runs it (it imports
                        hastur as the test kit) and takes its exit code as the
                        verdict. For suites that aren't a folder of inputs
                        (boot, incremental, validator) or need a bespoke tool
                        (nj, vl, dagon, pnak, hexer, controlflow, contracts).
  setup.hastur          prep for a built-in-runner directory: each line is a
                        hastur subcommand (e.g. `build nimony`) run before the
                        tests beneath it. `tests/setup.hastur` builds the
                        toolchain for the whole sweep.
  hastur.mode           this directory's category for the built-in nimony
                        runner: nosystem, track, compat, valgrind, opt, or
                        skip (excluded from the sweep, but still run when
                        pointed at directly). Absent means normal.
"""

proc quitWithText*(s: string) =
  stdout.write(s)
  stdout.flushFile()
  quit(0)

proc error*(msg: string) =
  when defined(debug):
    writeStackTrace()
  stdout.write("[Error] ")
  stdout.write msg
  stdout.write "\n"
  quit 1

proc writeHelp() = quitWithText(Usage)
proc writeVersion() = quitWithText(Version & "\n")

type
  TestCounters = object
    total: int
    failures: int

proc failure(c: var TestCounters; file, expected, given: string) =
  inc c.failures
  var m = file & " --------------------------------------\nFAILURE: expected:\n"
  m.add expected
  m.add "\nbut got\n"
  m.add given
  m.add "\n"
  echo m

proc failure(c: var TestCounters; file, msg: string) =
  inc c.failures
  let m = file & " --------------------------------------\nFAILURE: " & msg & "\n"
  echo m

proc diffFiles(c: var TestCounters; file, a, b: string; overwrite: bool) =
  if not os.sameFileContent(a, b):
    if overwrite:
      copyFile(b, a)
    else:
      let gitCmd = "git diff --no-index $1 $2" % [a.quoteShell, b.quoteShell]
      let (diff, diffExitCode) = execCmdEx(gitCmd)
      if diffExitCode == 0:
        failure c, file, diff
      else:
        failure c, file, gitCmd & "\n" & diff

const
  ErrorKeyword = "Error:"

  targetIs64bit = sizeof(int) == 8
    ## The checked-in golden `.nim.c` and `.nif` outputs are generated for a
    ## 64-bit target (e.g. `(i +64)`, `IL64(...)`, `NIM_INTBITS 64`). nimony
    ## defaults its target word size to the host CPU, so on a 32-bit host the
    ## generated code legitimately differs and those diffs would spuriously
    ## fail. Per nim-lang/nimony#1569, only run the golden-file comparisons on
    ## 64-bit hosts. (`sizeof(int)` reflects the host hastur was built for,
    ## which is the same machine that runs nimony for the tests.)

type
  LineInfo = object
    line, col: int
    filename: string

proc extractMarkers(s: string): seq[LineInfo] =
  ## Extracts markers like #[  ^suggest]# from a .nim file and translates the marker
  ## into (line, col) coordinates along with the marker's content which is 'suggest'
  ## in the example.
  var i = 0
  var line = 1
  var col = 1
  var markerAt = high(int)
  var inComment = 0
  var inLineComment = false
  result = @[]
  while i < s.len:
    case s[i]
    of '#':
      if i+1 < s.len and s[i+1] == '[':
        inc inComment
      else:
        inLineComment = true
    of ']':
      if i+1 < s.len and s[i+1] == '#':
        if inComment > 0:
          dec inComment
          markerAt = high(int)
    of '^':
      if inComment > 0 or inLineComment:
        markerAt = i
        result.add LineInfo(line: line-1, col: col, filename: "")
        #           ^ a marker refers to the previous line
    of '\n':
      inc line
      col = 0
      if inLineComment:
        inLineComment = false
        markerAt = high(int)
    of '\r':
      dec col
    else: discard
    if markerAt < i:
      result[^1].filename.add s[i]
    inc i
    inc col

proc markersToCmdLine(s: seq[LineInfo]; file: string): string =
  result = ""
  for x in items(s):
    case x.filename
    of "usages":
      result.add " --usages:" & file & "," & $x.line & "," & $x.col
    of "def":
      result.add " --def:" & file & "," & $x.line & "," & $x.col
    else:
      result.add " --track:" & $x.line & ":" & $x.col & ":" & x.filename

proc defaultToolchainDir(): string =
  ## Where hastur looks for its sibling toolchain binaries by default: the
  ## directory hastur itself lives in. `tester.nim` builds hastur into `bin/`
  ## alongside `nimony`, `lengc`, `hexer`, … so a `bin/hastur` invocation
  ## finds them regardless of the current working directory — no `--bindir`
  ## and no "run from the repo root" requirement. When hastur's own directory
  ## has no toolchain (e.g. a `nim c -r` run whose binary sits in a nimcache
  ## temp dir) fall back to the cwd-relative `bin/`.
  result = getAppFilename().parentDir
  if not fileExists(result / "nimony".addFileExt(ExeExt)):
    result = "bin"

var toolchainDir* = defaultToolchainDir()
  ## Directory the toolchain binaries (`nimony`, `lengc`, `hexer`, …) are
  ## resolved from. Defaults to hastur's own directory (its siblings);
  ## `--bindir:PATH` overrides it to point at any prebuilt/installed
  ## toolchain, and binaries missing there are looked up on `$PATH`.

proc toolExe*(name: string): string =
  ## Resolve a toolchain binary: `toolchainDir/<name>` if present, otherwise
  ## `<name>` on `$PATH`. The `$PATH` fallback lets an installed toolchain
  ## drive the tests. When neither exists we still return the `toolchainDir`
  ## path so the ensuing "not found" failure names the expected location.
  result = toolchainDir / name.addFileExt(ExeExt)
  if fileExists(result): return
  let onPath = findExe(name)
  if onPath.len > 0: result = onPath

proc execLocal(exe, cmd: string): (string, int) =
  result = osproc.execCmdEx(toolExe(exe).quoteShell & " " & cmd)

type
  Category = enum
    Normal, # normal category
    Basics, # basic tests: These are processed with --noSystem
    Tracked # tracked tests: These are processed and can contain "track info"
            # for line, col, filename extraction (useful for nimsuggest-like tests)
    Compat # compatibility mode tests
    Valgrind # valgrind tests
    Optimized # tests compiled with --opt:speed (exercise the shoggoth passes)
    Skip # `hastur.mode = skip`: the tree walk ignores this directory and its
         # subtree (non-suite dirs: fixtures, inputs owned by another runner)

var nimcacheDir* = "nimcache"
  ## Directory used for compiler intermediates. Per-test parallel runs
  ## point this at a unique sub-directory so concurrent tests don't
  ## race on the same `nimcache/` artifacts.

var parallelJobs* = 1
  ## How many tests `testDir` runs concurrently. 1 = serial (current
  ## behavior). `--jobs:N` on the command line overrides; `--jobs:auto`
  ## uses `countProcessors()`.

var skipBuild* = false
  ## Set by the parallel test runner on its worker invocations: the
  ## parent has already rebuilt nimony / lengc before kicking off the
  ## pool, so each worker skips the rebuild. Otherwise every worker
  ## spends seconds re-running `nim c` for nothing.

proc toCommand(cat: Category): string =
  case cat
  of Basics: "m"
  of Tracked: "check --silentMake"
  of Optimized: "c --silentMake --opt:speed"
  of Normal, Compat, Valgrind, Skip: "c --silentMake"

proc execNimony(cmd: string; cat: Category): (string, int) =
  let cacheArg =
    if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
    else: ""
  result = execLocal("nimony", toCommand(cat) & " " & cacheArg & cmd)

proc execNimonyNative(cmd: string): (string, int) =
  ## Compile with the C-FREE NATIVE backend (`nimony n` → arkham emits typed asm-NIF,
  ## nifasm links a static libc-free executable). Mirrors `execNimony` but selects the
  ## `n` command instead of `c`/`m`.
  let cacheArg =
    if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
    else: ""
  result = execLocal("nimony", "n --silentMake --isMain " & cacheArg & cmd)

const
  HasturSessionFile = "hastur_session.txt"

proc extractToolCmd(output: string): string =
  result = ""
  var i = 0
  while i < output.len:
    if output.continuesWith("nifmake: ", i):
      inc i, len("nifmake: ")
      var tool = ""
      var skip = false
      while i < output.len and output[i] != ' ':
        if output[i] in {'\'', '/'}:
          tool.setLen 0
          skip = false
        elif output[i] == '.':
          skip = true
        else:
          if not skip:
            tool.add output[i]
        inc i
      if tool.len > 0:
        result = "nim c -r src/" & tool & "/" & tool & ".nim "
        while i < output.len and output[i] != '\n':
          result.add output[i]
          inc i
        # the first `nifmake` line is of interest:
        return result
    else:
      inc i

proc loadSessionCmd(): string =
  try:
    result = readFile(HasturSessionFile).strip
  except IOError:
    result = ""

proc saveSessionCmd(cmd: string) =
  if cmd.len > 0:
    writeFile(HasturSessionFile, cmd)

proc pathsForFile(file: string): seq[string] =
  result = @[]
  let baseDir = file.splitFile.dir
  if baseDir.len > 0:
    let pathsFile = findArgs(baseDir, "nimony.paths")
    if pathsFile.len > 0:
      processPathsFile pathsFile, result

proc generatedFile(orig, ext: string): string =
  let name = modnames.moduleSuffix(orig, pathsForFile(orig))
  # Backend (DCE and after) is in nimcache/<mainmod>/, see deps.nim; .s.nif is shared
  result = if ext == ".s.nif": nimcacheDir / name.addFileExt(ext)
           else: nimcacheDir / name / name.addFileExt(ext)

proc generatedExeFile(orig: string): string =
  let name = modnames.moduleSuffix(orig, pathsForFile(orig))
  result = nimcacheDir / name / orig.splitFile.name.addFileExt(ExeExt)

proc removeMakeErrors(output: string): string =
  result = output.strip
  for prefix in ["FAILURE:", "make:", "nifmake:"]:
    let lastLine = rfind(result, '\n')
    if lastLine >= 0:
      if result.continuesWith(prefix, lastLine+1):
        result.setLen lastLine
    elif result.startsWith(prefix):
      result.setLen 0

proc stripValgrindPrefix(s: string): string =
  var i = 0
  if i < s.len and s.continuesWith("==", i):
    inc i, 2
    while i < s.len and s[i] in {'0'..'9'}:
      inc i
    if i < s.len and s.continuesWith("==", i):
      inc i, 2
      if i < s.len and s[i] == ' ':
        inc i
  result = s[i..^1]

proc compareValgrindOutput(s1: string, s2: string): bool =
  let marker = "HEAP SUMMARY:"
  let a = s1.find(marker)
  let b = s2.find(marker)
  if a < 0 or b < 0:
    return s1 == s2
  let lines1 = s1[a + marker.len..^1].splitLines()
  let lines2 = s2[b + marker.len..^1].splitLines()
  if lines1.len != lines2.len:
    return false
  for i in 0 .. lines1.high:
    if stripValgrindPrefix(lines1[i]) != stripValgrindPrefix(lines2[i]):
      return false
  return true

let hasValgrind = findExe("valgrind").len > 0
  ## Whether the `valgrind` binary (and, by extension, its dev headers) is
  ## available. mimalloc no longer hard-depends on valgrind, so the suite must
  ## run without it: when absent we neither pass `-DMI_TRACK_VALGRIND=1` (which
  ## would need `<valgrind/valgrind.h>` to compile) nor run the leak checks —
  ## the `.valgrind` tests simply skip rather than failing the whole run.

proc testValgrind(c: var TestCounters; file: string; overwrite: bool; cat: Category; exe: string) =
  if not hasValgrind: return
  let valgrind = file.changeFileExt(".valgrind")
  let hasValgrindFile = valgrind.fileExists()
  if cat == Valgrind or hasValgrindFile:
    let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(
          "valgrind --leak-check=full --error-exitcode=1 " & exe)
    if testProgramExitCode != 0:
      failure c, file, "valgrind program exitcode 0", "exitcode " & $testProgramExitCode

    if hasValgrindFile:
      let valgrindSpec = readFile(valgrind).strip
      let success = compareValgrindOutput(valgrindSpec, testProgramOutput.strip)
      if not success:
        if overwrite:
          writeFile(valgrind, testProgramOutput)

        failure c, file, valgrindSpec, testProgramOutput

proc echoTestSuccess(file: string) =
  echo "SUCCESS ", file

proc testFile(c: var TestCounters; file: string; overwrite: bool; cat: Category; forward: string) =
  #echo "TESTING ", file
  let failuresBefore = c.failures
  inc c.total
  var nimonycmd = "--isMain"
  case cat
  of Normal, Valgrind, Optimized, Skip: discard
  of Basics:
    nimonycmd.add " --noSystem"
  of Tracked:
    nimonycmd.add markersToCmdLine(extractMarkers(readFile(file)), file)
  of Compat:
    nimonycmd.add " --compat"
  if forward.len != 0:
    nimonycmd.add ' '
    nimonycmd.add forward
  when defined(linux):
    # Only request valgrind-tracked mimalloc when valgrind is actually present;
    # the flag pulls in `<valgrind/valgrind.h>`, which a valgrind-less box lacks.
    if hasValgrind:
      nimonycmd.add " --passC:\"-DMI_TRACK_VALGRIND=1\" "
    else:
      nimonycmd.add " "
  else:
    nimonycmd.add " "
  let (compilerOutput, compilerExitCode) = execNimony(nimonycmd & quoteShell(file), cat)

  let msgs = file.changeFileExt(".msgs")

  var expectedExitCode = 0
  if msgs.fileExists():
    let msgSpec = readFile(msgs).strip
    let strippedOutput = removeMakeErrors(compilerOutput)
    let success = msgSpec == strippedOutput
    if not success:
      if overwrite:
        writeFile(msgs, strippedOutput)
      failure c, file, msgSpec, strippedOutput
    expectedExitCode = if msgSpec.contains(ErrorKeyword): 1 else: 0
  elif overwrite and cat == Tracked:
    writeFile(msgs, removeMakeErrors(compilerOutput))
  if compilerExitCode != expectedExitCode:
    failure c, file, "compiler exitcode " & $expectedExitCode, compilerOutput & "\nexitcode " & $compilerExitCode

  if compilerExitCode == 0:
    let cfile = file.changeFileExt(".nim.c")
    if targetIs64bit and cfile.fileExists():
      let nimcacheC = generatedFile(file, ".c")
      diffFiles c, file, cfile, nimcacheC, overwrite

    if cat notin {Basics, Tracked}:
      let exe = file.generatedExeFile()
      let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell exe)
      var output = file.changeFileExt(".output")
      if testProgramExitCode != 0:
        output = file.changeFileExt(".exitcode")
        if not output.fileExists():
          failure c, file, "test program exitcode 0", "exitcode " & $testProgramExitCode & "\n" & testProgramOutput
      if output.fileExists():
        let outputSpec = readFile(output).strip
        let success = outputSpec == testProgramOutput.strip
        if not success:
          if overwrite:
            writeFile(output, testProgramOutput)
          failure c, file, outputSpec, testProgramOutput

      when defined(linux):
        testValgrind c, file, overwrite, cat, quoteShell exe

    # Only diff `.nif` expected outputs for `nosystem` tests: these do not
    # depend on `lib/std/system.nim` and so remain stable across system
    # changes. With the phase validator in place, diffing NIF for normal
    # tests causes noisy churn without meaningfully improving coverage.
    if cat == Basics and targetIs64bit:
      let ast = file.changeFileExt(".nif")
      if ast.fileExists():
        let nif = generatedFile(file, ".s.nif")
        diffFiles c, file, ast, nif, overwrite

  if c.failures == failuresBefore:
    echoTestSuccess(file)

proc canRunParallel(cat: Category): bool {.inline.} =
  ## `Compat` and `Basics` reset `nimcache/` around the loop and so are
  ## not parallel-safe with the current single-cache layout. Other
  ## categories use isolated per-test cache dirs and parallelize fine.
  cat notin {Compat, Basics}

proc warmupSharedCache(): string =
  ## Compile `tools/warmup.nim` once into `nimcache/warmup/` so each
  ## parallel test can start with system + common stdlib bundles already
  ## present. Returns the warmup cache directory, or "" on opt-out
  ## (warmup source missing or compile failed — tests still work, just
  ## without the savings).
  const warmupSrc = "tools/warmup.nim"
  if not fileExists(warmupSrc):
    return ""
  result = nimcacheDir / "warmup"
  let nimony = toolExe("nimony")
  if not fileExists(nimony):
    return ""
  let cmd = nimony.quoteShell & " c --nimcache:" & result.quoteShell &
            " " & warmupSrc.quoteShell
  let t0 = epochTime()
  let exit = execShellCmd(cmd)
  let dt = epochTime() - t0
  if exit != 0:
    stderr.writeLine "warmup: skipping (compile failed): " & cmd
    return ""
  if dt > 0.5:
    # Only report cold compiles; subsequent calls in the same `hastur all`
    # run are sub-second no-ops thanks to nimony's incremental build, and
    # printing them per-category just adds noise.
    echo "warmup compiled in ", formatFloat(dt, ffDecimal, precision=2), "s."

var sharedObjectsPrebuilt = false
  ## `nimcache_static/` holds object files that don't depend on per-project
  ## state and are reused across every build — currently just mimalloc's
  ## `static.o`. nifmake's `needsRebuild` is purely mtime-based, so when that
  ## `.o` is missing on a cold cache every parallel worker independently
  ## decides to (re)compile `static.c` into the same shared path at once.
  ## The concurrent `cc … -o static.o` writes clobber each other and a
  ## half-written object links with `undefined reference to mi_malloc`. We
  ## build it once, serially, before any worker starts; thereafter the file
  ## exists and every worker's staleness check skips it.

proc prebuildSharedObjects(forward: string) =
  ## Compile a trivial program once so the shared `nimcache_static/` object
  ## files exist before the parallel pool launches. Idempotent across the
  ## many `parallelTestDir` calls in a single `hastur` run (only the first
  ## does real work; once `static.o` is present the build is a no-op).
  ##
  ## `forward` MUST be the same flag string the test workers pass to nimony
  ## (e.g. `--cc:clang` on Windows CI). `static.o` lands in the shared
  ## `nimcache_static/` and is keyed only by mtime, so once we build it the
  ## workers reuse it verbatim — if we built it with a different compiler than
  ## the workers link with, the result is an ABI mismatch. Concretely: on
  ## Windows the tester forwards `--cc:clang` (clang uses native PE TLS); a
  ## prebuild with the default gcc emits gthr/emulated-TLS `static.o`, and the
  ## clang+lld worker link then fails with `undefined symbol: pthread_*`.
  if sharedObjectsPrebuilt: return
  sharedObjectsPrebuilt = true
  let nimony = toolExe("nimony")
  if not fileExists(nimony):
    return
  let cache = nimcacheDir / "prebuild_static"
  let src = cache / "prebuild_static.nim"
  try:
    createDir cache
    # Any program that pulls in `system` triggers the mimalloc `static.c`
    # build pragma; a bare `discard` is enough.
    writeFile(src, "discard\n")
  except OSError, IOError:
    return
  var cmd = nimony.quoteShell & " c --silentMake --nimcache:" & cache.quoteShell
  # Same compiler/link flags the workers use (`--cc:`, `--passL:` …), so the
  # shared `static.o` matches the toolchain the workers link with.
  if forward.len > 0:
    cmd.add ' '
    cmd.add forward
  # Match `testFile`'s per-platform flags so the prebuilt `static.o` is the
  # exact artifact the tests want (valgrind-tracked mimalloc on Linux).
  #
  # mimalloc's build pragma no longer bakes in `-DMI_TRACK_VALGRIND=1` (that
  # made the valgrind dev headers a hard build dependency for every nimony
  # program); valgrind tracking is now requested purely via this `--passC`.
  # But the shared `static.o` is keyed only by mtime, so a prior *non*-valgrind
  # build (e.g. a plain `bin/nimony c foo.nim`) can leave a stale, untracked
  # `static.o` that nifmake would happily reuse — silently running the valgrind
  # tests against non-tracked mimalloc. Delete it so this valgrind-tracked
  # variant is always freshly produced.
  when defined(linux):
    if hasValgrind:
      try: removeFile("nimcache_static" / "static.o")
      except OSError: discard
      cmd.add " --passC:\"-DMI_TRACK_VALGRIND=1\""
  cmd.add ' ' & src.quoteShell
  if execShellCmd(cmd) != 0:
    # Non-fatal: if this fails the tests still run, just without the
    # pre-built shared object (and may hit the original race). Surface it so
    # the cause is visible rather than silently degrading.
    stderr.writeLine "prebuild: shared object compile failed: " & cmd

var warmupCopySeconds: float = 0
  ## Aggregate prefill cost across one parallel run, reported alongside
  ## the test counts.

proc copyPreservingMtime(src, dst: string) =
  ## Copy `src` to `dst` and stamp `dst` with `src`'s mtime. Mtime
  ## preservation is load-bearing: `nifmake.needsRebuild` keys off
  ## output-mtime > input-mtime ordering, so a fresh "now" mtime on every
  ## prefilled file would scramble the DAG-order mtimes the warmup set
  ## up and trigger spurious recompiles. Hardlinks would also preserve
  ## mtimes "for free", but they share an inode — when one parallel test
  ## triggers an in-place rewrite of a shared bundle (say a config
  ## difference forces a recompile), every other test holding a hardlink
  ## sees the truncated/partial content and crashes. Copying gives each
  ## test an independent inode, paid for once at prefill.
  try:
    copyFile(src, dst)
    try: setLastModificationTime(dst, getLastModificationTime(src))
    except: discard
  except OSError, IOError:
    discard  # best-effort; falling back to a cold per-test compile is fine

proc prefillFromWarmup(warmupCache, cacheDir: string) =
  if warmupCache.len == 0 or not dirExists(warmupCache):
    return
  let t0 = epochTime()
  for path in walkDirRec(warmupCache, yieldFilter = {pcFile}, relative = true):
    let dst = cacheDir / path
    try: createDir(dst.parentDir)
    except OSError: discard
    copyPreservingMtime(warmupCache / path, dst)
  warmupCopySeconds += epochTime() - t0

type
  ReaderArg = object
    ## Pure value-type passed to a reader thread: just an OS handle and a
    ## pointer to a shared `Lock`. No `ref`, no string transfer across
    ## threads. The worker accumulates output in a thread-local string,
    ## allocated and freed in the same thread it lives in, and prints
    ## under the lock so concurrent slots don't interleave their
    ## per-test output. Earlier designs that handed the string back to
    ## the main thread (via ref, channel, or ptr-string) all hit
    ## `addToSharedFreeListBigChunks` SIGSEGVs in the runtime when ORC
    ## tried to free a worker-allocated big chunk on the main thread —
    ## keeping every alloc and dealloc thread-local sidesteps that.
    handle: int      # cast of the child's stdout `FileHandle` to int.
    lockPtr: pointer # ptr Lock guarding stdout.

proc drainStdout(arg: ReaderArg) {.thread, nimcall.} =
  ## Background reader: pulls bytes off the child's pipe as they arrive so
  ## the child never blocks on a full pipe buffer. The previous one-shot
  ## drain (only after `peekExitCode` reported the child gone) deadlocked
  ## on Windows: clang on the generated C emits enough `-W…-cast`
  ## warnings during a normal compile to fill the ~4KB pipe buffer, the
  ## child then blocks on its next write, the parent's `peekExitCode`
  ## never advances past -1, and the whole `--jobs:auto` run hangs
  ## producing zero output. Streaming as we go fixes that.
  ##
  ## At EOF we flush the accumulated buffer to stdout under
  ## `lockPtr[]` so the per-test block stays atomic relative to other
  ## slots' reads.
  var buf = newStringOfCap(1 shl 12)
  var tmp = newString(4096)
  while true:
    var n: int = 0
    when defined(windows):
      var bytesRead: int32 = 0
      let ok = winlean.readFile(cast[Handle](arg.handle), tmp[0].addr,
                                tmp.len.int32, addr bytesRead, nil)
      # `readFile` returns 0 on error; ERROR_BROKEN_PIPE is the normal EOF
      # when the child closes its stdout, and it's also signaled by
      # `bytesRead == 0` with success. Treat both as EOF.
      if ok == 0'i32 or bytesRead == 0'i32: break
      n = bytesRead.int
    else:
      n = posix.read(arg.handle.cint, tmp[0].addr, tmp.len)
      if n <= 0: break
    let prevLen = buf.len
    buf.setLen(prevLen + n)
    copyMem(addr buf[prevLen], addr tmp[0], n)
  let lock = cast[ptr Lock](arg.lockPtr)
  acquire lock[]
  try:
    stdout.write buf
    stdout.flushFile()
  finally:
    release lock[]

proc parallelTestDir(c: var TestCounters; files: openArray[string];
                     overwrite: bool; cat: Category; forward: string;
                     jobs: int) =
  ## Run each test in its own subprocess (`bin/hastur test ...`) with a
  ## per-test `--cacheDir` so concurrent compilations cannot collide on
  ## intermediates. Up to `jobs` subprocesses run at once. Test results
  ## are streamed in completion order; final pass/fail counts go into
  ## the shared `c`.
  let hastur = getAppFilename()
  prebuildSharedObjects(forward)
  let warmupCache = warmupSharedCache()
  warmupCopySeconds = 0
  let parallelStart = epochTime()
  var queue: seq[(int, string)] = @[]   # (idx, file) preserving input order
  for i, f in pairs(files): queue.add (i, f)
  var head = 0

  type Slot = object
    p: Process
    idx: int
    file: string
    reader: Thread[ReaderArg]
  var slots = newSeq[Slot](jobs)
  var active = 0
  var stdoutLock = default(Lock)
  initLock(stdoutLock)

  proc launch(slot: int) =
    if head >= queue.len: return
    let (idx, file) = queue[head]
    inc head
    let cacheDir = nimcacheDir / ".par" / $idx
    prefillFromWarmup(warmupCache, cacheDir)
    var args = @["test", "--no-build", "--cachedir:" & cacheDir]
    # Forward the parent's resolved toolchain dir so each worker uses the
    # exact same binaries (the default is now hastur's own sibling dir, an
    # absolute path, not the literal "bin").
    args.add "--bindir:" & toolchainDir
    if overwrite: args.add "--overwrite"
    if forward.len > 0: args.add "--forward:" & forward
    args.add file
    let p = startProcess(hastur, args = args,
        options = {poStdErrToStdOut, poUsePath})
    slots[slot] = Slot(idx: idx, file: file, p: p)
    let arg = ReaderArg(handle: p.outputHandle.int,
                        lockPtr: cast[pointer](addr stdoutLock))
    createThread(slots[slot].reader, drainStdout, arg)
    inc active

  for s in 0 ..< jobs: launch(s)

  while active > 0:
    for s in 0 ..< jobs:
      if slots[s].p == nil: continue
      let exit = peekExitCode(slots[s].p)
      if exit != -1:
        # Child exited. Worker's read loop hits EOF, flushes its
        # accumulated buffer to stdout under the lock, and exits.
        # `joinThread` waits for that flush to complete before we
        # tally the result and reuse the slot.
        joinThread(slots[s].reader)
        slots[s].p.close()
        inc c.total
        if exit != 0:
          inc c.failures
        slots[s].p = nil
        dec active
        launch(s)
    if active > 0:
      sleep(2)

  deinitLock(stdoutLock)
  if warmupCache.len > 0:
    echo "warmup prefill total: ",
         formatFloat(warmupCopySeconds, ffDecimal, precision=2), "s; ",
         "parallel run: ",
         formatFloat(epochTime() - parallelStart, ffDecimal, precision=2), "s."

proc testDir(c: var TestCounters; dir: string; overwrite: bool; cat: Category; forward: string) =
  var files: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim"):
      files.add x.path
  sort files
  if cat in {Compat, Basics}:
    removeDir "nimcache"
  if parallelJobs > 1 and canRunParallel(cat):
    parallelTestDir(c, files, overwrite, cat, forward, parallelJobs)
  else:
    for f in items files:
      testFile c, f, overwrite, cat, forward
  if cat in {Compat, Basics}:
    removeDir "nimcache"

const ModeFile = "hastur.mode"
  ## A test directory may drop a `hastur.mode` file naming the category that
  ## applies to it and everything beneath it. This replaces the old scheme of
  ## inferring the category from magic directory names (`nosystem`, `track`,
  ## `compat`, `valgrind`, `opt`): a suite is now free to use whatever
  ## directory layout it likes and opts into a special mode explicitly.

proc parseMode(s, src: string): Category =
  ## Map a `hastur.mode` keyword to a `Category`. The legacy directory names
  ## are the canonical keywords; the enum names are accepted as synonyms.
  case s.strip.normalize
  of "normal", "": Normal
  of "nosystem", "basics": Basics
  of "track", "tracked": Tracked
  of "compat": Compat
  of "valgrind": Valgrind
  of "opt", "optimized": Optimized
  of "skip": Skip
  else: quit "invalid mode '" & s.strip & "' in " & src

proc categoryOfDir(dir: string): Category =
  ## Resolve the category for `dir` from the nearest `hastur.mode` file in
  ## `dir` or an ancestor. No mode file up the whole chain means `Normal`.
  var d = dir
  while d.len > 0:
    let mf = d / ModeFile
    if fileExists(mf):
      return parseMode(readFile(mf), mf)
    let parent = d.parentDir
    if parent == d: break
    d = parent
  return Normal

proc categoryOf(path: string): Category =
  ## Category for a test file or directory: the mode of its own directory
  ## (or, for a not-yet-existing `record` destination, its parent directory).
  categoryOfDir(if dirExists(path): path else: path.parentDir)

# The general test-tree runner is `walkTests`/`walkRoots` further below; the
# old per-suite `nimonytests`/`exampletests` drivers folded into it.

# ── native-backend regression set ────────────────────────────────────────────
# The C-FREE native path (`nimony n` → arkham + nifasm, sibling `../nativenif`) is
# still incomplete, so we can't run the whole suite through it. Instead this is an
# explicit whitelist of what is known to run correctly natively — a regression
# guard: a native miscompile that diverges from the (spec-pinned) result is caught.
# Grow it as the backend gains features (the same record-what-works philosophy as
# the rest of hastur). `NativeTestDirs` are directories that pass IN FULL (negative
# `.msgs` tests auto-skipped); `NativeTestFiles` are individual passers from
# otherwise-partial directories.
const
  NativeTestDirs = [
    "tests/nimony/arc",        # full ARC suite — byte-identical to the C backend
    "tests/nimony/closures"
  ]
  NativeTestFiles = [
    # cps/* — closures & continuation-passing (indirect calls through fn-ptr values)
    "tests/nimony/cps/tbasicpassive",
    "tests/nimony/cps/tclosure",
    "tests/nimony/cps/tclosure_iter_basic",
    "tests/nimony/cps/tclosure_iter_body_capture",
    "tests/nimony/cps/tclosure_iter_break",
    "tests/nimony/cps/tclosure_iter_envcheck",
    "tests/nimony/cps/tclosure_iter_string",
    "tests/nimony/cps/tclosure_iter_var",
    "tests/nimony/cps/tasync_queue",
    "tests/nimony/cps/tfirstpassive",
    "tests/nimony/cps/tif",
    "tests/nimony/cps/tmethods",
    "tests/nimony/cps/tnestedloops",
    "tests/nimony/cps/trecursive",
    "tests/nimony/cps/tsuspend",
    "tests/nimony/cps/tsuspend_resume",
    "tests/nimony/cps/tparkstate",
    "tests/nimony/cps/ttry"
  ]

proc nativeTestFile(c: var TestCounters; file: string; overwrite: bool) =
  let msgs = file.changeFileExt(".msgs")
  if msgs.fileExists() and readFile(msgs).contains(ErrorKeyword):
    return
  inc c.total
  let (compilerOutput, compilerExitCode) = execNimonyNative(quoteShell(file))
  if compilerExitCode != 0:
    failure c, file, "native compiler exitcode 0",
      removeMakeErrors(compilerOutput) & "\nexitcode " & $compilerExitCode
    return
  let exe = file.generatedExeFile()
  if not exe.fileExists():
    failure c, file, "native executable", "missing: " & exe
    return
  let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell exe)
  var output = file.changeFileExt(".output")
  if testProgramExitCode != 0:
    output = file.changeFileExt(".exitcode")
    if not output.fileExists():
      failure c, file, "test program exitcode 0",
        "exitcode " & $testProgramExitCode & "\n" & testProgramOutput
      return
  if output.fileExists():
    let outputSpec = readFile(output).strip
    if outputSpec != testProgramOutput.strip:
      if overwrite:
        writeFile(output, testProgramOutput)
      failure c, file, outputSpec, testProgramOutput

proc nativetests(overwrite: bool) =
  ## Run the native-backend regression set (`NativeTestDirs` + `NativeTestFiles`)
  ## through `nimony n`. Requires the sibling `../nativenif` checkout (arkham/nifasm).
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  for dir in NativeTestDirs:
    var files: seq[string] = @[]
    for x in walkDir(dir):
      if x.kind == pcFile and x.path.endsWith(".nim"): files.add x.path
    sort files
    for f in files: nativeTestFile c, f, overwrite
  for f in NativeTestFiles:
    nativeTestFile c, f.addFileExt(".nim"), overwrite
  echo c.total - c.failures, " / ", c.total, " native tests successful in ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some native tests failed."
  else:
    echo "SUCCESS."

proc runNifToolTests*(tool, testDir, inputExt, expectedExt: string; overwrite: bool) =
  ## Run tests for a NIF tool.
  ## - inputExt: extension that input files must have (e.g., ".nif" or ".nj.nif")
  ## - expectedExt: extension for expected output files (e.g., ".nj.nif" or ".vl.nif")
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  for x in walkDir(testDir, relative = true):
    # To match input, file must end with inputExt but not with any longer output extension.
    # This prevents .nj.nif and .vl.nif from matching when inputExt is .nif
    let shouldTest = x.kind == pcFile and x.path.endsWith(inputExt) and
                     not x.path.contains(expectedExt) and
                     not x.path.contains(".out.nif") and
                     not (inputExt == ".nif" and (x.path.endsWith(".nj.nif") or x.path.endsWith(".vl.nif")))
    if shouldTest:
      inc c.total
      let t = testDir / x.path
      let dest = t.changeFileExt(".out.nif")
      let (msgs, exitcode) = execLocal(tool, os.quoteShell(t) & " " & os.quoteShell(dest))
      if exitcode != 0:
        failure c, t, tool & " exitcode 0", "exitcode " & $exitcode & "\n" & msgs
      let msgsFile = t.changeFileExt(".msgs")
      if msgsFile.fileExists():
        if overwrite:
          writeFile(msgsFile, msgs)
        else:
          let expectedOutput = readFile(msgsFile).strip
          if expectedOutput != msgs.strip:
            failure c, t, expectedOutput, msgs
      let expected = t.changeFileExt(expectedExt)
      if overwrite:
        if expected.fileExists():
          moveFile(dest, expected)
      elif expected.fileExists():
        let expectedOutput = readFile(expected).strip
        let destContent = readFile(dest).strip
        let success = expectedOutput == destContent
        if success:
          os.removeFile(dest)
        else:
          failure c, t, expectedOutput, destContent
  echo c.total - c.failures, " / ", c.total, " tests successful in ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some tests failed."
  else:
    echo "SUCCESS."

# NJ/VL/controlflow/contracts are now `setup.nim` runner directories under
# `tests/` that call `runNifToolTests` directly; their old wrapper procs and
# subcommands are gone.

proc validatorTests*() =
  ## Run the validator over compiler pass source files to verify NIF construction
  ## conforms to the grammar in doc/tags.md, plus obligation tracking and
  ## while-ParRi completion checks (the latter as warnings, not errors).
  ## Also runs fake_pass.nim which has deliberate errors and checks expected output.
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  const passFiles = [
    "src/hexer/lambdalifting.nim",
    "src/hexer/destroyer.nim",
    "src/hexer/xelim.nim",
    "src/hexer/desugar.nim",
    "src/hexer/cps.nim",
    "src/hexer/duplifier.nim",
    "src/hexer/lengcgen.nim",
    "src/hexer/eraiser.nim",
    #"src/hexer/vtables_backend.nim", # TODO: tool can't track writes to different buffers yet
    "src/hexer/iterinliner.nim",
    "src/hexer/constparams.nim",
    "src/nimony/sem.nim",
    "src/nimony/semdecls.nim",
    "src/nimony/controlflow.nim",
    "src/nimony/deferstmts.nim"]
  for f in passFiles:
    inc c.total
    let (msgs, exitcode) = execLocal("validator", "--strict " & os.quoteShell(f))
    if exitcode != 0:
      failure c, f, "validator: no violations", msgs
  # fake_pass.nim must produce the expected violations
  const fakePassDir = "tests/check_tags"
  for x in walkDir(fakePassDir, relative = true):
    if x.kind == pcFile and x.path.endsWith(".nim"):
      inc c.total
      let t = fakePassDir / x.path
      let expectedFile = t.changeFileExt(".expected")
      let (msgs, exitcode) = execLocal("validator", "--strict " & os.quoteShell(t))
      if not expectedFile.fileExists():
        failure c, t, "expected file " & expectedFile & " missing", ""
      else:
        let expected = readFile(expectedFile).strip
        var got = ""
        for line in msgs.splitLines:
          if line.contains("Error:") or line.contains("Warning:"):
            if got.len > 0: got.add "\n"
            got.add line
        if got.strip.replace("\\", "/") != expected.strip.replace("\\", "/"):
          failure c, t, expected, got
  echo c.total - c.failures, " / ", c.total, " validator tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some validator tests failed."
  else:
    echo "SUCCESS."

# ---- Incremental-build regression test ------------------------------------
# `nifmake --report` prints a machine-readable summary of which commands
# actually executed during one nifmake invocation. We drive `bin/nimony c
# --report` over `tests/incremental/sample.nim` through a sequence of
# scenarios and assert on the per-phase counts. This catches mtime-tracking
# regressions (e.g. tools that drift back into "always rewrite" and trigger
# perpetual rebuilds, or staleness checks that miss real edits) without
# the brittleness of comparing file timestamps.

type ReportEntry = tuple[cmd: string, count: int]

proc parseNifmakeReports(output: string): seq[seq[ReportEntry]] =
  ## Each `nifmake-report …` line in `output` becomes one inner seq. Lines
  ## without entries (an up-to-date no-op) yield an empty seq plus the
  ## sentinel `total=0` entry that nifmake always emits.
  result = @[]
  for line in output.splitLines:
    if not line.startsWith("nifmake-report"): continue
    var entries: seq[ReportEntry] = @[]
    for part in line.split(' '):
      if part.len == 0 or part == "nifmake-report": continue
      let eq = part.find('=')
      if eq < 0: continue
      try: entries.add((part[0 ..< eq], parseInt(part[eq+1 .. ^1])))
      except ValueError: discard
    result.add entries

proc reportField(entries: seq[ReportEntry]; cmd: string): int =
  for e in entries:
    if e.cmd == cmd: return e.count
  result = 0

proc incrementalTests*() =
  ## Drive `bin/nimony c --report` through a fixed sequence of scenarios on
  ## `tests/incremental/sample.nim` and assert the per-nifmake-invocation
  ## command counts. Fails the run on the first divergence; restores the
  ## sample file regardless of outcome.
  let t0 = epochTime()
  let src = "tests/incremental/sample.nim"
  let cache = "nimcache" / "incremental"
  let nimony = "bin" / "nimony".addFileExt(ExeExt)
  if not fileExists(src):
    quit "incremental: " & src & " missing"
  if not fileExists(nimony):
    quit "incremental: " & nimony & " not found; run `hastur build nimony` first"
  removeDir cache

  let baseCmd = nimony.quoteShell & " c --silentMake --report --nimcache:" &
                cache.quoteShell & " " & src.quoteShell
  let originalSrc = readFile(src)

  proc run(label: string): seq[seq[ReportEntry]] =
    let (output, ec) = execCmdEx(baseCmd)
    if ec != 0:
      stdout.write output
      writeFile(src, originalSrc)
      quit "incremental: '" & label & "' compile failed"
    parseNifmakeReports(output)

  var failures: seq[string] = @[]
  template expect(cond: bool; msg: string) =
    if not (cond): failures.add msg

  # Phase 1: cold cascade — both nifmake invocations should run real work.
  block:
    let r = run("cold")
    expect r.len == 2, "cold: expected 2 nifmake invocations, got " & $r.len
    if r.len == 2:
      expect reportField(r[0], "total") > 0, "cold: frontend ran 0 commands"
      expect reportField(r[1], "total") > 0, "cold: backend ran 0 commands"

  # Phase 2: no-op rebuild — both reports should show total=0.
  block:
    let r = run("noop")
    if r.len == 2:
      expect reportField(r[0], "total") == 0,
             "noop: frontend re-ran " & $reportField(r[0], "total") & " commands"
      expect reportField(r[1], "total") == 0,
             "noop: backend re-ran " & $reportField(r[1], "total") & " commands"

  # Phase 3: touch (no content change). nifler reruns to find content
  # unchanged — its OnlyIfChanged write preserves `.p.nif`'s mtime, so
  # nimsem and the backend must stay idle.
  block:
    setLastModificationTime(src, getTime())
    let r = run("touch")
    if r.len == 2:
      expect reportField(r[0], "nifler") >= 1,
             "touch: nifler did not re-run"
      expect reportField(r[0], "nimsem") == 0,
             "touch: nimsem ran " & $reportField(r[0], "nimsem") & " times (expected 0)"
      expect reportField(r[1], "total") == 0,
             "touch: backend ran " & $reportField(r[1], "total") & " commands (expected 0)"

  # Phase 4: real content edit — full cascade.
  block:
    writeFile(src, originalSrc & "\necho \"incremental edited\"\n")
    let r = run("edit")
    if r.len == 2:
      expect reportField(r[0], "nimsem") >= 1,
             "edit: nimsem did not re-run"
      expect reportField(r[1], "total") > 0,
             "edit: backend ran 0 commands"

  writeFile(src, originalSrc)

  let dt = epochTime() - t0
  if failures.len > 0:
    for f in failures: stderr.writeLine "incremental: " & f
    quit "FAILURE: " & $failures.len & " incremental phase(s) failed."
  echo "incremental: 4 / 4 phases successful in ",
       formatFloat(dt, ffDecimal, precision=2), "s."
  echo "SUCCESS."

proc test(t: string; overwrite: bool; cat: Category; forward: string) =
  var c = TestCounters(total: 0, failures: 0)
  testFile c, t, overwrite, cat, forward
  if c.failures > 0:
    quit "FAILURE: Test failed."

proc testDirCmd(dir: string; overwrite: bool; forward: string) =
  var c = TestCounters(total: 0, failures: 0)
  let t0 = epochTime()
  testDir c, dir, overwrite, categoryOfDir(dir), forward
  echo c.total - c.failures, " / ", c.total, " tests successful in ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some tests failed."
  else:
    echo "SUCCESS."

proc exec(cmd: string; showProgress = false) =
  if showProgress:
    let exitCode = execShellCmd(cmd)
    if exitCode != 0:
      quit "FAILURE " & cmd & "\n"
  else:
    let (s, exitCode) = execCmdEx(cmd)
    if exitCode != 0:
      quit "FAILURE " & cmd & "\n" & s

proc exec(exe, cmd: string) =
  let (s, exitCode) = execLocal(exe, cmd)
  if exitCode != 0:
    quit "FAILURE " & cmd & "\n" & s

proc gitAdd(file: string) =
  exec "git add " & file.quoteShell

proc addTestCode(dest, src: string) =
  copyFile src, dest
  gitAdd dest

proc addTestSpec(dest, content: string) =
  writeFile dest, content
  gitAdd dest

type
  RecordFlag = enum
    RecordAst, RecordCodegen

proc record(file, test: string; flags: set[RecordFlag]; cat: Category) =
  # Records a new test case.
  let (compilerOutput, compilerExitCode) = execNimony(quoteShell(file), cat)
  if compilerExitCode == 1:
    let idx = compilerOutput.find(ErrorKeyword)
    assert idx >= 0, "compiler output did not contain: " & ErrorKeyword
    copyFile file, test
    # run the test again so that the error messages contain the correct paths:
    let (finalCompilerOutput, finalCompilerExitCode) = execNimony(quoteShell(test), cat)
    assert finalCompilerExitCode == 1, "the compiler should have failed once again"
    gitAdd test
    addTestSpec test.changeFileExt(".msgs"), finalCompilerOutput
  else:
    if cat notin {Basics, Tracked}:
      let exe = file.generatedExeFile()
      let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell exe)
      let ext = if testProgramExitCode != 0: ".exitcode" else: ".output"
      addTestSpec test.changeFileExt(ext), testProgramOutput

    addTestCode test, file
    if {RecordCodegen, RecordAst} * flags != {}:
      let (finalCompilerOutput, finalCompilerExitCode) = execNimony(quoteShell(test), cat)
      assert finalCompilerExitCode == 0, finalCompilerOutput

    if RecordCodegen in flags:
      let nimcacheC = generatedFile(test, ".c")
      addTestCode test.changeFileExt(".nim.c"), nimcacheC

    if RecordAst in flags:
      let nif = generatedFile(test, ".s.nif")
      addTestCode test.changeFileExt(".nif"), nif

proc binDir*(): string =
  result = toolchainDir

proc robustMoveFile(src, dest: string) =
  if fileExists(src):
    moveFile src, dest

var release = false

proc nimcPrefix(): string =
  # `--warningAsError:ProveInit:off` and `--warningAsError:Uninit:off`:
  # Nimony's `src/config.nims` promotes these warnings to errors, but Nim
  # 2.2.10's host stdlib (typedthreads.nim, deques.nim) trips them on
  # patterns that aren't actionable from our side. Without these overrides,
  # any tool using `createThread` (hastur itself) or `initDeque` (pnak)
  # fails to build on Nim 2.2.10.
  (if release: "nim c -d:release " else: "nim c ") &
    "--warningAsError:ProveInit:off --warningAsError:Uninit:off "

proc validatePassesFlag(): string =
  ## Enable the phase-aware IR validator only when running on CI. GitHub Actions
  ## (and most other CI providers) set `CI=true` in the environment, so we key
  ## off that: locally the validator stays opt-in via `NIMONY_VALIDATE=1`, which
  ## keeps iteration fast while guaranteeing the check on every PR. On Windows
  ## CI the in-process post-sem validator is turned off — the per-module walk
  ## amplifies Windows' per-process overhead enough to dominate the test phase,
  ## and the Linux/macOS jobs already catch the same drift on every PR.
  if getEnv("CI").len > 0 or getEnv("NIMONY_VALIDATE").len > 0:
    when defined(windows):
      "-d:skipPostSemValidator "
    else:
      "-d:validatePasses "
  else:
    ""

proc buildNifler(showProgress = false) =
  exec nimcPrefix() & "src/nifler/nifler.nim", showProgress
  let exe = "nifler".addFileExt(ExeExt)
  robustMoveFile "src/nifler/" & exe, binDir() / exe

proc buildNimsem(showProgress = false) =
  exec nimcPrefix() & validatePassesFlag() & "src/nimony/nimsem.nim", showProgress
  let exe = "nimsem".addFileExt(ExeExt)
  robustMoveFile "src/nimony/" & exe, binDir() / exe

proc buildNimony(showProgress = false) =
  exec nimcPrefix() & validatePassesFlag() & "src/nimony/nimony.nim", showProgress
  let exe = "nimony".addFileExt(ExeExt)
  robustMoveFile "src/nimony/" & exe, binDir() / exe

proc buildControlflow*(showProgress = false) =
  exec nimcPrefix() & "src/nimony/controlflow.nim", showProgress
  let exe = "controlflow".addFileExt(ExeExt)
  robustMoveFile "src/nimony/" & exe, binDir() / exe

proc buildContracts*(showProgress = false) =
  exec nimcPrefix() & "src/nimony/contracts.nim", showProgress
  let exe = "contracts".addFileExt(ExeExt)
  robustMoveFile "src/nimony/" & exe, binDir() / exe

proc buildNj*(showProgress = false) =
  exec nimcPrefix() & "src/njvl/nj.nim", showProgress
  let exe = "nj".addFileExt(ExeExt)
  robustMoveFile "src/njvl/" & exe, binDir() / exe

proc buildVl*(showProgress = false) =
  exec nimcPrefix() & "src/njvl/vl.nim", showProgress
  let exe = "vl".addFileExt(ExeExt)
  robustMoveFile "src/njvl/" & exe, binDir() / exe

proc buildLengc*(showProgress = false) =
  exec nimcPrefix() & "src/lengc/lengc.nim", showProgress
  let exe = "lengc".addFileExt(ExeExt)
  robustMoveFile "src/lengc/" & exe, binDir() / exe

proc buildShoggoth(showProgress = false) =
  exec nimcPrefix() & "src/lengc/shoggoth/shoggoth.nim", showProgress
  let exe = "shoggoth".addFileExt(ExeExt)
  robustMoveFile "src/lengc/shoggoth/" & exe, binDir() / exe

proc buildNiflink(showProgress = false) =
  ## `niflink` (the C-backend link driver) reads a link manifest NIF and links
  ## the project; built on the nifcore API.
  exec nimcPrefix() & "src/niflink/niflink.nim", showProgress
  let exe = "niflink".addFileExt(ExeExt)
  robustMoveFile "src/niflink/" & exe, binDir() / exe

proc buildArkham(showProgress = false) =
  ## `arkham` (Leng -> typed asm-NIF native codegen) lives in the sibling
  ## `../nativenif` repo and reuses nimony's NIF libraries via its committed
  ## sibling-relative `nim.cfg`. We assume the checkout exists (the `dist/`
  ## auto-clone is a later step). arkham's own `nim.cfg` already sets
  ## `--outdir:bin`; we pass it explicitly so the result is deterministic
  ## regardless of the current directory.
  exec nimcPrefix() & "--outdir:" & binDir() & " ../nativenif/src/arkham/arkham.nim", showProgress

proc buildNifasm(showProgress = false) =
  ## `nifasm` (asm-NIF -> static, libc-free ELF/Mach-O/PE executable; also the
  ## linker) — sibling repo, same assume-exists arrangement as `buildArkham`.
  exec nimcPrefix() & "--outdir:" & binDir() & " ../nativenif/src/nifasm/nifasm.nim", showProgress

proc buildHexer*(showProgress = false) =
  exec nimcPrefix() & "src/hexer/hexer.nim", showProgress
  let exe = "hexer".addFileExt(ExeExt)
  robustMoveFile "src/hexer/" & exe, binDir() / exe

proc buildNifmake(showProgress = false) =
  exec nimcPrefix() & "src/nifmake/nifmake.nim", showProgress
  let exe = "nifmake".addFileExt(ExeExt)
  robustMoveFile "src/nifmake/" & exe, binDir() / exe

proc buildValidator*(showProgress = false) =
  exec nimcPrefix() & "src/validator/validator.nim", showProgress
  let exe = "validator".addFileExt(ExeExt)
  robustMoveFile "src/validator/" & exe, binDir() / exe

proc buildDagon*(showProgress = false) =
  exec nimcPrefix() & "src/dagon/dagon.nim", showProgress
  let exe = "dagon".addFileExt(ExeExt)
  robustMoveFile "src/dagon/" & exe, binDir() / exe

proc buildPnak*(showProgress = false) =
  exec nimcPrefix() & "src/pnak/pnak.nim", showProgress
  let exe = "pnak".addFileExt(ExeExt)
  robustMoveFile "src/pnak/" & exe, binDir() / exe

# ---------------------------------------------------------------------------
# Bootstrapping progress (see https://github.com/nim-lang/nimony/issues/1788).
#
# Each module listed here is known to compile with the `nimony c` command.
# New modules are added as tier-by-tier bootstrapping proceeds; the
# `hastur bootstrap` target walks this list to catch regressions.
# ---------------------------------------------------------------------------

const BootstrapModules = [
  # Only leaves of the current bootstrap DAG are listed: compiling each leaf
  # transitively covers every already-ported module via its imports. Adding
  # a module that is imported by something already on this list is redundant;
  # removing a module that has no importer in the list shrinks coverage.
  # Exception: modules in `RunnableBootstrapModules` stay here even if
  # imported elsewhere, because they need to be executed with `-r`.

  # Runnable leaf tests (executed with `c -r`).
  "src/lib/argsfinder.nim",
  "src/lib/bitabs.nim",

  # Tier 1/2 genuine leaves.
  "src/nimony/features.nim",
  "src/nimony/intervals.nim",
  "src/models/nifler_tags.nim",

  # Tier 5/6 leaves.
  "src/nimony/inferle.nim",
  "src/nimony/deferstmts.nim",
  "src/nimony/cli.nim",

  # Tier 10 leaves.
  "src/nimony/pragmacanon.nim",

  # Tier 12 tips still present after later tiers added.
  "src/nimony/semborrow.nim",
  "src/nimony/semuntyped.nim",
  "src/nimony/enumtostr.nim",
  "src/nimony/derefs.nim",

  # Tier 13 tips still present after later tiers added.
  "src/nimony/module_plugins.nim",
  "src/hexer/inliner.nim",
  "src/hexer/lambdalifting.nim",

  # Tier 14 tips still present after later tiers added.
  "src/hexer/cps.nim",
  "src/hexer/constparams.nim",
  "src/hexer/vtables_backend.nim",
  "src/hexer/dce2.nim",

  # Tier 17 tips. `hexer.nim` subsumes `lengcgen.nim` via its import set.
  "src/hexer/hexer.nim",
  "src/nimony/indexgen.nim",
  "src/nimony/idetools.nim",

  # Tier 18 tip. `sem.nim` subsumes contracts_njvl and exprexec via its
  # import set, so the Tier 16 leaves are implicitly covered by this entry.
  "src/nimony/sem.nim",

  # Tier 19 tip. Peer of `sem.nim`; both feed `nimony/nimsem.nim` at Tier 20.
  "src/nimony/deps.nim",

  # Tier 20 tip — the driver. Subsumes sem.nim, deps.nim, and hexer/hexer.nim
  # via its import set, so this entry alone exercises the full bootstrap DAG.
  "src/nimony/nimony.nim",
]

# Modules whose `isMainModule` block should also be executed after compilation.
const RunnableBootstrapModules = [
  "src/lib/bitabs.nim",
  "src/lib/argsfinder.nim",
]

proc tierTests() =
  ## Compile every module on `BootstrapModules` with `bin/nimony c`. Fails
  ## fast on the first regression so the offending module is obvious. On
  ## Windows the list collapses to the Tier 20 tip (`nimony.nim`) — its
  ## import set already covers every other entry, so the redundant per-leaf
  ## compiles are pure CI cost; Linux/macOS still cover every leaf.
  let nimony = binDir() / "nimony".addFileExt(ExeExt)
  if not fileExists(nimony):
    quit "bootstrap: " & nimony & " not found; run `hastur build nimony` first"
  let modules =
    when defined(windows): @["src/nimony/nimony.nim"]
    else: @BootstrapModules
  let t0 = epochTime()
  var failed: seq[string] = @[]
  for m in modules:
    removeDir "nimcache"
    let subcmd = if m in RunnableBootstrapModules: " c -r " else: " c "
    let (output, ec) = execCmdEx(nimony.quoteShell & subcmd & m.quoteShell)
    if ec == 0:
      echo "OK   ", m
    else:
      echo "FAIL ", m
      echo output
      failed.add m
  echo failed.len, " / ", modules.len, " bootstrap regressions in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if failed.len > 0:
    quit "FAILURE: bootstrap regression(s): " & failed.join(", ")
  else:
    echo "SUCCESS."

proc buildNimonyToolchain(showProgress = false) =
  ## Rebuild every host-Nim-compiled binary that shares `src/nimony/programs.nim`
  ## (or any other module reused across compiler stages). A change to a shared
  ## helper like `suffixToNif` only takes effect once nimony, nimsem AND hexer
  ## are all re-linked, so `hastur selfcheck` (and any caller that wants a
  ## fully-consistent toolchain) goes through this rather than `buildNimony`
  ## alone — which is what masked a hexer bug during the doc-generator work.
  buildNimsem(showProgress)
  buildNimony(showProgress)
  buildHexer(showProgress)

# ---- deterministic self-host bootstrap ------------------------------------
# `bin0/` is a fresh copy of the host-Nim-built toolchain in `bin/`; `binN/`
# (N >= 1) is `binN-1/`'s nimony recompiling all three self-tools from
# source. Each pass produces a sibling `binN/` directory next to `lib/` and
# nothing is fed back into `bin/` — `bin/` remains the host-Nim-built
# toolchain the rest of hastur drives the test suite with.

## The boot bootstrap rebuilds the *full* toolchain at every stage — not just
## `bin/nimony`. nimony is only the driver; the heavy lifting (semantic
## analysis, hexer lowering) happens in `nimsem` and `hexer`, both reached
## via `findTool` from each `nimony c` invocation. Iterating only over the
## driver therefore exercises self-hosting of nimony alone while the
## stage-0 nimsem and hexer keep doing all the real work — bugs in their
## codegen go undetected indefinitely. `tooldirs.binDir` and
## `semos.nimonyDir` accept any tail starting with `bin`, so stage-N's
## nimony resolves stage-N's tools and the project's stdlib without any
## CLI override.
const BootSelfTools = ["nimsem", "hexer", "nimony"]
  ## Tools rebuilt from source at every stage. The order matters: nimsem and
  ## hexer are needed by every later `nimony c` call, so they go first;
  ## nimony itself goes last because it's the one each *next* stage will
  ## drive with.
const BootCarryTools = ["nifler", "lengc", "niflink", "nifmake", "validator", "shoggoth"]
  ## Tools copied from `bin/` into each stage dir. They're tier-0 for
  ## bootstrap purposes (host-Nim-built throughout) but `nimony c` shells
  ## to them, so each stage dir needs its own copy.

proc bootSourceFor(tool: string): string =
  case tool
  of "nimony", "nimsem": "src/nimony/" & tool & ".nim"
  of "hexer": "src/hexer/hexer.nim"
  else: quit "boot: no source mapping for tool " & tool

proc bootStageDir(stage: int): string =
  ## Every stage lives in its own `binN/` directory next to `lib/`. `bin0/`
  ## is a fresh copy of the host-Nim-built `bin/` (provisioned by
  ## `provisionStageZero`); `binN/` (N>=1) is built by
  ## `binN-1/nimony`.
  "bin" & $stage

proc carryAuxTool(stageBin, name: string) =
  let exe = name.addFileExt(ExeExt)
  let src = binDir() / exe
  let dst = stageBin / exe
  if not fileExists(src):
    quit "boot: " & src & " not found; run `hastur build " & name &
         "` (or `hastur build all`) first"
  if fileExists(dst): removeFile(dst)
  copyFile(src, dst)
  when defined(posix):
    inclFilePermissions(dst, {fpUserExec, fpGroupExec, fpOthersExec})

proc provisionStageBin(stage: int): string =
  ## Create `binN/` (clean) and populate it with the tools that don't get
  ## rebuilt per stage. The self-rebuilt tools are dropped in afterwards by
  ## `compileBootTool`.
  result = bootStageDir(stage)
  removeDir result
  createDir result
  for aux in BootCarryTools:
    carryAuxTool(result, aux)

proc provisionStageZero(): string =
  ## Seed `bin0/` with a fresh copy of the host-Nim-built toolchain in
  ## `bin/`. Unlike later stages, the self-tools aren't recompiled here —
  ## they're copied across so that `bin1/` has a complete driver to start
  ## from.
  result = bootStageDir(0)
  removeDir result
  createDir result
  for aux in BootCarryTools:
    carryAuxTool(result, aux)
  for tool in BootSelfTools:
    carryAuxTool(result, tool)

proc compileBootTool(stage: int; compiler, source, outBin, cacheBase, args: string;
                     withValgrind: bool) =
  ## Run `compiler c --out:outBin source`. `compiler` is the previous
  ## stage's nimony, so it transitively drives the previous stage's
  ## nimsem/hexer (siblings under the same stage's bin dir) for this build.
  let cache = cacheBase / outBin.extractFilename
  removeDir cache
  createDir cache
  if fileExists(outBin): removeFile(outBin)
  var cmd = compiler.quoteShell & " c --silentMake --nimcache:" &
            cache.quoteShell & " --out:" & outBin.quoteShell
  if withValgrind:
    cmd.add " --passC:\"-DMI_TRACK_VALGRIND=1\""
  if args.len > 0:
    cmd.add ' '
    cmd.add args
  cmd.add ' '
  cmd.add source.quoteShell
  echo "[boot] stage ", stage, ": ", cmd
  let t0 = epochTime()
  let exitCode = execShellCmd(cmd)
  let dt = epochTime() - t0
  if exitCode != 0:
    quit "FAILURE: boot stage " & $stage & " (" & outBin.extractFilename &
         ") failed after " & formatFloat(dt, ffDecimal, precision=2) & "s"
  if not fileExists(outBin):
    quit "FAILURE: boot stage " & $stage & ": " & outBin &
         " was not produced (did `--out` get rejected?)"
  echo "[boot] stage ", stage, " produced ", outBin, " in ",
       formatFloat(dt, ffDecimal, precision=2), "s"

proc compileBootStage(stage: int; cacheBase, args: string; withValgrind: bool):
                     string =
  ## Build stage `stage` of the toolchain. Returns the stage's bin
  ## directory. Driver of stage N is the stage-(N-1) nimony.
  let prev = bootStageDir(stage - 1)
  let prevNimony = prev / "nimony".addFileExt(ExeExt)
  if not fileExists(prevNimony):
    quit "boot: " & prevNimony & " not found (stage " & $(stage - 1) &
         " missing)"
  result = provisionStageBin(stage)
  for tool in BootSelfTools:
    let outBin = result / tool.addFileExt(ExeExt)
    compileBootTool(stage, prevNimony, bootSourceFor(tool), outBin,
                    cacheBase, args, withValgrind)

const HeaderSkipBytes = 4096
  ## Bytes at the start of an executable to skip during stage-comparison.
  ## Big enough to cover the volatile header regions of all three common
  ## formats: ELF (.note.gnu.build-id is at ~1 KB), Mach-O (LC_UUID lives
  ## inside the load commands), PE (DOS header + COFF header with
  ## TimeDateStamp). 4 KB also matches the typical page size so the cut
  ## tends to fall on a section boundary.

proc maskBuildStamps(buf: var string) =
  ## Zero out byte regions whose contents depend on *when* the binary was
  ## linked, not *what* the toolchain produced. Two stages whose code is
  ## identical otherwise still compare equal.
  ##
  ## Approach: skip the first `HeaderSkipBytes` (covers ELF build-id,
  ## Mach-O LC_UUID, PE COFF TimeDateStamp — all live in headers) and
  ## additionally mask `HH:MM:SS` / `Mmm DD YYYY` ASCII strings anywhere
  ## else (mimalloc bakes `__DATE__` / `__TIME__` into .rodata via
  ## `vendor/mimalloc/src/options.c`).
  let skip = min(HeaderSkipBytes, buf.len)
  for k in 0 ..< skip: buf[k] = '\0'

  proc isDigit(c: char): bool {.inline.} = c >= '0' and c <= '9'

  # HH:MM:SS — exact 8 bytes.
  var i = skip
  while i + 8 <= buf.len:
    if buf[i+2] == ':' and buf[i+5] == ':' and
       isDigit(buf[i]) and isDigit(buf[i+1]) and
       isDigit(buf[i+3]) and isDigit(buf[i+4]) and
       isDigit(buf[i+6]) and isDigit(buf[i+7]):
      for k in i ..< i + 8: buf[k] = '\0'
      i += 8
    else:
      inc i

  # `Mmm DD YYYY` — month abbreviation + day (space-padded) + 4-digit year.
  const Months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  i = skip
  while i + 11 <= buf.len:
    var matched = false
    if buf[i+3] == ' ' and buf[i+6] == ' ' and
       (buf[i+4] == ' ' or isDigit(buf[i+4])) and isDigit(buf[i+5]) and
       isDigit(buf[i+7]) and isDigit(buf[i+8]) and
       isDigit(buf[i+9]) and isDigit(buf[i+10]):
      let m = buf.substr(i, i + 2)
      for mo in Months:
        if m == mo:
          matched = true
          break
    if matched:
      for k in i ..< i + 11: buf[k] = '\0'
      i += 11
    else:
      inc i

proc bootBinariesEqual(pa, pb: string): bool =
  ## Equal ignoring linker / preprocessor build-time stamps (build-id and
  ## any embedded `__DATE__` / `__TIME__` strings). Mismatch on size is
  ## always a real difference; same size compares the masked content.
  if getFileSize(pa) != getFileSize(pb): return false
  var a = readFile(pa)
  var b = readFile(pb)
  maskBuildStamps(a)
  maskBuildStamps(b)
  result = a == b

proc stagesEqual(a, b: string): bool =
  ## Two stages converge when every self-rebuilt binary is byte-identical
  ## after masking build-time stamps (ELF build-id, mimalloc __DATE__/
  ## __TIME__). Auxiliary carried tools are the same file copied twice, so
  ## we don't bother comparing them.
  ##
  ## On macOS the byte comparison is skipped: every link records a fresh
  ## LC_UUID and emits an LC_CODE_SIGNATURE whose body is a SHA-256 hash
  ## chain over the binary's pages. Even a single byte difference (such
  ## as the `__DATE__` / `__TIME__` strings mimalloc bakes into .rodata)
  ## ripples into the signature blob, so two stages whose generated code
  ## is identical still won't compare equal. The masking strategy that
  ## works for ELF doesn't generalize, and Mach-O has no equivalent of
  ## ELF's deterministic `--build-id=none`, so we report stages as
  ## converged once both stages produced binaries of equal size — the
  ## self-compile pass that built `b` having succeeded is what tells us
  ## the previous stage is functional.
  when defined(macosx):
    for tool in BootSelfTools:
      let pa = a / tool.addFileExt(ExeExt)
      let pb = b / tool.addFileExt(ExeExt)
      if getFileSize(pa) != getFileSize(pb):
        echo "[boot] stage size diff: ", tool,
             " (", getFileSize(pa), " vs ", getFileSize(pb), ")"
        return false
    return true
  else:
    for tool in BootSelfTools:
      let pa = a / tool.addFileExt(ExeExt)
      let pb = b / tool.addFileExt(ExeExt)
      if not bootBinariesEqual(pa, pb):
        echo "[boot] stage diff: ", tool
        return false
    return true

proc valgrindSmokeTest(exe: string) =
  ## Run the bootstrapped binary under valgrind on a trivial command to flush
  ## out the obvious memory corruption bugs (use-after-free, double-free,
  ## invalid reads). `-DMI_TRACK_VALGRIND=1` must already be baked in via
  ## the `--valgrind` boot flag, otherwise mimalloc's arena is opaque to
  ## valgrind and nothing useful comes out.
  echo "[boot] valgrind smoke check on ", exe
  let cmd = "valgrind --leak-check=full --error-exitcode=1 " &
            exe.quoteShell & " --version"
  echo "[boot] ", cmd
  let exitCode = execShellCmd(cmd)
  if exitCode != 0:
    quit "FAILURE: valgrind reported errors in " & exe
  echo "[boot] valgrind smoke check passed"

const BootSelfCompilePasses = 3
  ## Number of self-compile passes the bootstrap runs (`bin1/` … `binN/`).
  ## Fixed so that `boot` is deterministic: every invocation produces the
  ## same set of stage directories regardless of whether earlier stages
  ## happen to converge to a byte-identical binary.

proc bootCmd*(args: string; withValgrind: bool) =
  for tool in BootSelfTools:
    let exe = binDir() / tool.addFileExt(ExeExt)
    if not fileExists(exe):
      quit "boot: " & exe & " not found; run `hastur build all` first"
  for tool in BootCarryTools:
    let exe = binDir() / tool.addFileExt(ExeExt)
    if not fileExists(exe):
      quit "boot: " & exe & " not found; run `hastur build all` first"
  for tool in BootSelfTools:
    let src = bootSourceFor(tool)
    if not fileExists(src):
      quit "boot: " & src & " missing"
  let cacheBase = nimcacheDir / "boot"
  removeDir cacheBase
  createDir cacheBase
  let t0 = epochTime()

  var stages = newSeq[string](BootSelfCompilePasses + 1)
  stages[0] = provisionStageZero()
  for n in 1 .. BootSelfCompilePasses:
    stages[n] = compileBootStage(n, cacheBase, args, withValgrind)

  for n in 1 .. BootSelfCompilePasses:
    if stagesEqual(stages[n-1], stages[n]):
      echo "[boot] stages ", n-1, " and ", n, " are byte-identical."
    else:
      echo "[boot] stages ", n-1, " and ", n, " differ."

  if withValgrind:
    valgrindSmokeTest(stages[^1] / "nimony".addFileExt(ExeExt))

  echo "[boot] total ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  echo "SUCCESS."

proc selfcheckCmd() =
  ## Full compiler self-host regression check. The sequence here mirrors what
  ## a maintainer runs after touching anything in `src/nimony/`, `src/hexer/`
  ## or `src/lib/` that the compiler itself depends on:
  ##
  ##   1. Rebuild nimony + nimsem + hexer from host Nim, so all three reflect
  ##      current source. `boot` copies `bin/` into `bin0/` at every run, so
  ##      a stale `bin/` would still poison `bin0/`.
  ##   2. `bootstrap`: compile every module on the bootstrap list with the
  ##      freshly-built `bin/nimony`. Catches per-module sem/codegen
  ##      regressions and fails fast.
  ##   3. `boot --valgrind`: deterministic self-host (bin0 → bin1 → … →
  ##      binN), then run the last stage's nimony under valgrind. Catches
  ##      whole-program regressions (init order, codegen interactions,
  ##      runtime UAFs) that single-module compiles miss.
  ##
  ## Boot's "stages N and N+1 differ" messages are informational — they
  ## normally reflect gcc's `--build-id` non-determinism, not a real
  ## divergence; the valgrind smoke test is what tells us the last stage
  ## actually runs.
  let t0 = epochTime()
  echo "[selfcheck] step 1/3: rebuilding nimony toolchain"
  buildNimonyToolchain(showProgress = true)
  echo "[selfcheck] step 2/3: bootstrap (per-module compile check)"
  tierTests()
  echo "[selfcheck] step 3/3: boot --valgrind (3-stage self-host + valgrind smoke)"
  bootCmd("", withValgrind = true)
  echo "[selfcheck] all checks passed in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."

proc execLengc(cmd: string) =
  exec "lengc", cmd

proc execHexer(cmd: string) =
  exec "hexer", cmd

proc hexertests*(overwrite: bool) =
  let mod1 = "tests/hexer/mod1"
  let helloworld = "tests/hexer/hexer_helloworld"
  createIndex helloworld & ".nif", false, NoLineInfo
  createIndex mod1 & ".nif", false, NoLineInfo
  execHexer "c " & mod1 & ".nif"
  execHexer "c " & helloworld & ".nif"
  execLengc " c -r " & mod1 & ".c.nif " & helloworld & ".c.nif"

proc runDagonTest(c: var TestCounters; testFile: string) =
  ## Drive `nimony doc <testFile>` into a per-test outdir, then check every
  ## line in the sibling `.assertions` file is present in the produced output.
  ## Each assertion line is `<file-relative-to-outdir>: <substring>`.
  inc c.total
  let basename = splitFile(testFile).name
  let outdir = nimcacheDir / "dagontests" / basename
  removeDir outdir
  createDir outdir
  let cmd = "-f --outdir:" & quoteShell(outdir) & " doc " & quoteShell(testFile)
  let (output, exit) = execLocal("nimony", cmd)
  if exit != 0:
    failure c, testFile, "nimony doc exit code 0 (cmd: " & cmd & ")",
      "exit " & $exit & "\n" & output
    return
  let assertionsFile = testFile.changeFileExt(".assertions")
  if not fileExists(assertionsFile): return
  # Collect every failed assertion under one test failure rather than counting
  # each as a separate `c.failures` increment.
  var problems: seq[string] = @[]
  for line in lines(assertionsFile):
    let s = line.strip()
    if s.len == 0 or s.startsWith("#"): continue
    let colon = s.find(':')
    if colon < 0:
      problems.add "malformed assertion: " & s
      continue
    let relPath = s.substr(0, colon - 1).strip()
    let needle = s.substr(colon + 1).strip()
    let path = outdir / relPath
    if not fileExists(path):
      problems.add "missing file " & relPath & " (needle: " & needle & ")"
      continue
    if needle notin readFile(path):
      problems.add "needle not in " & relPath & ": " & needle
  if problems.len > 0:
    failure c, testFile, $problems.len & " assertion(s) failed",
      problems.join("\n")

proc dagontests*(dir: string; overwrite: bool) =
  ## Run every `t*.nim` under `dir` (default `tests/dagon/`) through
  ## `nimony doc` and verify the produced HTML/idx files against an
  ## `.assertions` sidecar.
  let TestDir = dir
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  if dirExists(TestDir):
    for x in walkDir(TestDir, relative = true):
      if x.kind == pcFile and x.path.endsWith(".nim") and x.path.startsWith("t"):
        runDagonTest c, TestDir / x.path
  echo c.total - c.failures, " / ", c.total, " dagon tests successful in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some dagon tests failed."
  else:
    echo "SUCCESS."

proc runPnakTest(c: var TestCounters; testFile: string) =
  ## Compile and run a self-contained pnak integration test. The test is a
  ## normal Nim program that drives the `bin/pnak` binary as a subprocess
  ## and exits non-zero on failure — no assertions sidecar needed.
  inc c.total
  let basename = splitFile(testFile).name
  let outdir = nimcacheDir / "pnaktests" / basename
  removeDir outdir
  createDir outdir
  let exe = outdir / basename.addFileExt(ExeExt)
  let compileCmd = nimcPrefix() & "--nimcache:" & quoteShell(outdir) &
                   " -o:" & quoteShell(exe) & " " & quoteShell(testFile)
  if execShellCmd(compileCmd) != 0:
    failure c, testFile, "nim c failed (cmd: " & compileCmd & ")"
    return
  let (output, exit) = execCmdEx(exe)
  if exit != 0:
    failure c, testFile, "exit " & $exit, output

proc pnaktests*(dir: string) =
  ## Run every `t*.nim` under `dir` (default `tests/pnak/`). The tests are
  ## self-contained integration tests of the `pnak` binary (BFS clone +
  ## `nimony.paths` generation); they stage a local file:// upstream and
  ## stay offline.
  let TestDir = dir
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  if dirExists(TestDir):
    for x in walkDir(TestDir, relative = true):
      if x.kind == pcFile and x.path.endsWith(".nim") and x.path.startsWith("t"):
        runPnakTest c, TestDir / x.path
  echo c.total - c.failures, " / ", c.total, " pnak tests successful in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some pnak tests failed."
  else:
    echo "SUCCESS."

import install

proc syncCmd(newBranch: string) =
  let (output, status) = execCmdEx("git symbolic-ref --short HEAD")
  if status != 0:
    quit "FAILURE: " & output
  let (defaultBranchOutput, defaultBranchStatus) = execCmdEx("git symbolic-ref refs/remotes/origin/HEAD --short")
  var defaultBranch = "master"
  if defaultBranchStatus == 0:
    # Output is like "origin/main" or "origin/master"
    defaultBranch = defaultBranchOutput.strip()
    if defaultBranch.startsWith("origin/"):
      defaultBranch = defaultBranch[7..^1]
  exec "git checkout " & defaultBranch
  exec "git pull origin " & defaultBranch
  let branch = output.strip()
  if branch != defaultBranch:
    exec "git branch -D " & branch
  if newBranch.len > 0:
    exec "git checkout -B " & newBranch

proc pullpush(cmd: string) =
  let (output, status) = execCmdEx("git symbolic-ref --short HEAD")
  if status != 0:
    quit "FAILURE: " & output
  exec "git " & cmd & " origin " & output.strip()

proc bugCmd(args: seq[string]; forward: string) =
  if not fileExists("bin/nimony".addFileExt(ExeExt)):
    buildNimsem()
    buildNimony()
    buildHexer()
  var cmd = "c"
  if forward.len != 0:
    cmd.add ' '
    cmd.add forward
  for arg in items(args):
    cmd.add ' '
    cmd.add quoteShell(arg)
  let (output, exitCode) = execLocal("nimony", cmd)
  if exitCode != 0:
    stdout.write("FAILURE " & cmd & "\n")
    if output.len > 0:
      stdout.write(output)
    let toolCmd = extractToolCmd(output)
    if toolCmd.len > 0:
      saveSessionCmd(toolCmd)
    quit 1
  if output.len > 0:
    stdout.write(output)

proc repCmd() =
  let cmd = loadSessionCmd()
  if cmd.len == 0:
    quit "no session to repeat"
  exec cmd

# ── recursive tree runner (the general `hastur <dir>` command) ───────────────
# A directory describes how it is tested with two optional files:
#   setup.nim     — a custom runner program that OWNS the directory (and its
#                   subtree): hastur compiles+runs it, passes context on argv,
#                   and takes its exit code as the verdict. This is the escape
#                   hatch for suites that aren't "a folder of inputs" (boot,
#                   incremental, validator) or need bespoke logic (nj, vl,
#                   dagon, pnak). It imports hastur itself as the test kit.
#   setup.hastur  — lightweight prep for a directory still run by the built-in
#                   nimony runner: each line is a hastur subcommand (e.g.
#                   `build nimony`), run before the tests below it.
# Neither present → the built-in nimony runner processes the directory's own
# `.nim` files (category from its `hastur.mode`) and recursion continues.

proc runSetupHastur(dir: string) =
  ## Prep step for a built-in-runner directory: run each line of
  ## `<dir>/setup.hastur` as a hastur subcommand before its tests. `--release`
  ## is forwarded so a release CI run also builds the toolchain in release
  ## mode (the child hastur wouldn't otherwise inherit it).
  if skipBuild: return
  let f = dir / "setup.hastur"
  if not fileExists(f): return
  let self = getAppFilename().quoteShell
  let relFlag = if release: " --release" else: ""
  for raw in lines(f):
    let line = raw.strip
    if line.len == 0 or line.startsWith("#"): continue
    exec self & relFlag & " " & line, showProgress = true

proc runSetupNimDir(c: var TestCounters; dir, forward: string; overwrite: bool) =
  ## A `setup.nim` owns its directory. Compile and run it, passing context on
  ## argv (the test dir, toolchain dir, cache dir, overwrite, forwarded
  ## flags); its exit code is the directory's verdict. The program reports its
  ## own per-test detail, so here the whole directory counts as one result.
  inc c.total
  let setupNim = dir / "setup.nim"
  let cache = nimcacheDir / "setupnim" / dir.splitPath.tail
  # `-o:` keeps the compiled runner in the cache; without it nim drops the
  # binary next to `setup.nim` and litters the test tree.
  let outBin = cache / "setup".addFileExt(ExeExt)
  var cmd = "nim c -r --warningAsError:ProveInit:off --warningAsError:Uninit:off" &
            " --nimcache:" & cache.quoteShell & " -o:" & outBin.quoteShell & " " &
            setupNim.quoteShell & " --" &
            " --dir:" & dir & " --bindir:" & toolchainDir & " --cachedir:" & nimcacheDir
  if overwrite: cmd.add " --overwrite"
  if forward.len > 0: cmd.add " --forward:" & forward
  if execShellCmd(cmd) != 0:
    inc c.failures

proc walkTests(c: var TestCounters; dir, forward: string; overwrite, isRoot: bool) =
  # `hastur.mode = skip` excludes a directory from the sweep, but only when the
  # walk *descends* into it — pointing hastur straight at it (isRoot) still
  # runs it. That's how a WIP/known-broken suite (e.g. dagon) stays out of the
  # default `all` run yet remains explicitly runnable via `hastur tests/dagon`.
  let cat = categoryOfDir(dir)
  if cat == Skip and not isRoot: return
  if fileExists(dir / "setup.nim"):
    runSetupNimDir(c, dir, forward, overwrite)
    return
  runSetupHastur(dir)
  var hasNim = false
  var subs: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim"): hasNim = true
    elif x.kind == pcDir: subs.add x.path
  if hasNim:
    # Leaf test directory: run its own `.nim` files via the flat built-in
    # runner and do NOT descend. Nested dirs here (`deps/`, `imp/`, `system/`,
    # …) hold import fixtures pulled in by those tests, not standalone tests —
    # the old per-category runner never entered them either.
    testDir(c, dir, overwrite, cat, forward)
  else:
    # Pure grouping directory (e.g. `tests/`, `tests/nimony/`): recurse.
    sort subs
    for s in subs: walkTests(c, s, forward, overwrite, isRoot = false)

proc walkRoots(roots: openArray[string]; forward: string; overwrite: bool) =
  ## Run one or more test trees, accumulating into shared counters and
  ## reporting once. `hastur <dir>` passes a single root; `all` passes
  ## `tests/` and `examples/`.
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  for r in roots:
    if not dirExists(r): quit "FAILURE: not a directory: " & r
    walkTests(c, r, forward, overwrite, isRoot = true)
  echo c.total - c.failures, " / ", c.total, " tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0: quit "FAILURE: Some tests failed."
  else: echo "SUCCESS."

proc walkCmd(dir, forward: string; overwrite: bool) =
  ## The general entry point: `hastur <dir>` runs the whole test tree at
  ## `<dir>`. `hastur tests/` is what `all` becomes.
  walkRoots([dir], forward, overwrite)

proc handleCmdLine =
  var primaryCmd = ""
  var rawPrimary = ""   # unnormalized first arg; a directory path stays intact
  var args: seq[string] = @[]

  var flags: set[RecordFlag] = {}
  var overwrite = false
  var forward = ""
  var withValgrind = false
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if primaryCmd.len == 0:
        primaryCmd = key.normalize
        rawPrimary = key
      else:
        args.add key
    of cmdLongOption, cmdShortOption:
      # `--cachedir` / `--jobs` / `--forward` can appear anywhere — they
      # configure the test runner regardless of position relative to the
      # primary cmd. `--forward` in particular MUST be position-agnostic
      # because the parallel test runner spawns child `hastur test ...`
      # invocations and threads the forward value back in after `test`,
      # so requiring it before the subcommand would silently lose it in
      # the child. Other long options stay tied to the pre-command
      # position (or to the `record` subcommand).
      let n = normalize(key)
      case n
      of "cachedir":
        if val.len == 0: writeHelp()
        nimcacheDir = val
      of "bindir":
        # Point hastur at a prebuilt/installed toolchain instead of the
        # project-local `bin/`. Implies `--no-build`: there is no source
        # tree to rebuild from when testing out-of-tree code.
        if val.len == 0: writeHelp()
        toolchainDir = val
        skipBuild = true
      of "jobs", "j":
        if val == "auto" or val.len == 0:
          parallelJobs = countProcessors()
        else:
          try: parallelJobs = max(1, parseInt(val))
          except: writeHelp()
      of "no-build", "nobuild":
        skipBuild = true
      of "valgrind":
        withValgrind = true
      of "forward":
        # Accumulate so callers can layer flags — `--forward:--cc:clang
        # --forward:--passL:-fuse-ld=lld` reaches nimony as both options
        # rather than only the last one. The whole string is appended
        # verbatim to the nimony command line.
        if forward.len > 0: forward.add ' '
        forward.add val
      else:
        if primaryCmd.len == 0 or primaryCmd == "record":
          case n
          of "help", "h": writeHelp()
          of "version", "v": writeVersion()
          of "codegen": flags.incl RecordCodegen
          of "ast": flags.incl RecordAst
          of "overwrite": overwrite = true
          of "release": release = true
          else: writeHelp()
        else:
          args.add key
          if val.len != 0:
            args[^1].add ':'
            args[^1].add val
    of cmdEnd: assert false, "cannot happen"
  if primaryCmd.len == 0:
    writeHelp()

  createDir binDir()

  case primaryCmd
  of "all":
    # `all` is now the tree walk: `tests/` (each suite via its setup.nim or the
    # built-in nimony runner; `tests/setup.hastur` builds the toolchain first)
    # plus `examples/`. Directories marked `hastur.mode = skip` (dagon, hexer)
    # stay out of the sweep but remain runnable via `hastur tests/<dir>`.
    walkRoots(["tests", "examples"], forward, overwrite)

  of "tiers":
    buildNimony()
    tierTests()

  of "boot":
    buildNimony()
    var bootArgs = ""
    if release: bootArgs.add "--opt:speed"
    for a in items(args):
      if bootArgs.len > 0: bootArgs.add ' '
      bootArgs.add quoteShell(a)
    # `--forward:<flag>` is appended verbatim to every stage's `nimony c`
    # command line. Unlike positional `args`, getopt keeps the value intact
    # (dashes and all), so this is the way to forward flags like
    # `-d:nimNativeAlloc` that must survive unmangled into nimony (and thus
    # into nimsem, where the `when defined(...)` is evaluated).
    if forward.len > 0:
      if bootArgs.len > 0: bootArgs.add ' '
      bootArgs.add forward
    bootCmd(bootArgs, withValgrind)

  of "selfcheck":
    selfcheckCmd()

  of "build":
    const showProgress = true
    exec "git submodule update --init"
    case (if args.len > 0: args[0] else: "")
    of "", "all":
      buildNifler(showProgress)
      buildNimsem(showProgress)
      buildNimony(showProgress)
      buildLengc(showProgress)
      buildShoggoth(showProgress)
      buildNiflink(showProgress)
      buildHexer(showProgress)
      buildNifmake(showProgress)
      buildNj(showProgress)
      buildVl(showProgress)
      buildValidator(showProgress)
      buildDagon(showProgress)
      buildPnak(showProgress)
    of "nifler":
      buildNifler(showProgress)
    of "nimony":
      buildNimsem(showProgress)
      buildNimony(showProgress)
      buildHexer(showProgress)
    of "lengc":
      buildLengc(showProgress)
    of "shoggoth":
      buildShoggoth(showProgress)
    of "niflink":
      buildNiflink(showProgress)
    of "arkham":
      buildArkham(showProgress)
    of "nifasm":
      buildNifasm(showProgress)
    of "native":
      # The C-free native toolchain used by `nimony n`: arkham + nifasm (from
      # the sibling `../nativenif`) plus shoggoth (the opt-gated Leng optimizer
      # that also feeds the native path). Kept out of the default `all` build so
      # the sibling-repo dependency stays opt-in.
      buildArkham(showProgress)
      buildNifasm(showProgress)
      buildShoggoth(showProgress)
    of "hexer":
      buildHexer(showProgress)
    of "nifmake":
      buildNifmake(showProgress)
    of "nj":
      buildNj(showProgress)
    of "vl":
      buildVl(showProgress)
    of "validator":
      buildValidator(showProgress)
    of "dagon":
      buildDagon(showProgress)
    of "pnak":
      buildPnak(showProgress)
    else:
      writeHelp()
    removeDir "nimcache"

  of "native":
    # Run the curated native-backend regression set through `nimony n`. Build the
    # front end AND the C-free native toolchain (arkham + nifasm + shoggoth live in
    # the sibling `../nativenif`; nifmake drives the `n` pipeline).
    buildShoggoth()
    buildArkham()
    buildNifasm()
    nativetests(overwrite)
  of "lengc":
    buildLengc()

  of "test":
    if args.len > 0:
      for arg in args:
        if arg.dirExists():
          testDirCmd arg, overwrite, forward
        else:
          test arg, overwrite, categoryOf(arg), forward
    else:
      quit "`test` takes an argument"
  of "bug", "debug":
    if args.len == 0:
      args = @["bug.nim"]
    bugCmd(args, forward)
  of "rep":
    repCmd()
  of "record":
    buildNimony()
    if args.len == 2:
      let inp = args[0].addFileExt(".nim")
      let outp = args[1].addFileExt(".nim")
      let dest = if splitFile(args[1]).dir == "": "tests/nimony/basics" / outp
                 else: outp
      record inp, dest, flags, categoryOf(dest)
    else:
      quit "`record` takes two arguments"
  of "clean":
    removeDir "nimcache"
    removeDir "bin"
    for n in 0 .. 9:
      removeDir "bin" & $n
  of "install":
    runInstall(args)
  of "sync":
    syncCmd(if args.len > 0: args[0] else: "")
  of "pull":
    pullpush("pull")
  of "push":
    pullpush("push")
  else:
    if dirExists(rawPrimary):
      walkCmd(rawPrimary, forward, overwrite)
    else:
      quit "invalid command: " & primaryCmd

# `handleCmdLine()` runs only when hastur is the program. Imported as a module
# (e.g. from a directory's `setup.nim` custom runner) it is just the test kit:
# `runNifToolTests`, `buildNj`, the toolchain resolution, counters, … with no
# CLI side effects.
when isMainModule:
  handleCmdLine()

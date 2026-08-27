## Hastur - Tester tool for Nimony and its related subsystems (Leng etc).
## (c) 2024-2025 Andreas Rumpf

when defined(windows):
  when defined(gcc):
    when defined(x86):
      {.link: "../icons/hastur.res".}
    else:
      {.link: "../icons/hastur_icon.o".}

import std / [syncio, assertions, parseopt, strutils, times, os, osproc, algorithm, typedthreads, locks, sets]
when defined(windows):
  import std/winlean
else:
  import std/posix

import lib / [nifindexes, argsfinder]
from lib / nifpools import NoLineInfo
import gear2 / modnames

const
  Version = "0.6.0"
  Usage = "hastur - tester tool for Nimony Version " & Version & """

  (c) 2024-2026 Andreas Rumpf
Usage:
  hastur [options] [command] [arguments]

Commands:
  build [all|nimony|nifler|hexer|lengc|shoggoth|nifmake|nj|vl|validator|dagon|pnak|arkham|nifasm|native|nifbench]   build selected tools (default: all).
                       `nifbench` is the NIF micro-benchmark suite (src/nifbench),
                       built with host Nim so it can be compared against the same
                       source built by `nimony c` and `nimony n`.
                       `all` builds arkham + nifasm too when the sibling
                       `../nativenif` is checked out, and says so when it is not;
                       `native` (arkham + nifasm + shoggoth) asks for them by
                       name and errors out instead.
  tiers [native]       compile every module on the bootstrap list with nimony.
                       `native` uses `nimony n` (arkham + nifasm) instead of
                       `nimony c`: the list is a walk of the bootstrap DAG from
                       the leaves up, so it attributes each native gap to the
                       smallest module that reaches it — which `boot` cannot do,
                       since it compiles three whole tools and reports only the
                       first that died. Nothing fails fast: the run ends with one
                       line per distinct diagnostic and the modules reaching it.
                       `--forward:` is appended to every compile (e.g.
                       `--forward:-d:release`).
  boot [options]       Self-host the *full* nimony toolchain (nimony,
                       nimsem, hexer). `bin0/` is a fresh copy of the
                       host-Nim-built toolchain; `binN/` is `binN-1/`'s
                       nimony recompiling all three from source. Runs a
                       fixed number of self-compile passes and leaves the
                       results in place — nothing is installed back to
                       `bin/`. Extra args are forwarded to every
                       `nimony c` invocation.
                       Every stage is compiled with `-d:release` by default
                       (the wider test: shoggoth + the `when defined(release)`
                       paths). `--no-release` boots at the default opt level.
                       To boot at another mode use `--forward:` so the flag
                       survives getopt intact — `--forward:-d:danger` replaces
                       the default rather than stacking on it.
                       On linux/amd64 every stage is built with the C-FREE
                       NATIVE backend (`nimony n`: arkham + nifasm, no C
                       compiler, no libc) when those two are built; otherwise
                       through the C backend.
  selfcheck            full compiler regression check: rebuilds the nimony
                       toolchain (nimony+nimsem+hexer share `programs.nim`),
                       runs `tiers`, then `boot --valgrind` (release). Use this
                       after touching any module the compiler itself imports.
  <dir>                run the test tree rooted at <dir> (see Files below):
                       each directory is either a setup.nim custom runner or
                       the built-in nimony runner, recursively. This is the
                       general entry point — point it at any suite.
  all                  run the whole suite: `<tests>` + `<examples>`.
  nativevalgrind       run the native regression set with `-d:valgrind`, under
                       memcheck, and demand that memcheck reports nothing. This
                       is what says the allocator's heap instrumentation
                       (`lib/std/system/valgrind.nim`) describes the heap
                       truthfully rather than plausibly. Linux/AArch64 for now,
                       which is where `VgClientRequest` has a lowering.
  native               run the curated native-backend regression set through
                       `nimony n` (arkham + nifasm, from the sibling
                       `../nativenif` checkout). See `NativeTestDirs`/`Files`.
  lengc                 run Leng tests.
  test <file>/<dir>    run a single test <file>, or a flat <dir> of tests.
  joined <dir>         run <dir>'s joinable tests as ONE program (see
                       `--joined` below). This is what the test pool spawns
                       per directory; by hand it is how you run or debug a
                       single group.
  bug [file]           build nimony+hexer and compile <file> to fill nimcache/.
                       If no file is provided `bug.nim` is used.
  rep                  repeat the last failing tool command from the session.
  record <file> <tout> track the results to make it part of the test suite.
  update deps          re-pin `src/nativenif.commit` — the sibling nativenif
                       commit arkham and nifasm are built from — to whatever
                       `../nativenif` currently has checked out. Every other
                       command PUTS nativenif on that commit before building
                       it (a dirty nativenif tree is left alone, with a
                       warning). The file is read at run time; an empty or
                       absent one means "build whatever is checked out".
  install              write activation script(s) at the project root that
                       prepend the toolchain dirs to `$PATH` for the current
                       shell. On Windows, also download MinGW+LLVM (gcc,
                       clang, lld) and Nim's DLL deps into `external/`.
  clean                remove all generated files.
  sync [new-branch]    delete current branch and pull the latest
                       changes from remote. Optionally creates a new branch.

Arguments are forwarded to the Nimony compiler.

Options:
  --overwrite           overwrite the selected test results. Implies
                        `--joined:off`: a golden is regenerated from the test
                        running on its own. A plain test that prints without
                        an `.output` file gets one written here — that is what
                        makes it joinable.
  --ast                 track the contents of the AST too
  --codegen             track the contents of the code generator too
  --version             show the version
  --help                show this help
  --forward:OPTION      pass an option to the Nimony compiler
  --debug               build the front-end tools (nimony, nimsem, hexer, …)
                        unoptimized; they default to -d:release, like arkham
                        and nifasm. For gdb work on the tools themselves —
                        a debug toolchain is ~8x slower at compiling, so all
                        test runs want the default
  --release             no-op for the toolchain (release is the default); for
                        `boot` it additionally compiles the bootstrapped
                        nimony's own output with --opt:speed
  --jobs:N|auto         run up to N tests in parallel (auto = #cores)
  --joined:on|off       compile a directory's plain tests into ONE program
                        (default on) instead of one per test. Most of a test's
                        cost is the process tree behind it — nifler, nimsem,
                        hexer, lengc, nifmake, the C compiler, the linker —
                        so this is the big lever on Windows. A group that
                        fails is re-run test by test, so failures stay
                        attributable; `off` skips the grouping altogether.
  --native:on|off       on WINDOWS, compile the tests `hastur native` vouches
                        for with `nimony n` rather than `nimony c` (default
                        on). Same assertion, no gcc and no linker behind it —
                        the other half of the lever `--joined` pulls. Ignored
                        on every other host.
  --cachedir:PATH       use PATH instead of `nimcache/` for intermediates
  --bindir:PATH         resolve the toolchain (nimony, lengc, …) from
                        directory PATH instead of hastur's own directory
                        (implies --no-build). Binaries not found there are
                        looked up on `$PATH`.
  --no-build            skip the setup.hastur prep step during the tree walk
  --skip:DIR            leave DIR out of the tree walk (repeatable). For
                        splitting one sweep across CI runners: the tester job
                        passes `--skip:tests/boot` while a second job runs
                        `hastur tests/boot`. Unlike `hastur.mode = skip` this
                        does not change what a plain local run covers.
  --native-debug        build arkham + nifasm unoptimized (they default to
                        -d:release); for `-d:arkhamDbgSym` / gdb toolchain work
  --boot-backend:auto|c|native
                        which backend `boot` compiles the stages with. `auto`
                        (default) is per-host: `nimony n` where the native
                        backend is complete and arkham/nifasm are in bin/,
                        `nimony c` otherwise. The other two force it, which is
                        how the two are timed against each other.
  --valgrind            for `boot`: build with -DMI_TRACK_VALGRIND=1 so
                        mimalloc plays nicely with valgrind, then run a
                        valgrind smoke test on the bootstrapped binary.
                        Forces the C backend (valgrind cannot see the native
                        backend's libc-free heap).

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
  <test>.nojoin         keep <test> out of its directory's joined program (see
                        `--joined`). For a test whose output is not
                        reproducible in a shared process, or that races with
                        its neighbours over a shared build artifact.
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
    failed: seq[string]
      ## Names of the tests that failed, in failure order. A run of several
      ## hundred tests streams far too much output for a bare "N / M" to be
      ## actionable — and under `--jobs` the child's own FAILURE block sits
      ## thousands of lines up, interleaved with every other worker's.
      ## `reportFailures` replays this list right above the summary.

proc noteFailure(c: var TestCounters; file: string) =
  inc c.failures
  c.failed.add file

proc failure(c: var TestCounters; file, expected, given: string) =
  noteFailure c, file
  var m = file & " --------------------------------------\nFAILURE: expected:\n"
  m.add expected
  m.add "\nbut got\n"
  m.add given
  m.add "\n"
  echo m

proc failure(c: var TestCounters; file, msg: string) =
  noteFailure c, file
  let m = file & " --------------------------------------\nFAILURE: " & msg & "\n"
  echo m

proc reportFailures(c: TestCounters) =
  if c.failed.len == 0: return
  echo "\nFAILED (", c.failed.len, "):"
  for f in c.failed: echo "  ", f
  echo ""

proc diffFiles(c: var TestCounters; file, a, b: string; overwrite: bool) =
  if not os.sameFileContent(a, b):
    if overwrite:
      copyFile(b, a)
    else:
      let gitCmd = "git diff --no-index $1 $2" % [a.quoteShell, b.quoteShell]
      let (diff, diffExitCode) = execCmdEx(gitCmd)
      if diffExitCode <= 1:
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

proc normalizeDirKey(p: string): string =
  ## Compare directories by a single spelling: the tree walk builds paths with
  ## the host separator (`tests\boot` on Windows), while a `--skip:` on the
  ## command line is written portably (`tests/boot`).
  result = p.replace('\\', '/').strip(chars = {'/'})

var skipDirs*: seq[string] = @[]
  ## Directories the tree walk leaves out, from `--skip:<dir>` (repeatable).
  ## Unlike `hastur.mode = skip`, this is a property of *this run*, not of the
  ## suite: it exists so CI can split one sweep across runners (`--skip:tests/boot`
  ## on the job that tests, `hastur tests/boot` on the job that boots) without
  ## changing what a plain local `hastur all` covers.

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

# ── native-backend regression set ────────────────────────────────────────────
# The C-FREE native path (`nimony n` → arkham + nifasm, sibling `../nativenif`) is
# still incomplete, so we can't run the whole suite through it. Instead this is an
# explicit whitelist of what is known to run correctly natively — a regression
# guard: a native miscompile that diverges from the (spec-pinned) result is caught.
# Grow it as the backend gains features (the same record-what-works philosophy as
# the rest of hastur). `NativeTestDirs` are directories that pass apart from the
# files `NativeTestSkip` names (negative `.msgs` tests auto-skipped too);
# `NativeTestFiles` are individual passers from otherwise-partial directories.
const
  NativeTestDirs = [
    "tests/nimony/arc",        # full ARC suite — byte-identical to the C backend
    "tests/nimony/closures"
  ]

  NativeTestDirsWindows = [
    # Windows grew a native target later than the other hosts, so what it is known
    # to run correctly is recorded separately until a run elsewhere confirms the
    # same — the whole point of this file is that it states OBSERVED results, and
    # these were observed on Windows. Fold an entry into `NativeTestDirs` once it
    # has been seen green on Linux/macOS too.
    #
    # Every entry below was established the same way, under Wine against the real
    # MinGW bundle (`tools/wine_test.sh`, see `doc/wine_testing.md`): each of the
    # directory's tests run through `nimony n` on its own AND the whole directory
    # run as one native joined program, which is the shape the tree walk actually
    # compiles it in and NOT the same question: five of these directories passed
    # test-by-test and failed as a group, on three backend bugs that only a
    # module compiled as an import can reach (see `NativeJoinSkip`).
    #
    # `tests/nimony/stdlib`: 45 of its 53 pass; the rest are in `NativeTestSkip`.
    # This is the suite that covers the process vectors the Windows entry point
    # does not receive (`tcmdline`, `tenvvars`, `tos`) — the reason it is worth
    # running natively at all rather than only through the C backend.
    "tests/nimony/stdlib",
    "tests/nimony/assembler",
    "tests/nimony/basics",
    "tests/nimony/borrows",
    "tests/nimony/casestmt",
    "tests/nimony/consteval",
    "tests/nimony/converter",
    "tests/nimony/cyclic",
    "tests/nimony/enums",
    "tests/nimony/exceptions",
    "tests/nimony/intrinsics",
    "tests/nimony/lastuse",
    "tests/nimony/lookups",
    "tests/nimony/macros",
    "tests/nimony/mut",
    "tests/nimony/overload",
    "tests/nimony/plugins",
    "tests/nimony/sets",
    "tests/nimony/strings",
    "tests/nimony/templates",
    "tests/nimony/threads",
    "tests/nimony/types",
    "tests/nimony/untyped",
    "tests/nimony/when"
  ]

  NativeJoinSkip: array[0, string] = [
    # Directories whose tests each compile and run correctly through `nimony n`
    # — they are in the lists above — but whose JOINED program does not, so the
    # group keeps the C backend while the same directory's standalone tests
    # stay native.
    #
    # Empty, and worth keeping empty: the tree walk compiles a group's members
    # as IMPORTS of a generated driver, and no other runner does — `hastur
    # native` compiles every test as a MAIN module — so this list is the only
    # place a native gap in that shape can be recorded. Five directories were
    # here when it was written (arc, strings, consteval, templates, untyped)
    # and all five were three backend bugs, not five: a peephole that answered
    # its liveness question across proc boundaries, a template-gensym'd routine
    # named with local layout, and a `dynlib` that only one backend wrote into
    # the shared `.x.nif`. An entry belongs here only while such a bug is open.
  ]

  NativeTestSkip = [
    # Files a `NativeTestDir` does not cover yet, with the reason each fails. All
    # but the last PASS through the C backend, so each of those is a gap in the
    # native path rather than in the test — which is what makes them worth naming
    # individually instead of dropping the directory.
    #
    # A libc math/stdio extern with a FLOAT parameter. Win64 indexes its SSE
    # argument registers positionally (an xmm slot burns the GPR of the same
    # position), which arkham's ABI planner does not model — `emitWinExtproc`
    # rejects it rather than guess. Moot until the freestanding target has a libc
    # to bind these to at all.
    "tests/nimony/stdlib/tcomplex",     # sqrtf
    "tests/nimony/stdlib/tmath",        # frexp
    "tests/nimony/stdlib/tstrutils",    # snprintf
    # `mov` of a `bool` into an `(i 64)` result: nifasm's widening rule admits
    # int→int only, so a bool source is a type error. Arch-neutral.
    "tests/nimony/stdlib/thashes",
    # Compiles and links, but the result diverges from the spec-pinned output —
    # a native MISCOMPILE (all three are green on the C backend). Undiagnosed.
    "tests/nimony/stdlib/tbitops",
    "tests/nimony/stdlib/tencodings",
    "tests/nimony/stdlib/trandom",
    # The odd one out: NOT a native gap. Its `std/typetraits` compile-time plugin
    # fails to run ("vfs: open failed"), and the C backend fails it identically,
    # so nothing here would fix it.
    "tests/nimony/stdlib/ttypetraits"
  ]
  NativeTestFiles = [
    # Portable intrinsics: `(instr …)` lowers to the target's own instruction —
    # x86-64 `bsf`, AArch64 `rbit`+`clz` — so this is the one test whose POINT is
    # that the C and native backends agree on results they reach by different
    # instructions.
    "tests/nimony/intrinsics/tintrinsics",
    # The atomics are intrinsic rows too, and the backends reach them from even
    # further apart: C emits the `__atomic_*` builtins, x86-64 a `lock`-prefixed
    # sequence, AArch64 an `ldaxr`/`stlxr` retry loop. Running it here is what says
    # the three agree — including the compare-exchange FAILURE path, which a wrong
    # lowering still "passes" single-threaded unless the test reads back the value
    # the CAS observed.
    "tests/nimony/intrinsics/tatomics",
    # The valgrind client request. Native-only by nature: under the C backend the
    # mechanism is valgrind's own headers around mimalloc, so there is no row to
    # lower and the test compiles to its `echo` alone. What it checks here is the
    # property the C path never has to have — that the request sequence is inert
    # on real hardware, and leaves every live register where it found it.
    "tests/nimony/intrinsics/tvalgrind",
    # `div`/`mod` by a constant power of two, which arkham strength-reduces to a
    # shift (and, for a signed dividend, a round-toward-zero bias). Native-relevant
    # by nature: under `nimony c` the C compiler owns that rewrite, so only a
    # backend doing its own can round the wrong way on a negative dividend.
    "tests/nimony/basicarith/tdivmodpow2",
    # `float32` at every width-sensitive spot. Native-relevant by nature: the C
    # backend gets the widths from C's own type system, so this only ever fails on
    # a backend that has to derive them itself.
    "tests/nimony/types/tfloat32",
    # Indexing an array of AGGREGATES and reading one field of the element: the
    # element stride and the access width are different numbers, which AArch64's
    # register-offset addressing cannot express (`[Xn, Xm, LSL #k]` scales by the
    # ACCESS width, not by an arbitrary amount). Native-relevant by nature — the C
    # compiler owns addressing under `nimony c`. It read `arr[i div 2]` for a 4-byte
    # field, which is what kept `-d:release` off the native self-host.
    "tests/nimony/calls/tindexednarrowfield",
    # More arguments than the ABI has argument registers, with the parameter shapes
    # that give a stack-passed one a stack HOME (a `var seq` written through, an
    # object read after a call). Native-relevant by nature: the C compiler owns
    # argument passing under `nimony c`, so only a backend that marshals arguments
    # itself can fail this — arkham's AArch64 prologue used to abort on it outright
    # (">8 integer params (stack TODO)"), which is what kept `nimony.nim` off the
    # native bootstrap ladder.
    "tests/nimony/calls/tstackparams",
    # cps/* — closures & continuation-passing (indirect calls through fn-ptr values)
    "tests/nimony/cps/tbasicpassive",
    "tests/nimony/cps/tclosure",
    "tests/nimony/cps/tclosure_iter_basic",
    "tests/nimony/cps/tclosure_iter_body_capture",
    "tests/nimony/cps/tclosure_iter_break",
    "tests/nimony/cps/tclosure_iter_envcheck",
    # One frame field of every kind. Native-relevant by nature: this is the
    # backend that depends on `oconstr` being total, since it stores exactly the
    # fields the constructor lists and zeroes nothing.
    "tests/nimony/cps/tclosure_iter_frametypes",
    # Sets in a closure iterator, in both representations (word-sized and the
    # 32-byte array). Worth running natively because the big-set path builds
    # its value through `zeroMem` plus per-element stores rather than a
    # literal, which is a different shape for the back end than the C one.
    "tests/nimony/cps/tclosure_iter_sets",
    "tests/nimony/cps/tclosure_iter_string",
    "tests/nimony/cps/tclosure_iter_var",
    "tests/nimony/cps/tfirstpassive",
    "tests/nimony/cps/tif",
    "tests/nimony/cps/tmethods",
    "tests/nimony/cps/tnestedloops",
    "tests/nimony/cps/trecursive",
    "tests/nimony/cps/tsuspend",
    "tests/nimony/cps/tsuspend_resume",
    "tests/nimony/cps/tparkstate",
    "tests/nimony/cps/ttry",
    # The native backend is the only one that HAS a stack trace: it walks nifasm's
    # own per-proc table, seeded by a `{.naked.}` proc. Running it here is what
    # checks the three halves agree — the table nifasm lays down, the two
    # intrinsics arkham lowers, and the walk in `lib/std/stacktraces`.
    "tests/nimony/stacktraces/tstacktrace",
    # A `const` set is read straight out of read-only data, so its membership
    # test is an indexed load from a GLOBAL — a shape the C backend never sees a
    # register problem in and arkham got wrong twice. Native-only by nature.
    "tests/nimony/sets/tconstsetscan",
    # The one test here whose point is WHERE its code is compiled rather than
    # what it does: everything it checks lives in `deps/mimportshapes`, so this
    # is the suite's only native compile of a non-main module. Every other test
    # arrives as `--isMain`, and three bugs lived in that gap — see the test.
    "tests/nimony/modules/timportshapes"
  ]

const
  JoinedPrefix = "_hastur_"
    ## Generated files carry this prefix; they are never tests themselves.
  JoinedDriver = JoinedPrefix & "joined"
    ## Basename of the per-directory joined-group driver module.
  MinJoinGroup = 2
    ## A group of one saves nothing and only obscures the reporting.

var joinTests* = true
  ## `--joined:off` restores one process per test (bisecting a group, or
  ## working on the joining itself). See the `joined tests` section below.

type WorkItem = object
  ## One unit of work for the parallel pool: either a single test file, or a
  ## whole directory's joined group. `weight` is how many tests the unit
  ## accounts for, so the run's totals stay per-test even though a group is a
  ## single process.
  path: string
  joined: bool
  weight: int
  native: bool
    ## Compiled by `nimony n` (see `walkUsesNative`). The worker decides this
    ## for itself from the same inputs; the parent needs to know it too,
    ## because the two backends take their cache prefill from different
    ## warmups and handing a native item the C one breaks its build.

proc isGeneratedTestFile*(file: string): bool {.inline.} =
  file.splitFile.name.startsWith(JoinedPrefix)

# ── native compilation inside the tree walk ──────────────────────────────────
# What a Windows test costs is mostly not nimony: it is the gcc invocation per
# module and the link that follow it, each a process creation the platform
# charges dearly for (see the `joined tests` note below — same cause, different
# lever). `nimony n` has no such tail: arkham and nifasm consume the same front
# end and write the PE themselves. So for a test the native backend is KNOWN to
# compile correctly, the walk takes that path on Windows and pays a fraction of
# the time, asserting exactly what it asserted before — the same program's
# output against the same `.output` file, only the code generator differs.
#
# "Known to compile correctly" is not a new list: it is the `hastur native`
# whitelist directly above, read the same way. A test the native suite does not
# vouch for keeps the C backend, so this can never turn a native gap into a
# tree-walk failure that `hastur native` would not already report.

var walkNative* = true
  ## `--native:off` puts the whole tree walk back on the C backend. Only ever
  ## consulted on Windows (elsewhere `walkUsesNative` is false regardless): the
  ## other hosts run their C toolchain fast enough that the native path would
  ## trade coverage for nothing.

proc nativeWhitelisted(file: string): bool =
  ## Is `file` one of the tests `hastur native` runs — i.e. one the native
  ## backend is recorded as compiling correctly on this host?
  let p = file.replace('\\', '/')
  if not p.endsWith(".nim"): return false
  let stem = p[0 ..< p.len - ".nim".len]
  for f in NativeTestSkip:
    if stem == f: return false
  for f in NativeTestFiles:
    if stem == f: return true
  # A whitelisted directory covers its own `.nim` files only; the nested dirs
  # (`deps/`, `imp/`, …) hold import fixtures, which are never tests. Sliced by
  # hand rather than with `parentDir`, which re-spells the result with the host
  # separator — a backslash on the one platform this runs on, and so never
  # equal to a list entry.
  let sep = stem.rfind('/')
  if sep < 0: return false
  let dir = stem[0 ..< sep]
  for d in NativeTestDirs:
    if dir == d: return true
  when defined(windows):
    for d in NativeTestDirsWindows:
      if dir == d: return true

proc nativeJoinable(dir: string): bool =
  ## May this directory's joined group be compiled natively? Separate from
  ## `walkUsesNative` because it is a different question: a group compiles its
  ## members as imports, and `NativeJoinSkip` records where the native backend
  ## gets that shape wrong for tests it gets right on their own.
  let d = dir.replace('\\', '/').strip(chars = {'/'})
  for x in NativeJoinSkip:
    if d == x: return false
  result = true

proc walkUsesNative(file: string; cat: Category): bool =
  ## Whether the tree walk compiles this test with `nimony n` rather than
  ## `nimony c`. Windows-only, and only for a plain compile-run-diff test:
  ## a `.msgs` spec, a golden `.nim.c`, a `.nif` dump or a valgrind run each
  ## pin the test to the C pipeline, and every non-`Normal` category carries
  ## flags (`--noSystem`, `--compat`, `--opt:speed`) the native path does not
  ## take.
  when not defined(windows):
    result = false
  else:
    if not walkNative or cat != Normal: return false
    # A plain clone of this repo has no `../nativenif`, so `build all` skips
    # arkham and nifasm and says so rather than failing; the walk has to make
    # the same allowance or every whitelisted test would die on a missing tool.
    # Safe to ask here: `tests/setup.hastur` builds the toolchain on the way
    # down, before any leaf directory is planned.
    if not (fileExists(toolExe("arkham")) and fileExists(toolExe("nifasm"))):
      return false
    for ext in [".msgs", ".nim.c", ".nif", ".valgrind"]:
      if file.changeFileExt(ext).fileExists(): return false
    result = nativeWhitelisted(file)

proc joinable*(file: string; cat: Category): bool =
  ## A test folds into its directory's joined group when nothing about it needs
  ## a process of its own: it must be a plain compile-run-diff-output test (no
  ## expected diagnostics, no golden C or NIF to diff, no non-zero exit code, no
  ## valgrind run), it must not care about being the main module, and it must
  ## not opt out with a `.nojoin` sidecar. Everything else keeps its own run.
  ## Whether a group is actually formed is `joinMembers`' call — this is only
  ## about the test itself.
  if cat != Normal: return false
  if isGeneratedTestFile(file): return false
  for ext in [".msgs", ".nim.c", ".nif", ".exitcode", ".valgrind", ".nojoin"]:
    if file.changeFileExt(ext).fileExists(): return false
  # `isMainModule` is false for an imported member, so such a test would
  # silently exercise the other branch.
  result = not readFile(file).contains("isMainModule")

proc nimonyCmdFor(file: string; cat: Category; forward: string): string =
  ## The `nimony` command line a test file is compiled with, minus the file
  ## itself. Shared by the per-file runner and the joined-group runner so a
  ## test's module is built with the exact same flags either way — they land
  ## in the same `nimcache/`, and differing flags would thrash it.
  result = "--isMain"
  case cat
  of Normal, Valgrind, Optimized, Skip: discard
  of Basics:
    result.add " --noSystem"
  of Tracked:
    result.add markersToCmdLine(extractMarkers(readFile(file)), file)
  of Compat:
    result.add " --compat"
  if forward.len != 0:
    result.add ' '
    result.add forward
  # The libc-free stdlib (native allocator + raw-syscall IO) is the compiler's
  # default now, but some tests assume the libc build, so they opt back in with
  # `-d:useLibc`: valgrind can only track the libc (mimalloc) heap — the native
  # mmap heap has no malloc hooks and is invisible to it — and the checked-in
  # golden `.nim.c` files were generated for the libc configuration.
  if cat == Valgrind or
     file.changeFileExt(".valgrind").fileExists() or
     file.changeFileExt(".nim.c").fileExists():
    result.add " -d:useLibc"
  when defined(linux):
    # Only request valgrind-tracked mimalloc when valgrind is actually present;
    # the flag pulls in `<valgrind/valgrind.h>`, which a valgrind-less box lacks.
    if hasValgrind:
      result.add " --passC:\"-DMI_TRACK_VALGRIND=1\" "
    else:
      result.add " "
  else:
    result.add " "

proc nimonyNativeCmdFor(forward: string): string =
  ## The flags a NATIVE tree-walk compile takes, minus the file. Deliberately
  ## only `--forward`'s: `execNimonyNative` already supplies
  ## `n --silentMake --isMain`, and every flag `nimonyCmdFor` adds beyond that
  ## names something the native path does not have (`-d:useLibc` picks the libc
  ## stdlib for a golden `.nim.c` or a valgrind run, `--passC` a C compiler) —
  ## which is also why `walkUsesNative` excludes the tests that need them.
  result = ""
  if forward.len != 0:
    result.add forward
    result.add ' '

proc testFile(c: var TestCounters; file: string; overwrite: bool; cat: Category; forward: string) =
  #echo "TESTING ", file
  let failuresBefore = c.failures
  inc c.total
  let (compilerOutput, compilerExitCode) =
    if walkUsesNative(file, cat):
      execNimonyNative(nimonyNativeCmdFor(forward) & quoteShell(file))
    else:
      execNimony(nimonyCmdFor(file, cat, forward) & quoteShell(file), cat)

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
      elif overwrite and testProgramExitCode == 0 and
           testProgramOutput.strip.len > 0 and joinable(file, cat):
        # A joined member's share of the group's output IS its `.output` file,
        # so a member that prints without one would leave text nothing accounts
        # for. `--overwrite` is where that gap gets closed: record what the
        # test prints (which also gives it an output check it never had).
        writeFile(output, testProgramOutput)

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

# ── joined tests ("megatest") ────────────────────────────────────────────────
# The dominant cost of the suite is not compiling test code, it is the process
# tree each test drags along: nimony spawns nifler/nimsem/hexer/lengc/nifmake,
# nifmake spawns a C compiler and the linker, and then the executable itself
# runs. That is a dozen-odd process creations per test — cheap on Unix, brutal
# on Windows, and paid ~500 times over.
#
# So a directory's plain tests are compiled into ONE program: a generated
# driver imports each of them, and module initialization (which is where a test
# file's top-level code lives) runs in import order. The program's output is
# therefore the members' outputs concatenated, which is exactly what the
# members' `.output` files already spell out — no new golden files, and every
# test stays individually runnable via `hastur test <file>`.
#
# Same idea as Nim testament's `megatest`, with one deliberate difference:
# testament interleaves marker modules (`echo "megatest:processing: ..."`)
# between the members to attribute output. Here a marker module would double
# the module count — and modules, not tests, are what the process spawns are
# proportional to — so instead a group that diverges *at all* is re-run
# member-by-member, which pins the blame precisely and costs nothing when green.

proc joinMembers*(dir: string; cat: Category; overwrite: bool): seq[string] =
  ## The joinable tests of `dir`, sorted — empty when this directory forms no
  ## group. Both the parent (planning the run) and the `joined` worker derive
  ## the member list this way, so they agree on what a group contains without
  ## having to pass it along.
  ##
  ## `--overwrite` never joins: regenerating a member's `.output` means seeing
  ## that member's output on its own.
  result = @[]
  if cat != Normal or not joinTests or overwrite: return
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim") and joinable(x.path, cat):
      result.add x.path
  sort result

proc joinedExpectedOutput(files: openArray[string]): string =
  ## What the joined program must print: each member's `.output` in import
  ## order. A member without one is expected to print nothing — the same
  ## contract `testFile` enforces is absent there, made explicit here.
  result = ""
  for f in items files:
    let o = f.changeFileExt(".output")
    if o.fileExists():
      let spec = readFile(o).strip
      if spec.len > 0:
        if result.len > 0: result.add '\n'
        result.add spec

proc joinedTest(c: var TestCounters; dir: string; files: seq[string];
                overwrite: bool; forward: string) =
  ## Run `files` as one program. On any divergence — the group does not
  ## compile, the program exits non-zero, or the output is not the members'
  ## `.output` files concatenated — fall back to running each member on its
  ## own, so the report names the test that actually broke.
  var driverSrc = "# Generated by hastur; see `joinedTest`. Not a test itself.\n"
  for f in files:
    driverSrc.add "import \"" & f.splitFile.name & "\"\n"
  let driver = dir / JoinedDriver.addFileExt(".nim")
  # Rewrite only on change: an untouched driver keeps its mtime, so a group
  # whose members did not change stays fully cached across runs.
  if not fileExists(driver) or readFile(driver) != driverSrc:
    writeFile(driver, driverSrc)

  template bail(reason: string; detail: string) =
    echo dir, ": joined group ", reason, "; re-running its ", files.len,
         " tests individually"
    if detail.len > 0: echo detail
    for f in files: testFile c, f, overwrite, Normal, forward
    return

  # A group is compiled by ONE backend: whether that is the native one is a
  # property of the members, not of the generated driver (which has no test
  # sidecars of its own to look at). All-or-nothing on purpose — splitting a
  # directory into a native group and a C group would add a whole second
  # process tree, and the fixed cost of one of those is most of what a group
  # costs, so the split would give the time back.
  var native = files.len > 0 and nativeJoinable(dir)
  if native:
    for f in files:
      if not walkUsesNative(f, Normal): native = false; break
  let (compilerOutput, compilerExitCode) =
    if native:
      execNimonyNative(nimonyNativeCmdFor(forward) & quoteShell(driver))
    else:
      execNimony(nimonyCmdFor(driver, Normal, forward) & quoteShell(driver), Normal)
  if compilerExitCode != 0:
    bail "did not compile", compilerOutput

  let exe = driver.generatedExeFile()
  let (progOutput, progExitCode) = osproc.execCmdEx(quoteShell exe)
  if progExitCode != 0:
    bail "exited with " & $progExitCode, progOutput
  if progOutput.strip != joinedExpectedOutput(files):
    bail "printed unexpected output", ""

  for f in files:
    inc c.total
    echoTestSuccess f

proc canRunParallel(cat: Category): bool {.inline.} =
  ## `Compat` and `Basics` reset `nimcache/` around the loop and so are
  ## not parallel-safe with the current single-cache layout. Other
  ## categories use isolated per-test cache dirs and parallelize fine.
  cat notin {Compat, Basics}

proc warmupSharedCache(native = false): string =
  ## Compile `tools/warmup.nim` once into `nimcache/warmup/` so each
  ## parallel test can start with system + common stdlib bundles already
  ## present. Returns the warmup cache directory, or "" on opt-out
  ## (warmup source missing or compile failed — tests still work, just
  ## without the savings).
  ##
  ## `native` seeds the OTHER pipeline's cache, in a directory of its own.
  ## The two must not mix: the intermediates the C and native backends read
  ## share file names but not content — a native compile handed the C run's
  ## bundle of `system` gets one whose externs have lost their `dynlib`, and
  ## arkham stops at the first of them ("`GetStdHandle` names no import
  ## library"). So a native work item prefills from here and a C one from
  ## there, and neither ever sees the other's files.
  const warmupSrc = "tools/warmup.nim"
  if not fileExists(warmupSrc):
    # Loud, because the fallback is silent-but-slow: without the prefill every
    # test recompiles `system` from scratch (~3s each on Windows CI, ~700
    # tests). A missing warmup file previously just disabled the optimization
    # with no output at all, so the regression was invisible.
    stderr.writeLine "warmup: " & warmupSrc &
      " missing; every test will recompile the stdlib from scratch"
    return ""
  result = nimcacheDir / (if native: "warmup_native" else: "warmup")
  let nimony = toolExe("nimony")
  if not fileExists(nimony):
    stderr.writeLine "warmup: skipping, no nimony at " & nimony
    return ""
  # The same command the work items are compiled with, so what lands here is
  # exactly what they would have produced themselves (`execNimonyNative` for
  # the native side, `execNimony`'s `c` for the other).
  let cmd = nimony.quoteShell &
            (if native: " n --silentMake --isMain --nimcache:" else: " c --nimcache:") &
            result.quoteShell & " " & warmupSrc.quoteShell
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
      # The valgrind tests compile with `-d:useLibc` (valgrind can only track the
      # libc/mimalloc heap; the native mmap heap has no hooks), so the shared
      # `static.o` they reuse must be the *mimalloc* object — build the prebuild
      # probe with `-d:useLibc` too. Without it the libc-free default is used and
      # `static.o` is never produced (mimalloc isn't compiled), so valgrind runs
      # against an untracked heap and reports 0 allocations.
      cmd.add " -d:useLibc --passC:\"-DMI_TRACK_VALGRIND=1\""
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

proc parallelTestDir(c: var TestCounters; items: openArray[WorkItem];
                     overwrite: bool; cat: Category; forward: string;
                     jobs: int) =
  ## Run each work item in its own subprocess (`bin/hastur test ...` for a
  ## file, `bin/hastur joined ...` for a directory's group) with a per-item
  ## `--cacheDir` so concurrent compilations cannot collide on intermediates.
  ## Up to `jobs` subprocesses run at once. Test results are streamed in
  ## completion order; final pass/fail counts go into the shared `c`.
  let hastur = getAppFilename()
  prebuildSharedObjects(forward)
  let warmupCache = warmupSharedCache()
  # Seeded only when something in this run actually wants it: on every host but
  # Windows no item is native, and paying for a second warmup compile there
  # would be pure loss.
  var anyNative = false
  for it in items: (if it.native: anyNative = true)
  let nativeWarmupCache = if anyNative: warmupSharedCache(native = true) else: ""
  warmupCopySeconds = 0
  let parallelStart = epochTime()
  var queue: seq[(int, WorkItem)] = @[]   # (idx, item) preserving input order
  for i, it in pairs(items): queue.add (i, it)
  var head = 0

  type Slot = object
    p: Process
    idx: int
    item: WorkItem
    reader: Thread[ReaderArg]
  var slots = newSeq[Slot](jobs)
  var active = 0
  var stdoutLock = default(Lock)
  initLock(stdoutLock)

  proc launch(slot: int) =
    if head >= queue.len: return
    let (idx, item) = queue[head]
    inc head
    let cacheDir = nimcacheDir / ".par" / $idx
    prefillFromWarmup(if item.native: nativeWarmupCache else: warmupCache, cacheDir)
    var args = @[(if item.joined: "joined" else: "test"),
                 "--no-build", "--cachedir:" & cacheDir]
    # Forward the parent's resolved toolchain dir so each worker uses the
    # exact same binaries (the default is now hastur's own sibling dir, an
    # absolute path, not the literal "bin").
    args.add "--bindir:" & toolchainDir
    # A work item's backend is settled HERE, by the parent, because the parent
    # is what prefills the cache — and the two warmups cannot be mixed (see
    # `warmupSharedCache`). The worker would reach the same verdict on its own
    # for the item as planned, but not on every path through it: a joined group
    # that diverges re-runs its members one by one, and in a group that is not
    # all-native some of those members are individually eligible. That re-run
    # inherits the C prefill, so it must inherit the C backend with it.
    when defined(windows):
      if not item.native: args.add "--native:off"
    if overwrite: args.add "--overwrite"
    if forward.len > 0: args.add "--forward:" & forward
    args.add item.path
    let p = startProcess(hastur, args = args,
        options = {poStdErrToStdOut, poUsePath})
    slots[slot] = Slot(idx: idx, item: item, p: p)
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
        inc c.total, slots[s].item.weight
        if exit != 0:
          # A `test` worker exits 1; a `joined` worker exits with how many of
          # its members failed, so a group cannot hide a second failure.
          let failed = min(exit, slots[s].item.weight)
          inc c.failures, failed
          # The worker already printed which of its members failed; up here
          # only the unit has a name, so that plus the count is what the
          # summary can replay.
          c.failed.add(if failed > 1:
                         slots[s].item.path & " (" & $failed & " tests)"
                       else: slots[s].item.path)
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
  let members = joinMembers(dir, cat, overwrite)
  let joined = members.len >= MinJoinGroup
  var files: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim") and not isGeneratedTestFile(x.path):
      # When the directory has a joined group its members are covered by that
      # single program; only what is left over still needs a run of its own.
      if not (joined and joinable(x.path, cat)):
        files.add x.path
  sort files
  if cat in {Compat, Basics}:
    removeDir "nimcache"
  if joined:
    joinedTest c, dir, members, overwrite, forward
  if parallelJobs > 1 and canRunParallel(cat):
    if files.len > 0:
      var work: seq[WorkItem] = @[]
      for f in items files: work.add WorkItem(path: f, weight: 1)
      parallelTestDir(c, work, overwrite, cat, forward, parallelJobs)
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

proc joinedDirCmd(dir: string; overwrite: bool; forward: string) =
  ## `hastur joined <dir>` — the worker the parallel pool spawns for a group.
  ## Exits with the number of failed members so the parent can tally them (a
  ## per-file `hastur test` worker exits 1, i.e. its own single failure).
  var c = TestCounters(total: 0, failures: 0)
  # The group's membership does not depend on `--overwrite`; only whether the
  # members are compiled together does.
  let members = joinMembers(dir, categoryOfDir(dir), overwrite = false)
  if not overwrite and members.len >= MinJoinGroup:
    joinedTest c, dir, members, overwrite, forward
  else:
    for f in members: testFile c, f, overwrite, Normal, forward
  if c.failures > 0:
    echo "FAILURE: ", c.failures, " of ", c.total, " tests in ", dir, " failed."
    quit min(c.failures, 125)

# The general test-tree runner is `collectTests`/`walkRoots` further below; the
# old per-suite `nimonytests`/`exampletests` drivers folded into it.

when defined(linux):
  var nativeElfShapeChecked = false
    ## The shape check below is an invariant of nifasm's ELF writer, not of any one
    ## test, so it runs once per suite — on the first native binary produced.

  proc u16(s: string; at: int): int =
    int(uint8(s[at])) or (int(uint8(s[at+1])) shl 8)

  proc u32(s: string; at: int): int =
    var r = 0
    for i in countdown(3, 0): r = (r shl 8) or int(uint8(s[at+i]))
    result = r

  proc u64(s: string; at: int): uint64 =
    result = 0'u64
    for i in countdown(7, 0): result = (result shl 8) or uint64(uint8(s[at+i]))

  proc checkNativeElfShape(c: var TestCounters; file, exe: string) =
    ## The three properties that make a nifasm image DEBUGGABLE, asserted against the
    ## bytes rather than against the writer that produced them.
    ##
    ## All three were absent once, and the way they failed is why this is a test:
    ## nothing crashed and no test went red. The image merged code and data into one
    ## R+W+X segment, which valgrind's ELF reader — it classifies a mapping as the
    ## text map only when it is R+X and NOT writable — declined to read at all, so
    ## every frame of every report came back as `???` and valgrind looked like it
    ## simply had nothing to say about native binaries. `.eh_frame` sat outside every
    ## PT_LOAD, which valgrind treats as a FATAL debug-info error, taking `.symtab`
    ## down with it. A regression here costs no test and one very confusing week.
    if nativeElfShapeChecked: return
    nativeElfShapeChecked = true
    var img = ""
    try: img = readFile(exe)
    except IOError, OSError: return
    if img.len < 64 or not img.startsWith("\x7FELF"): return
    if uint8(img[4]) != 2'u8 or uint8(img[5]) != 1'u8: return  # ELF64 little-endian only

    const PtLoad = 1
    const (PfX, PfW, PfR) = (1, 2, 4)
    let phoff = int(u64(img, 32))
    let phentsize = u16(img, 54)
    let phnum = u16(img, 56)
    var loads: seq[(uint64, uint64, int)] = @[]   # vaddr, memsz, flags
    var rxText = false
    for i in 0 ..< phnum:
      let at = phoff + i * phentsize
      if at + phentsize > img.len: return
      if u32(img, at) != PtLoad: continue
      let flags = u32(img, at + 4)
      let vaddr = u64(img, at + 16)
      let memsz = u64(img, at + 40)
      if memsz == 0: continue
      loads.add (vaddr, memsz, flags)
      if (flags and PfW) != 0 and (flags and PfX) != 0:
        failure c, file, "no writable+executable PT_LOAD (W^X)",
          "segment at 0x" & toHex(vaddr, 8) & " is R+W+X"
        return
      if (flags and PfX) != 0 and (flags and PfW) == 0 and (flags and PfR) != 0:
        rxText = true
    if not rxText:
      failure c, file, "a readable+executable, non-writable PT_LOAD",
        "none of the " & $loads.len & " PT_LOADs is R+X and not writable"
      return

    # `.eh_frame` must be SHF_ALLOC and land inside one of those PT_LOADs.
    const ShfAlloc = 2'u64
    let shoff = int(u64(img, 40))
    let shentsize = u16(img, 58)
    let shnum = u16(img, 60)
    let shstrndx = u16(img, 62)
    if shoff == 0 or shnum == 0: return          # stripped: nothing to check
    let strBase = int(u64(img, shoff + shstrndx * shentsize + 24))
    for i in 0 ..< shnum:
      let at = shoff + i * shentsize
      if at + shentsize > img.len: return
      let nameOff = strBase + u32(img, at)
      if nameOff >= img.len: return
      var name = ""
      var k = nameOff
      while k < img.len and img[k] != '\0': name.add img[k]; inc k
      if name != ".eh_frame": continue
      let shFlags = u64(img, at + 8)
      let shAddr = u64(img, at + 16)
      let shSize = u64(img, at + 32)
      if shSize == 0: return                     # built with `--no-debug-info`
      if (shFlags and ShfAlloc) == 0 or shAddr == 0:
        failure c, file, ".eh_frame is SHF_ALLOC with a real sh_addr",
          "sh_flags=0x" & toHex(shFlags, 4) & " sh_addr=0x" & toHex(shAddr, 8)
        return
      for (vaddr, memsz, _) in loads:
        if shAddr >= vaddr and shAddr + shSize <= vaddr + memsz: return
      failure c, file, ".eh_frame inside a PT_LOAD",
        "sh_addr=0x" & toHex(shAddr, 8) & " is in none of the loaded segments"
      return

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
  when defined(linux):
    checkNativeElfShape c, file, exe
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

proc nativeValgrindTestFile(c: var TestCounters; file: string) =
  ## One native test built with `-d:valgrind` and run under memcheck.
  ##
  ## Two things are asserted, and the second is the one worth the runtime: that
  ## the program still prints what it should, and that memcheck has NOTHING to
  ## say about it. The heap instrumentation lives in the allocator, so a mistake
  ## in it is not a wrong answer anywhere — it is a false report, arriving in
  ## whatever unrelated program someone was debugging, looking exactly like the
  ## bug they were hunting. The only way to know the instrumentation is honest is
  ## to run a corpus that is known good and demand silence.
  let msgs = file.changeFileExt(".msgs")
  if msgs.fileExists() and readFile(msgs).contains(ErrorKeyword):
    return
  inc c.total
  let (compilerOutput, compilerExitCode) = execNimonyNative("-d:valgrind " & quoteShell(file))
  if compilerExitCode != 0:
    failure c, file, "native -d:valgrind compiler exitcode 0",
      removeMakeErrors(compilerOutput) & "\nexitcode " & $compilerExitCode
    return
  let exe = file.generatedExeFile()
  if not exe.fileExists():
    failure c, file, "native executable", "missing: " & exe
    return
  # `-q` so only real findings appear, and `--error-exitcode` so the verdict is
  # the exit code rather than something to parse out of the log.
  let (vgOutput, vgExitCode) = osproc.execCmdEx(
      "valgrind --error-exitcode=99 --leak-check=no -q " & quoteShell(exe))
  if vgExitCode == 99:
    failure c, file, "no valgrind findings", vgOutput
    return
  let output = file.changeFileExt(".output")
  if output.fileExists():
    let outputSpec = readFile(output).strip
    if outputSpec != vgOutput.strip:
      failure c, file, outputSpec, vgOutput

const NativeValgrindSkip: array[0, string] = [
  # Empty. A test belongs here when it runs correctly under `hastur native` but
  # memcheck reports a real defect in it that is NOT the allocator
  # instrumentation's doing — with the reason spelled out, as
  # `tclosure_iter_string` had before its two causes were fixed (`allocFrame`
  # now uses `alloc0`, and the coroutine frame's `oconstr` is total).
]

proc nativeValgrindTests() =
  ## The native regression set again, this time with the allocator telling
  ## valgrind what it is doing (`lib/std/system/valgrind.nim`). Same corpus as
  ## `nativetests`, so what it adds is purely the memcheck verdict.
  if not hasValgrind:
    echo "0 / 0 native valgrind tests (valgrind not installed)"
    return
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  proc slashed(p: string): string = p.replace('\\', '/')
  var skip = initHashSet[string]()
  for f in NativeTestSkip: skip.incl slashed(f.addFileExt(".nim"))
  for f in NativeValgrindSkip: skip.incl slashed(f.addFileExt(".nim"))
  var dirs: seq[string] = @NativeTestDirs
  when defined(windows): dirs.add @NativeTestDirsWindows
  for dir in dirs:
    var files: seq[string] = @[]
    for x in walkDir(dir):
      if x.kind == pcFile and x.path.endsWith(".nim") and
         not isGeneratedTestFile(x.path) and slashed(x.path) notin skip:
        files.add x.path
    sort files
    for f in files: nativeValgrindTestFile c, f
  for f in NativeTestFiles:
    if slashed(f.addFileExt(".nim")) in skip: continue
    nativeValgrindTestFile c, f.addFileExt(".nim")
  reportFailures c
  echo c.total - c.failures, " / ", c.total, " native valgrind tests successful in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some native valgrind tests failed."

proc nativetests(overwrite: bool) =
  ## Run the native-backend regression set (`NativeTestDirs` + `NativeTestFiles`,
  ## plus `NativeTestDirsWindows` on Windows, minus `NativeTestSkip`) through
  ## `nimony n`. Requires the sibling `../nativenif` checkout (arkham/nifasm).
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  # `walkDir` joins with the platform separator onto a forward-slashed constant, so
  # its paths come back mixed (`tests/nimony/stdlib\tbitops.nim` on Windows) and can
  # only be compared to the skip list once both sides are spelled one way.
  proc slashed(p: string): string = p.replace('\\', '/')
  var skip = initHashSet[string]()
  for f in NativeTestSkip: skip.incl slashed(f.addFileExt(".nim"))
  var dirs: seq[string] = @NativeTestDirs
  when defined(windows): dirs.add @NativeTestDirsWindows
  for dir in dirs:
    var files: seq[string] = @[]
    for x in walkDir(dir):
      # `_hastur_joined.nim` is the C-backend runner's own build artifact, left
      # in the tree by an earlier `hastur tests/…`. Walking it in as a test made
      # the native result depend on whether that run happened, and reported the
      # joined program's arkham gaps against whichever test was added last.
      if x.kind == pcFile and x.path.endsWith(".nim") and
         not isGeneratedTestFile(x.path) and slashed(x.path) notin skip:
        files.add x.path
    sort files
    for f in files: nativeTestFile c, f, overwrite
  for f in NativeTestFiles:
    nativeTestFile c, f.addFileExt(".nim"), overwrite
  reportFailures c
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
  reportFailures c
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
  reportFailures c
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
  reportFailures c
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

var bootRelease = true
  ## `boot` compiles the bootstrapped toolchain with `-d:release` by default:
  ## it has proven to be the wider test. It implies `--opt:speed` (so the
  ## shoggoth tree optimizer and the inter-module inliner run) *and* defines
  ## `release`, which exercises the `when defined(release)` paths in nimony,
  ## nimsem and hexer themselves — a whole class of bugs a debug boot cannot
  ## reach. `--no-release` boots at the default opt level instead.
var bootNative = false
  ## Compile the stages with the C-free native backend (`nimony n` → arkham +
  ## nifasm)? Decided by `useNativeBoot` at the start of `bootCmd`.

type BootBackend = enum
  bbAuto,   ## whatever `useNativeBoot` says for this host — the default
  bbC,      ## force `nimony c`
  bbNative  ## force `nimony n`, and fail loudly when it is not available

var bootBackend = bbAuto
  ## `--boot-backend:`. Exists so the two paths can be MEASURED against each
  ## other on a host where the native one is automatic: without it, timing the
  ## C boot on linux/amd64 means hiding `bin/arkham` from hastur.
var debugBuild = false
var nativeToolsDebug = false

proc nimcPrefix(): string =
  ## The front-end tools ship OPTIMIZED by default, exactly like arkham and
  ## nifasm (see `nativeToolPrefix`). A debug toolchain is ~8x slower at
  ## compiling (measured on `nimony n bug.nim`: nimsem 0.155s release vs
  ## 1.204s debug, hexer 0.065s vs 0.676s), which dominates every test run —
  ## and because the suites rebuild the toolchain via `setup.hastur`, an
  ## unoptimized default silently downgraded whatever the user had built
  ## before. `--debug` opts out for gdb work on the tools themselves.
  #
  # `--warningAsError:ProveInit:off` and `--warningAsError:Uninit:off`:
  # Nimony's `src/config.nims` promotes these warnings to errors, but Nim
  # 2.2.10's host stdlib (typedthreads.nim, deques.nim) trips them on
  # patterns that aren't actionable from our side. Without these overrides,
  # any tool using `createThread` (hastur itself) or `initDeque` (pnak)
  # fails to build on Nim 2.2.10.
  (if debugBuild: "nim c " else: "nim c -d:release ") &
    "--warningAsError:ProveInit:off --warningAsError:Uninit:off "

proc nativeToolPrefix(): string =
  ## arkham + nifasm are the native backend's HOT codegen tools: they run
  ## once per module on every `nimony n` build, so unlike the other tools they
  ## ship OPTIMIZED by default. A debug build roughly doubles `nimony n` cold
  ## time (measured: nimsem 50.7s debug → 39.5s release) because — unlike the C
  ## backend, whose heavy lifting is gcc (always optimized) — the native
  ## backend's heavy lifting IS these two tools. Pass `--native-debug` to hastur
  ## to build them unoptimized instead (for `-d:arkhamDbgSym` / gdb work on the
  ## toolchain itself).
  (if nativeToolsDebug: "nim c " else: "nim c -d:release ") &
    "--warningAsError:ProveInit:off --warningAsError:Uninit:off "

# ---- the pinned `nativenif` dependency -------------------------------------
# arkham and nifasm are built from a SIBLING repo, so "which nativenif" is an
# input to every native build the same way a source file is — and until this
# pin existed it was whatever each machine happened to have checked out. That
# is not a hypothetical: a native-suite failure could mean a real gap, a
# nativenif commit newer than the one the tests were recorded against, or a
# local branch someone was mid-way through. `src/nativenif.commit` makes it one
# answer, recorded in this repo and moved deliberately by `hastur update deps`.

const
  NativenifDir = "../nativenif"
    ## Sibling checkout arkham + nifasm live in. Their committed `nim.cfg`s
    ## reach back here through `../../../nimony/src/lib`, so the layout — and
    ## this repo's directory name — is already load-bearing.
  NativenifCommitFile = "src/nativenif.commit"
    ## Under `src/` with the rest of what a build is made of — and so a change to
    ## it invalidates the CI artifact cache, whose key hashes `src/**`. Read
    ## relative to the project root, which is where every hastur command runs.

proc pinnedNativenifCommit(): string =
  ## The pin is read at RUNTIME, not compiled in: a `slurp`ed copy would let the
  ## file and the binary disagree, so `git checkout <pin>` and `hastur update
  ## deps` would go on enforcing and reporting whatever commit the binary was
  ## last built from — silently, and worst on a stale `bin/hastur`, which is the
  ## exact failure mode this pin exists to end. Missing or empty file means "no
  ## pin": build whatever is checked out, the escape hatch for bisecting
  ## nativenif itself.
  if fileExists(NativenifCommitFile): readFile(NativenifCommitFile).strip
  else: ""

proc gitIn(dir, args: string): (string, int) =
  execCmdEx("git -C " & dir.quoteShell & " " & args)

var nativenifChecked = false

proc syncNativenif() =
  ## Put `../nativenif` on the pinned commit before anything is built from it.
  ##
  ## Never at the cost of uncommitted work: a dirty tree is left alone with a
  ## warning, because the person who dirtied it is precisely the person who
  ## wants arkham built from THEIR edits. A clean tree gets detached onto the
  ## pin (the branch that was there is still a `git switch -` away).
  if nativenifChecked: return
  nativenifChecked = true
  let pin = pinnedNativenifCommit()
  if pin.len == 0: return
  if not dirExists(NativenifDir):
    quit "hastur: " & NativenifDir & " not found; clone nim-lang/nativenif " &
         "next to this repo (it holds arkham + nifasm)"
  let (headOut, headCode) = gitIn(NativenifDir, "rev-parse HEAD")
  if headCode != 0:
    echo "[deps] ", NativenifDir, " is not a git checkout; building it as it is"
    return
  let head = headOut.strip
  if head == pin: return

  let (dirty, dirtyCode) = gitIn(NativenifDir, "status --porcelain --untracked-files=no")
  if dirtyCode != 0 or dirty.strip.len > 0:
    echo "[deps] WARNING: ", NativenifDir, " has uncommitted changes — leaving it at ",
         head
    echo "[deps] WARNING: arkham/nifasm are built from YOUR tree, not from the ",
         NativenifCommitFile, " pin (", pin, ")"
    return

  # A checkout sitting on a BRANCH is someone's working tree, and committing a
  # fix there is exactly what makes it stop being dirty — so the guard above
  # would hand it straight to the `git checkout` below and build the arkham that
  # fix was replacing. Leave it be and say so. CI is the one place that must
  # enforce the pin regardless: its checkout is on the default branch too, and a
  # pin deliberately BEHIND that branch is the whole point of pinning.
  let (branch, branchCode) = gitIn(NativenifDir, "symbolic-ref --quiet --short HEAD")
  if branchCode == 0 and branch.strip.len > 0 and getEnv("CI").len == 0:
    echo "[deps] WARNING: ", NativenifDir, " is on branch '", branch.strip, "' at ", head
    echo "[deps] WARNING: leaving it there; ", NativenifCommitFile, " pins ", pin
    echo "[deps] to build the pin instead: git -C ", NativenifDir,
         " checkout --detach ", pin
    return

  # A checkout that never had the pin — a shallow CI clone of the default
  # branch, or a tree that has not fetched in a while. Ask for the commit by
  # name first: github.com serves any SHA reachable from a ref, which is the
  # cheapest way to get exactly the one object we are after.
  if gitIn(NativenifDir, "cat-file -e " & pin & "^{commit}")[1] != 0:
    echo "[deps] fetching ", pin, " into ", NativenifDir
    if gitIn(NativenifDir, "fetch --no-tags origin " & pin)[1] != 0:
      # A server that refuses the object by name (git's default for anything
      # but github.com: `uploadpack.allowReachableSHA1InWant`) still serves the
      # refs, but on a shallow clone their history stops above the pin — so
      # deepen, or fetching the branch tips would not bring it either.
      let shallow = gitIn(NativenifDir, "rev-parse --is-shallow-repository")[0].strip
      discard gitIn(NativenifDir,
                    if shallow == "true": "fetch --no-tags --unshallow origin"
                    else: "fetch --no-tags origin")
    let (missing, missingCode) = gitIn(NativenifDir, "cat-file -e " & pin & "^{commit}")
    if missingCode != 0:
      quit "hastur: " & NativenifCommitFile & " pins " & pin &
           ", which " & NativenifDir & " cannot fetch:\n" & missing &
           "\nIs the commit pushed? `hastur update deps` re-pins to the local HEAD."

  echo "[deps] nativenif: ", head, " -> ", pin, " (pinned by ", NativenifCommitFile, ")"
  let (coOut, coCode) = gitIn(NativenifDir, "checkout --detach --quiet " & pin)
  if coCode != 0:
    quit "hastur: cannot check out " & pin & " in " & NativenifDir & ":\n" & coOut

proc updateDepsCmd() =
  ## `hastur update deps`: re-pin `src/nativenif.commit` to whatever `../nativenif`
  ## has checked out right now. The one sanctioned way to move the pin, so that
  ## "the native backend needs a newer arkham" is a reviewable one-line diff
  ## next to the test results that needed it.
  if not dirExists(NativenifDir):
    quit "hastur update deps: " & NativenifDir & " not found"
  let (headOut, headCode) = gitIn(NativenifDir, "rev-parse HEAD")
  if headCode != 0:
    quit "hastur update deps: " & NativenifDir & " is not a git checkout"
  let head = headOut.strip

  let (dirty, _) = gitIn(NativenifDir, "status --porcelain --untracked-files=no")
  if dirty.strip.len > 0:
    echo "[deps] WARNING: ", NativenifDir, " has uncommitted changes; ", head,
         " does NOT include them"
  let (remotes, remotesCode) = gitIn(NativenifDir, "branch --remotes --contains " & head)
  if remotesCode != 0 or remotes.strip.len == 0:
    echo "[deps] WARNING: ", head, " is on no remote branch — push it, ",
         "or CI cannot fetch the pin"

  let pin = pinnedNativenifCommit()
  if head == pin:
    echo "[deps] ", NativenifCommitFile, " already pins ", head
    return
  writeFile(NativenifCommitFile, head & "\n")
  echo "[deps] ", NativenifCommitFile, ": ",
       (if pin.len > 0: pin else: "(unpinned)"), " -> ", head

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
  ## auto-clone is a later step) and put it on the `src/nativenif.commit` pin
  ## first. arkham's own `nim.cfg` already sets `--outdir:bin`; we pass it
  ## explicitly so the result is deterministic regardless of the current
  ## directory.
  syncNativenif()
  exec nativeToolPrefix() & "--outdir:" & binDir() & " " & NativenifDir &
       "/src/arkham/arkham.nim", showProgress

proc buildNifasm(showProgress = false) =
  ## `nifasm` (asm-NIF -> static, libc-free ELF/Mach-O/PE executable; also the
  ## linker) — sibling repo, same assume-exists arrangement as `buildArkham`.
  syncNativenif()
  exec nativeToolPrefix() & "--outdir:" & binDir() & " " & NativenifDir &
       "/src/nifasm/nifasm.nim", showProgress

proc buildNativeTools(showProgress = false) =
  ## arkham + nifasm for `build all`. They are part of the toolchain now — a
  ## native boot and every `nimony n` need them in `bin/`, and leaving them to a
  ## separate opt-in command meant each caller had to remember (CI did not, and
  ## the Windows bootstrap of #2325 quietly kept using the C backend for it).
  ##
  ## Two hosts do not get them, and both say so instead of failing the build:
  ## there is no native target for a 32-bit host, and the sibling checkout is
  ## something a plain `git clone` of this repo does not bring. Asking for them
  ## by name (`build native`, `build arkham`, `hastur native`) still goes
  ## straight at `buildArkham`, which quits when the sibling is missing — an
  ## explicit request deserves an error, a blanket `all` does not.
  when defined(cpu64):
    if dirExists(NativenifDir):
      buildArkham(showProgress)
      buildNifasm(showProgress)
    else:
      echo "[build] ", NativenifDir, " not found — skipping arkham + nifasm; ",
           "`nimony n` and a native boot need it (clone nim-lang/nativenif next to this repo)"
  else:
    echo "[build] no native backend target for this host — skipping arkham + nifasm"

proc buildHexer*(showProgress = false) =
  exec nimcPrefix() & "src/hexer/hexer.nim", showProgress
  let exe = "hexer".addFileExt(ExeExt)
  robustMoveFile "src/hexer/" & exe, binDir() / exe

proc buildNifmake(showProgress = false) =
  exec nimcPrefix() & "src/nifmake/nifmake.nim", showProgress
  let exe = "nifmake".addFileExt(ExeExt)
  robustMoveFile "src/nifmake/" & exe, binDir() / exe

proc buildNifbench*(showProgress = false) =
  ## The host-Nim build of `nifbench`, which is the BASELINE column: the point
  ## of the tool is to compile the same source with `nim c`, `nimony c` and
  ## `nimony n` and diff the per-phase timings, so this one has to exist for
  ## the other two to mean anything. Kept out of `all` — it is a measuring
  ## instrument, not part of the toolchain.
  exec nimcPrefix() & "src/nifbench/nifbench.nim", showProgress
  let exe = "nifbench".addFileExt(ExeExt)
  robustMoveFile "src/nifbench/" & exe, binDir() / exe

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

  # The Leng backend's own driver: a separate DAG tip (nothing in the nimony
  # front end imports it), covering both the C and the LLVM code generators
  # plus `lib/foreignmodules` and `lib/nifcdecl` via its import set.
  "src/lengc/lengc.nim",
]

# Modules whose `isMainModule` block should also be executed after compilation.
const RunnableBootstrapModules = [
  "src/lib/bitabs.nim",
  "src/lib/argsfinder.nim",
]

const BootNativeTools = ["arkham", "nifasm"]
  ## Carried too for a native boot: `nimony n` reaches them through `findTool`,
  ## i.e. relative to the running nimony, so without them in `binN/` stage N
  ## would silently drive `bin/`'s copies.

proc missingNativeTools(): seq[string] =
  ## Which of `BootNativeTools` are not in `bin/`. They come from the sibling
  ## `../nativenif`, so a checkout that never had it has neither.
  result = @[]
  for tool in BootNativeTools:
    if not fileExists(binDir() / tool.addFileExt(ExeExt)): result.add tool

proc firstDiagnostic(output: string): string =
  ## The one line of a failed compile worth grouping failures by. Prefer a real
  ## diagnostic over the `FAILURE: <nifmake command line>` trailer, which names a
  ## per-module cache path and so is different for every module that shares a
  ## cause. Then drop the position tail arkham and nifasm append — ` at ??? (…)`
  ## and ` in proc <sym>` — for the same reason: one arkham assertion reached by
  ## six modules is one gap, and six spellings of it is a work list that lies
  ## about its own length.
  result = ""
  for line in output.splitLines():
    let s = line.strip()
    if s.len == 0: continue
    if "Error" in s or "error" in s or "AssertionDefect" in s or s.startsWith("[Error]"):
      result = s
      break
    if result.len == 0 and not s.startsWith("FAILURE:"): result = s
  if result.len == 0: result = "(no diagnostic)"
  let at = result.find(" at ???")
  if at >= 0: result.setLen at
  let inProc = result.find(" in proc ")
  if inProc >= 0: result.setLen inProc

proc tierTests(native = false; extraArgs = "") =
  ## Compile every module on `BootstrapModules` with `bin/nimony`. Fails
  ## fast on the first regression so the offending module is obvious. On
  ## Windows the list collapses to the Tier 20 tip (`nimony.nim`) — its
  ## import set already covers every other entry, so the redundant per-leaf
  ## compiles are pure CI cost; Linux/macOS still cover every leaf.
  ##
  ## `native` drives `nimony n` (arkham + nifasm) rather than `nimony c`, which
  ## is what makes this list the ladder for bringing a new native target up.
  ## `boot` compiles three whole tools at once and reports the first of them that
  ## died, so a target with several independent gaps shows up as one opaque
  ## "stage 1 (nimsem) failed"; the tier list is a walk of the same DAG from the
  ## leaves upwards, so each gap is attributed to the SMALLEST module that
  ## reaches it and the ones that are already fine are named as such. Every
  ## module is attempted — no fail-fast — because the point of a bring-up run is
  ## the whole work list, not its first entry. Windows keeps its collapse to the
  ## tip either way: it is a CI cost decision, and a tip-only native run still
  ## reports every gap, just without attributing them.
  let nimony = binDir() / "nimony".addFileExt(ExeExt)
  if not fileExists(nimony):
    quit "bootstrap: " & nimony & " not found; run `hastur build nimony` first"
  if native:
    let missing = missingNativeTools()
    if missing.len > 0:
      quit "tiers native: " & missing.join(", ") & " not in " & binDir() &
           "; `hastur build native` builds them from " & NativenifDir
  let modules =
    when defined(windows): @["src/nimony/nimony.nim"]
    else: @BootstrapModules
  let backend = if native: "n" else: "c"
  let t0 = epochTime()
  var failed: seq[string] = @[]
  # Keyed by the first diagnostic line, so N modules tripping one arkham
  # assertion read as one gap with N witnesses rather than N separate failures.
  var gaps: seq[(string, seq[string])] = @[]
  for m in modules:
    removeDir "nimcache"
    var cmd = nimony.quoteShell & " " & backend
    if m in RunnableBootstrapModules: cmd.add " -r"
    if extraArgs.len > 0: cmd.add " " & extraArgs
    cmd.add " " & m.quoteShell
    let (output, ec) = execCmdEx(cmd)
    if ec == 0:
      echo "OK   ", m
    else:
      echo "FAIL ", m
      echo output
      failed.add m
      let first = firstDiagnostic(output)
      var seen = false
      for i in 0 ..< gaps.len:
        if gaps[i][0] == first:
          gaps[i][1].add m
          seen = true
          break
      if not seen: gaps.add (first, @[m])
  echo failed.len, " / ", modules.len, " bootstrap regressions in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if gaps.len > 0:
    # The work list: one line per distinct diagnostic, with the modules that
    # reach it. Fixing the top entry is what moves the most tiers at once.
    echo "distinct failure modes (", gaps.len, "):"
    for (diag, mods) in gaps:
      echo "  * ", diag
      echo "    reached by: ", mods.join(", ")
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

proc asmNifTarget(asmFile: string): string =
  ## The target arkham compiled for, read off the `(arch …)` its asm-NIF opens
  ## with (`x64`, `win_x64`, `arm64`, `linux_arm64`). Taken from the OUTPUT rather
  ## than re-derived from `hostOS`/`hostCPU` so the golden can never be filed under
  ## a target the file was not actually generated for.
  let s = readFile(asmFile)
  let i = s.find("(arch ")
  if i < 0: return ""
  let j = s.find(')', i)
  if j < 0: return ""
  result = s[i + len("(arch ") .. j - 1].strip

proc runNativeCodegenTests*(dir: string; overwrite: bool) =
  ## Custom runner for `tests/nativecg`: a golden suite over the C-free native
  ## backend's *emitted machine code*. For each `.nim` it
  ##   1. compiles with `nimony n --opt:speed` (so the shoggoth inliner/optimizer
  ##      that feeds the native path actually runs),
  ##   2. goldens arkham's `<main>.asm.nif` (the typed assembler NIF) against a
  ##      checked-in `<test>.<target>.asm.nif`, and
  ##   3. runs the linked libc-free executable, checking `.output` / `.exitcode`.
  ##
  ## The asm-NIF is byte-stable for a fixed *relative* test path — module
  ## suffixes are derived from the relative path, and a module's own symbols
  ## carry no suffix — so the golden is portable across checkouts/machines as
  ## long as hastur is invoked from the repo root (which it always is). No
  ## normalization is needed.
  ##
  ## It is NOT portable across TARGETS, though — machine code is the point of the
  ## suite. So the golden is per-target (`tinlinecond.x64.asm.nif`,
  ## `tinlinecond.win_x64.asm.nif`, …), named for the `(arch …)` arkham actually
  ## emitted. A target with no checked-in golden yet fails as a missing file;
  ## `--overwrite` writes it, which is how a new one is added.
  ##
  ## Requires the sibling `../nativenif` checkout (arkham/nifasm), exactly like
  ## the `native` subcommand; the directory is `hastur.mode = skip` so the
  ## default `all` sweep leaves this opt-in.
  if not skipBuild:
    buildNimony()
    buildHexer()
    buildShoggoth()
    buildArkham()
    buildNifasm()
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  var files: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim") and
       x.path.extractFilename != "setup.nim":   # the runner itself, not a test
      files.add x.path
  sort files
  for file in files:
    let msgs = file.changeFileExt(".msgs")
    if msgs.fileExists() and readFile(msgs).contains(ErrorKeyword):
      continue                              # negative test: not a codegen case
    inc c.total
    let cacheArg =
      if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
      else: ""
    let (compilerOutput, compilerExitCode) =
      execLocal("nimony",
        "n --opt:speed --silentMake --isMain " & cacheArg & quoteShell(file))
    if compilerExitCode != 0:
      failure c, file, "native compiler exitcode 0",
        removeMakeErrors(compilerOutput) & "\nexitcode " & $compilerExitCode
      continue
    # 1) Golden arkham's assembler NIF for the main module.
    let asmFile = generatedFile(file, ".asm.nif")
    if not asmFile.fileExists():
      failure c, file, "arkham asm.nif", "missing: " & asmFile
      continue
    let target = asmNifTarget(asmFile)
    if target.len == 0:
      failure c, file, "arkham asm.nif with an (arch …)", "no target in: " & asmFile
      continue
    diffFiles(c, file, file.changeFileExt(target & ".asm.nif"), asmFile, overwrite)
    # 2) Behavioural check: the linked ELF must run and match .output/.exitcode.
    let exe = generatedExeFile(file)
    if not exe.fileExists():
      failure c, file, "native executable", "missing: " & exe
      continue
    let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell exe)
    var output = file.changeFileExt(".output")
    if testProgramExitCode != 0:
      output = file.changeFileExt(".exitcode")
      if not output.fileExists():
        failure c, file, "test program exitcode 0",
          "exitcode " & $testProgramExitCode & "\n" & testProgramOutput
        continue
    if output.fileExists():
      let outputSpec = readFile(output).strip
      if outputSpec != testProgramOutput.strip:
        if overwrite:
          writeFile(output, testProgramOutput)
        failure c, file, outputSpec, testProgramOutput
  reportFailures c
  echo c.total - c.failures, " / ", c.total,
    " native-codegen tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some native-codegen tests failed."
  else:
    echo "SUCCESS."

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

proc bootCarryTools(): seq[string] =
  result = @BootCarryTools
  if bootNative: result.add BootNativeTools

const NativeBootReady = true
  ## Requires a nativenif checkout whose arkham holds each emit step's demand
  ## inside the transient-register budget (the `semBodyCheckBody` staging
  ## exhaustion: binFold's pick-before-premat, emitCondValue2's early result
  ## hold, and a staging pick blind to a free temp pool). When stage 1 dies
  ## with "no staging register available", rebuild arkham/nifasm from a
  ## nativenif master that carries those fixes — or flip this back to `false`
  ## and the C-backend boot below is the whole self-host gate meanwhile.

proc useNativeBoot(): bool =
  ## linux/amd64 is the platform the native backend is complete on: x86-64
  ## codegen plus the Linux syscall table the libc-free stdlib runs on. Its
  ## tools come from the sibling `../nativenif` checkout, which `build all`
  ## builds when it is there — so fall back to the C backend when they're
  ## missing, and say so (`bootBackendLine`) rather than let a missing sibling
  ## look like a slow C boot.
  ##
  ## linux/arm64 is IN, as of 2026-08-22. `hastur tiers native --forward:-d:release`
  ## is 27/27 and `--boot-backend:native` reaches a byte-identical
  ## stage1==stage2==stage3 fixed point in ~405s, against ~650s through the C backend.
  ##
  ## What had held it back was never the syscall/ABI layer. In order of discovery, and
  ## each one attributed by the tier ladder rather than by staring at a boot stage:
  ## arkham's ">8 integer params (stack TODO)"; a stack-marshalled by-value aggregate
  ## whose home is a register PAIR, which has no address to marshal from; a spilled
  ## `nil` declared `(s) (nil)` where the slot is storage and wants `(ptr void)`; a
  ## word-sized set constant `desugar` emitted unsuffixed, so `xelim` typed its temp
  ## `u64` and the native backend rejected the narrowing the C one performs silently;
  ## the emitter's last-resort register draws handing out live PARAMETER homes, which
  ## are never `rb`-bound and so were invisible to the liveness test they use; and
  ## finally nifasm encoding an element STRIDE into AArch64's register-offset scale
  ## bit, which can only mean "the access width" — `(dot (at arrayOfTuples i) fld)`
  ## read `arr[i div 2]`.
  ##
  ## Only the last two needed `-d:release`, and only because shoggoth's inter-module
  ## inliner supplies the register pressure that reaches them; neither was the
  ## inliner's fault. `SHOGGOTH_DISABLE=<pass,…>` is what separated those questions.
  ##
  ## windows/amd64 is in as of #2325: arkham's win_x64 target emits a PE that
  ## imports what it needs per dll, so a native boot there needs no MinGW and no
  ## libc — which is what makes the Windows job the slowest in the matrix (601s
  ## of stages against 61s natively).
  when defined(linux) and (defined(amd64) or defined(arm64)):
    result = NativeBootReady and missingNativeTools().len == 0
  elif defined(windows) and defined(amd64):
    result = NativeBootReady and missingNativeTools().len == 0
  else:
    result = false

proc bootBackendLine(withValgrind: bool): string =
  ## One line naming the backend `boot` is about to use, and — when it is not the
  ## native one — why. The fallback used to be silent, which reads as "the native
  ## path is enabled and slow" instead of "the native path was never reached":
  ## #2325 turned windows/amd64 on and the boot went on emitting `nimony c`
  ## because nothing had built arkham into `bin/`.
  if bootNative:
    return "[boot] backend: native (`nimony n`: arkham + nifasm)" &
           (if bootBackend == bbNative: " — forced by --boot-backend:native" else: "")
  result = "[boot] backend: C (`nimony c`)"
  if bootBackend == bbC:
    return result & " — forced by --boot-backend:c"
  when (defined(linux) and (defined(amd64) or defined(arm64))) or
       (defined(windows) and defined(amd64)):
    if withValgrind:
      result.add " — --valgrind cannot see the native heap"
    elif not NativeBootReady:
      result.add " — NativeBootReady is off"
    else:
      let missing = missingNativeTools()
      if missing.len > 0:
        result.add " — " & missing.join(", ") & " missing from " & binDir() &
                   "; `hastur build all` builds them when " & NativenifDir &
                   " is checked out"
  else:
    result.add " — no native backend target for this host"

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
  for aux in bootCarryTools():
    carryAuxTool(result, aux)

proc provisionStageZero(): string =
  ## Seed `bin0/` with a fresh copy of the host-Nim-built toolchain in
  ## `bin/`. Unlike later stages, the self-tools aren't recompiled here —
  ## they're copied across so that `bin1/` has a complete driver to start
  ## from.
  result = bootStageDir(0)
  removeDir result
  createDir result
  for aux in bootCarryTools():
    carryAuxTool(result, aux)
  for tool in BootSelfTools:
    carryAuxTool(result, tool)

proc bootToolCmd(compiler, source, outBin, cacheBase, args: string;
                 withValgrind: bool): string =
  ## The `compiler <c|n> --out:outBin source` command line for one boot tool.
  ## `compiler` is the previous stage's nimony, so it transitively drives the
  ## previous stage's nimsem/hexer (siblings under the same stage's bin dir)
  ## for this build. The subcommand is `n` for a native boot (arkham + nifasm,
  ## no C compiler), `c` otherwise.
  let cache = cacheBase / outBin.extractFilename
  removeDir cache
  createDir cache
  if fileExists(outBin): removeFile(outBin)
  result = compiler.quoteShell & (if bootNative: " n" else: " c") &
           " --silentMake --nimcache:" &
           cache.quoteShell & " --out:" & outBin.quoteShell
  if withValgrind:
    result.add " --passC:\"-DMI_TRACK_VALGRIND=1\""
  if args.len > 0:
    result.add ' '
    result.add args
  result.add ' '
  result.add source.quoteShell

proc compileBootStage(stage: int; cacheBase, args: string; withValgrind: bool):
                     string =
  ## Build stage `stage` of the toolchain. Returns the stage's bin
  ## directory. Driver of stage N is the stage-(N-1) nimony.
  ##
  ## The stage's three tools (nimsem, hexer, nimony) are compiled CONCURRENTLY.
  ## They are independent: each gets its own `--nimcache` and its own `--out`,
  ## and the only shared input — the previous stage's `bin/` — is read-only for
  ## the duration. Serially this was the single longest stretch of the Windows
  ## CI run (9 compiles, ~600s, all of it one core at a time while nifmake's
  ## `-j` had nothing left to overlap with across the tool boundary).
  let prev = bootStageDir(stage - 1)
  let prevNimony = prev / "nimony".addFileExt(ExeExt)
  if not fileExists(prevNimony):
    quit "boot: " & prevNimony & " not found (stage " & $(stage - 1) &
         " missing)"
  result = provisionStageBin(stage)

  var outBins: seq[string] = @[]
  var cmds: seq[string] = @[]
  for tool in BootSelfTools:
    let outBin = result / tool.addFileExt(ExeExt)
    outBins.add outBin
    cmds.add bootToolCmd(prevNimony, bootSourceFor(tool), outBin, cacheBase,
                         args, withValgrind)
    echo "[boot] stage ", stage, ": ", cmds[^1]

  let t0 = epochTime()
  var procs: seq[Process] = @[]
  for cmd in cmds:
    # `poEvalCommand` so the already-quoted command line goes through the
    # shell exactly as `execShellCmd` ran it. Output is inherited: the tools
    # are quiet under `--silentMake`, and interleaved progress from three
    # compiles is still more useful than buffering it all to the end.
    procs.add startProcess(cmd, options = {poEvalCommand, poParentStreams})
  var failed = ""
  for i in 0 ..< procs.len:
    let exitCode = waitForExit(procs[i])
    close procs[i]
    if exitCode != 0 and failed.len == 0:
      failed = outBins[i].extractFilename
  let dt = epochTime() - t0
  if failed.len > 0:
    quit "FAILURE: boot stage " & $stage & " (" & failed &
         ") failed after " & formatFloat(dt, ffDecimal, precision=2) & "s"
  for outBin in outBins:
    if not fileExists(outBin):
      quit "FAILURE: boot stage " & $stage & ": " & outBin &
           " was not produced (did `--out` get rejected?)"
  echo "[boot] stage ", stage, " produced ", BootSelfTools.join(", "), " in ",
       formatFloat(dt, ffDecimal, precision=2), "s"

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

proc specifiesOptLevel(args: string): bool =
  ## Does the caller already pick a build mode for the bootstrapped compiler?
  ## Then `boot` must not layer its `-d:release` default on top: `-d:danger`
  ## in particular is a deliberate *stronger* choice, and passing both would
  ## read as a contradiction on the command line.
  ##
  ## Matched without the leading dashes on purpose, so that every spelling
  ## counts: getopt strips them off a positional `-d:danger` (it reaches here as
  ## `d:danger` — which is what `--forward:` exists to avoid), while
  ## `--forward:-d:danger` keeps them.
  result = "d:release" in args or "d:danger" in args or "opt:" in args

proc bootCmd*(args: string; withValgrind: bool; release = true) =
  ## `release` ⇒ compile every stage with `-d:release` unless `args` already
  ## names a build mode. Defaulted here rather than at the dispatch site so
  ## `selfcheck`, which calls this directly, gets the same coverage.
  var args = args
  if release and not specifiesOptLevel(args):
    args = if args.len > 0: "-d:release " & args else: "-d:release"
  for tool in BootSelfTools:
    let exe = binDir() / tool.addFileExt(ExeExt)
    if not fileExists(exe):
      quit "boot: " & exe & " not found; run `hastur build all` first"
  # valgrind cannot see the native backend's static, libc-free `mmap` heap, so
  # `--valgrind` (and thus `selfcheck`) always boots through the C backend.
  # `--boot-backend:native` FORCES: the only thing that can stop it is not having
  # arkham and nifasm to run. It used to defer to `useNativeBoot` as well, which
  # made the flag useless on exactly the hosts it is needed on — a host where the
  # native path is off is a host where someone is trying to turn it ON, and
  # "not available here" told them nothing they could act on. `--valgrind` still
  # wins, because memcheck cannot see the native heap at all.
  if bootBackend == bbNative:
    if withValgrind:
      quit "boot: --boot-backend:native and --valgrind are exclusive: " &
           bootBackendLine(withValgrind)
    let missing = missingNativeTools()
    if missing.len > 0:
      quit "boot: --boot-backend:native needs " & missing.join(", ") & " in " &
           binDir() & "; `hastur build native` builds them from " & NativenifDir
    bootNative = true
  else:
    bootNative = not withValgrind and bootBackend != bbC and useNativeBoot()
  echo bootBackendLine(withValgrind)
  for tool in bootCarryTools():
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

  echo "[boot] compiling stages with: ",
       (if args.len > 0: args else: "(no extra flags)")
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
  ##      runtime UAFs) that single-module compiles miss. Boots at
  ##      `-d:release` (boot's default), so the shoggoth optimizer and the
  ##      `when defined(release)` paths are part of what this checks.
  ##
  ##      `--valgrind` keeps this on the C backend (see `bootCmd`); plain
  ##      `hastur boot` is the native self-host.
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
  reportFailures c
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
  reportFailures c
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
  ## `<dir>/setup.hastur` as a hastur subcommand before its tests. `--debug`
  ## is forwarded so a `--debug` run also builds the toolchain unoptimized
  ## (the child hastur wouldn't otherwise inherit it); without it the child
  ## builds release, same as the parent's default.
  if skipBuild: return
  let f = dir / "setup.hastur"
  if not fileExists(f): return
  let self = getAppFilename().quoteShell
  let relFlag = if debugBuild: " --debug" else: ""
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
    # The runner printed its own per-test detail; name the suite so the
    # top-level summary still points somewhere.
    noteFailure c, dir & " (setup.nim runner)"

type WalkPlan = object
  ## Accumulated during the tree walk so the run phase can drive ONE
  ## saturated parallel pool instead of one pool per directory. The old
  ## walk ran each leaf directory to completion before starting the next,
  ## so every directory boundary was a hard pool barrier (N-1 cores idle on
  ## its tail test) plus a fresh `warmupSharedCache` spawn. Flattening every
  ## parallel-safe file into a single queue pays warmup/prebuild once and
  ## keeps the pool full across the whole run — the win is largest where
  ## process spawns are expensive (Windows CI).
  parItems: seq[WorkItem]            # parallel-safe units of work, flattened
  serialDirs: seq[(string, Category)] # dirs that must run serially (see below)

proc collectTests(c: var TestCounters; plan: var WalkPlan; dir, forward: string;
                  overwrite, isRoot: bool) =
  # `--skip:` is honoured even for an explicit root: it says "not in this run",
  # so a caller that names both is asking for nothing rather than for a fight.
  if normalizeDirKey(dir) in skipDirs: return
  # `hastur.mode = skip` excludes a directory from the sweep, but only when the
  # walk *descends* into it — pointing hastur straight at it (isRoot) still
  # runs it. That's how a WIP/known-broken suite (e.g. dagon) stays out of the
  # default `all` run yet remains explicitly runnable via `hastur tests/dagon`.
  let cat = categoryOfDir(dir)
  if cat == Skip and not isRoot: return
  if fileExists(dir / "setup.nim"):
    # A `setup.nim` owns its subtree and runs its own tests right here — it is
    # a self-contained runner, not part of the shared file pool.
    runSetupNimDir(c, dir, forward, overwrite)
    return
  # `setup.hastur` prep (e.g. building the toolchain) must precede every test
  # in its subtree, so it runs during the walk, before the run phase kicks off.
  runSetupHastur(dir)
  var hasNim = false
  var subs: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim"): hasNim = true
    elif x.kind == pcDir: subs.add x.path
  if hasNim:
    # A grouping directory has no `.nim` files of its own, so a stray one
    # dropped into `tests/` demotes the whole tree to a "leaf" and silently
    # skips every suite below it — the run still says SUCCESS, just for 7
    # tests instead of 672. Nothing distinguishes the two roles except this:
    # a real leaf's subdirectories are import fixtures (`deps/`, `imp/`, …)
    # and never carry a runner marker. If one does, the `.nim` here is the
    # mistake, so say which file and stop rather than quietly test nothing.
    for s in subs:
      if fileExists(s / "setup.nim") or fileExists(s / ModeFile):
        var strays: seq[string] = @[]
        for x in walkDir(dir):
          if x.kind == pcFile and x.path.endsWith(".nim"): strays.add x.path
        quit "FAILURE: " & dir & " groups test suites (" & s &
             " is one) but also holds test files:\n  " & strays.join("\n  ") &
             "\nMove them into a suite directory — left here they hide the whole tree."
    # Leaf test directory: gather its own `.nim` files and do NOT descend.
    # Nested dirs here (`deps/`, `imp/`, `system/`, …) hold import fixtures
    # pulled in by those tests, not standalone tests — the old per-category
    # runner never entered them either.
    if parallelJobs > 1 and canRunParallel(cat):
      # The directory's plain tests become ONE unit (a `joined` worker compiles
      # them into a single program); whatever cannot be joined stays a unit of
      # its own. Both kinds go into the same flat queue.
      let members = joinMembers(dir, cat, overwrite)
      let joined = members.len >= MinJoinGroup
      if joined:
        # Mirrors `joinedTest`'s own all-or-nothing rule, so the prefill the
        # parent hands the worker is the one the worker will want.
        var groupNative = nativeJoinable(dir)
        if groupNative:
          for f in members:
            if not walkUsesNative(f, cat): groupNative = false; break
        plan.parItems.add WorkItem(path: dir, joined: true, weight: members.len,
                                   native: groupNative)
      for x in walkDir(dir):
        if x.kind == pcFile and x.path.endsWith(".nim") and
           not isGeneratedTestFile(x.path) and
           not (joined and joinable(x.path, cat)):
          plan.parItems.add WorkItem(path: x.path, weight: 1,
                                     native: walkUsesNative(x.path, cat))
    else:
      # `Basics`/`Compat` reset the shared `nimcache/` around their loop and so
      # cannot share the pool's cache layout; serial (`--jobs:1`) runs keep the
      # in-process `testFile` path. Either way, defer to a per-dir `testDir`.
      plan.serialDirs.add (dir, cat)
  else:
    # Pure grouping directory (e.g. `tests/`, `tests/nimony/`): recurse.
    sort subs
    for s in subs: collectTests(c, plan, s, forward, overwrite, isRoot = false)

proc walkRoots(roots: openArray[string]; forward: string; overwrite: bool) =
  ## Run one or more test trees, accumulating into shared counters and
  ## reporting once. `hastur <dir>` passes a single root; `all` passes
  ## `tests/` and `examples/`.
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  var plan = WalkPlan()
  for r in roots:
    if not dirExists(r): quit "FAILURE: not a directory: " & r
    collectTests(c, plan, r, forward, overwrite, isRoot = true)
  # Serial suites first: `Basics`/`Compat` wipe the whole `nimcache/`, which
  # would delete the pool's per-test cache dirs if it ran afterwards. They
  # finish and reset the cache, then the single flat pool populates it.
  for (d, cat) in plan.serialDirs:
    testDir(c, d, overwrite, cat, forward)
  if plan.parItems.len > 0:
    # Biggest units first: a joined group is many tests in one process, so
    # starting the long poles early keeps the pool's tail short.
    sort plan.parItems, proc (a, b: WorkItem): int =
      result = cmp(b.weight, a.weight)
      if result == 0: result = cmp(a.path, b.path)
    # One saturated pool over every parallel-safe unit from every directory.
    # `parallelTestDir` ignores the `cat` argument (each worker re-derives its
    # own category from its path's directory), so a mixed-category queue is safe.
    parallelTestDir(c, plan.parItems, overwrite, Normal, forward, parallelJobs)
  reportFailures c
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
      of "skip":
        if val.len == 0: writeHelp()
        skipDirs.add normalizeDirKey(val)
      of "joined":
        # `--joined:off` gives every test its own process again. The joined
        # runner already falls back to that per group on failure; this is for
        # ruling the joining out entirely.
        joinTests = val.normalize notin ["off", "no", "false", "0"]
      of "native":
        # `--native:off` puts the Windows tree walk back on the C backend for
        # the whitelisted tests it would otherwise compile with `nimony n`
        # (see `walkUsesNative`). Nothing anywhere else reads it.
        walkNative = val.normalize notin ["off", "no", "false", "0"]
      of "native-debug", "nativedebug":
        # Build arkham + nifasm UNOPTIMIZED (they default to -d:release; see
        # nativeToolPrefix). For `-d:arkhamDbgSym` / gdb work on the toolchain.
        nativeToolsDebug = true
      of "debug":
        # Build the front-end tools UNOPTIMIZED (they default to -d:release;
        # see nimcPrefix). Position-agnostic like `--release` below: these
        # decide how the toolchain is built, and the tree walk rebuilds it
        # via `setup.hastur` long after the subcommand was parsed.
        debugBuild = true
      of "release":
        # Build-mode-wise a no-op (release IS the default), for the toolchain
        # itself *and* — since it is now `boot`'s default — for the toolchain
        # `boot` compiles. Retained as the explicit spelling of that default.
        bootRelease = true
      of "no-release", "norelease":
        # Boot at the default opt level. For isolating whether a boot failure
        # is release-specific (`-d:release` turns the shoggoth optimizer on and
        # flips the `when defined(release)` paths at once, so a green
        # `--no-release` boot narrows the cause to one of those two).
        bootRelease = false
      of "valgrind":
        withValgrind = true
      of "boot-backend", "bootbackend":
        # Which backend `boot` compiles the stages with. `auto` (the default)
        # is `useNativeBoot`; the other two are for measuring one against the
        # other, which is otherwise impossible on a host where the native path
        # is the automatic one.
        case val.normalize
        of "auto": bootBackend = bbAuto
        of "c": bootBackend = bbC
        of "native", "n": bootBackend = bbNative
        else: writeHelp()
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
    # `buildNimonyToolchain`, not `buildNimony`: every module on the tier list is
    # compiled by driving nimsem and hexer, so rebuilding the driver alone would
    # check current sources against whichever nimsem/hexer `bin/` was left with.
    if not skipBuild: buildNimonyToolchain()
    # `hastur tiers native` walks the same list through `nimony n`. Spelled as a
    # positional rather than reusing `--boot-backend:`, which names what `boot`
    # does and would read as a flag with no effect here.
    var tiersNative = false
    for a in items(args):
      case a.normalize
      of "native", "n": tiersNative = true
      of "c": tiersNative = false
      else: quit "tiers: unknown argument " & a & " (expected `native` or `c`)"
    tierTests(tiersNative, forward)

  of "boot":
    # Same reason, one step further: `bin0/` is a COPY of `bin/`, so a stale
    # nimsem or hexer there is what every later stage is grown from.
    if not skipBuild: buildNimonyToolchain()
    var bootArgs = ""
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
    bootCmd(bootArgs, withValgrind, release = bootRelease)

  of "selfcheck":
    selfcheckCmd()

  of "build":
    const showProgress = true
    # Only fetch the submodule when it is actually missing. A `jj` workspace
    # (or a `git worktree`) shares one repo and has no gitlink of its own, so
    # `git submodule update --init` fails there with "Unable to find current
    # revision" even when the sources are present. Checking for the files is
    # what the build cares about anyway.
    if not fileExists("vendor" / "mimalloc" / "src" / "static.c"):
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
      buildNativeTools(showProgress)
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
      # that also feeds the native path). `all` builds these three too; this
      # spelling is the one that rebuilds JUST them, and errors out rather than
      # skip when the sibling checkout is missing.
      buildArkham(showProgress)
      buildNifasm(showProgress)
      buildShoggoth(showProgress)
    of "hexer":
      buildHexer(showProgress)
    of "nifmake":
      buildNifmake(showProgress)
    of "nifbench":
      buildNifbench(showProgress)
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
    #
    # The front end is rebuilt HERE and not left to whatever `bin/` happens to
    # hold. The shared host-Nim artifact cache is restored through a
    # `restore-keys:` prefix, so a commit that changes `src/**` misses the exact
    # key and still gets the PREVIOUS commit's `bin/` handed to it — which is how
    # the new closure tests of #2292 came to be run against the pre-#2292 hexer
    # and reported as eight "native gap" failures.
    #
    # nimsem/nimony/hexer and not `nifler`: those three share `programs.nim` and
    # ARE what a native run exercises, while nifler is the tier-0 Nim parser in
    # front of them — and the most expensive tool in the tree by a distance
    # (28s cold, more than the three together). `build all` covers it, in this
    # same job for CI and via `tests/setup.hastur` for a tree walk.
    if not skipBuild:
      buildNimonyToolchain()
      buildNifmake()
      buildShoggoth()
      buildArkham()
      buildNifasm()
    nativetests(overwrite)

  of "nativevalgrind":
    # The same corpus as `native`, built with `-d:valgrind` and run under
    # memcheck. Separate command rather than part of `native` because it is a
    # different question (is the ALLOCATOR's story to valgrind true?) at maybe
    # twenty times the runtime — memcheck's interpretation is not cheap.
    if not skipBuild:
      buildNimonyToolchain()
      buildNifmake()
      buildShoggoth()
      buildArkham()
      buildNifasm()
    nativeValgrindTests()

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
  of "joined":
    # Internal: the worker the parallel pool spawns for a directory's joined
    # group. Usable by hand to run (or debug) one group.
    if args.len == 1 and args[0].dirExists():
      joinedDirCmd args[0], overwrite, forward
    else:
      quit "`joined` takes one directory"
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
    # The joined-group drivers are generated into the test tree itself.
    for root in ["tests", "examples"]:
      if dirExists(root):
        for f in walkDirRec(root):
          if isGeneratedTestFile(f) and f.endsWith(".nim"): removeFile f
  of "update":
    # `update deps` rather than a bare `update`, because the sibling repos are
    # not the only thing this could come to re-pin.
    if args.len == 1 and args[0] == "deps": updateDepsCmd()
    else: writeHelp()
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

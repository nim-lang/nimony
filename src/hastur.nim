## Hastur - Tester tool for Nimony and its related subsystems (NIFC etc).
## (c) 2024-2025 Andreas Rumpf

when defined(windows):
  when defined(gcc):
    when defined(x86):
      {.link: "../icons/hastur.res".}
    else:
      {.link: "../icons/hastur_icon.o".}

import std / [syncio, assertions, parseopt, strutils, times, os, osproc, algorithm, streams]

import lib / [nifindexes, lineinfos, argsfinder]
import gear2 / modnames

const
  Version = "0.6.0"
  Usage = "hastur - tester tool for Nimony Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  hastur [options] [command] [arguments]

Commands:
  build [all|nimony|nifler|hexer|nifc|nifmake|nj|vl|validator]   build selected tools (default: all).
  bootstrap            compile every module on the bootstrap list with nimony.
  all                  run all tests (also the default action).
  nimony               run Nimony tests.
  examples             run examples (examples/ directory).
  nifc                 run NIFC tests.
  nj                   run NJ (Nimony Jump Elimination) tests.
  vl                   run VL (Versioned Locations) tests.
  incremental          verify nifmake's mtime-based incremental rebuilds via
                       the `--report` machine-readable summary.
  test <file>/<dir>    run test <file> or <dir>.
  bug [file]           build nimony+hexer and compile <file> to fill nimcache/.
                       If no file is provided `bug.nim` is used.
  rep                  repeat the last failing tool command from the session.
  record <file> <tout> track the results to make it part of the test suite.
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
  --no-build            skip rebuilding nimony/nifc before `test`
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

proc execLocal(exe, cmd: string): (string, int) =
  let bin = "bin" / exe.addFileExt(ExeExt)
  result = osproc.execCmdEx(bin & " " & cmd)

type
  Category = enum
    Normal, # normal category
    Basics, # basic tests: These are processed with --noSystem
    Tracked # tracked tests: These are processed and can contain "track info"
            # for line, col, filename extraction (useful for nimsuggest-like tests)
    Compat # compatibility mode tests
    Valgrind # valgrind tests

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
  ## parent has already rebuilt nimony / nifc before kicking off the
  ## pool, so each worker skips the rebuild. Otherwise every worker
  ## spends seconds re-running `nim c` for nothing.

proc toCommand(cat: Category): string =
  case cat
  of Basics: "m"
  of Tracked: "check --silentMake"
  of Normal, Compat, Valgrind: "c --silentMake"

proc execNimony(cmd: string; cat: Category): (string, int) =
  let cacheArg =
    if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " "
    else: ""
  result = execLocal("nimony", toCommand(cat) & " " & cacheArg & cmd)

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

proc testValgrind(c: var TestCounters; file: string; overwrite: bool; cat: Category; exe: string) =
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

proc testFile(c: var TestCounters; file: string; overwrite: bool; cat: Category; forward: string) =
  #echo "TESTING ", file
  inc c.total
  var nimonycmd = "--isMain"
  case cat
  of Normal, Valgrind: discard
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
    nimonycmd.add " --passC:\"-DMI_TRACK_VALGRIND=1\" "
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
    if cfile.fileExists():
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
    if cat == Basics:
      let ast = file.changeFileExt(".nif")
      if ast.fileExists():
        let nif = generatedFile(file, ".s.nif")
        diffFiles c, file, ast, nif, overwrite

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
  let nimony = "bin" / "nimony".addFileExt(ExeExt)
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

proc parallelTestDir(c: var TestCounters; files: openArray[string];
                     overwrite: bool; cat: Category; forward: string;
                     jobs: int) =
  ## Run each test in its own subprocess (`bin/hastur test ...`) with a
  ## per-test `--cacheDir` so concurrent compilations cannot collide on
  ## intermediates. Up to `jobs` subprocesses run at once. Test results
  ## are streamed in completion order; final pass/fail counts go into
  ## the shared `c`.
  let hastur = getAppFilename()
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
  var slots = newSeq[Slot](jobs)
  var active = 0

  proc launch(slot: int) =
    if head >= queue.len: return
    let (idx, file) = queue[head]
    inc head
    let cacheDir = nimcacheDir / ".par" / $idx
    prefillFromWarmup(warmupCache, cacheDir)
    var args = @["test", "--no-build", "--cachedir:" & cacheDir]
    if overwrite: args.add "--overwrite"
    if forward.len > 0: args.add "--forward:" & forward
    args.add file
    slots[slot] = Slot(idx: idx, file: file,
      p: startProcess(hastur, args = args,
        options = {poStdErrToStdOut, poUsePath}))
    inc active

  for s in 0 ..< jobs: launch(s)

  while active > 0:
    for s in 0 ..< jobs:
      if slots[s].p == nil: continue
      let exit = peekExitCode(slots[s].p)
      if exit != -1:
        var outp = ""
        var line = ""
        while slots[s].p.outputStream.readLine(line):
          outp.add line
          outp.add '\n'
        slots[s].p.close()
        inc c.total
        if exit != 0:
          inc c.failures
          stdout.write outp
        else:
          stdout.write outp
        slots[s].p = nil
        dec active
        launch(s)
    if active > 0:
      sleep(2)

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

proc parseCategory(path: string): Category =
  case path
  of "track": Tracked
  of "nosystem": Basics
  of "compat": Compat
  of "valgrind": Valgrind
  else: Normal

proc findCategory(path: string): Category =
  for x in split(path, {DirSep, AltSep}):
    let cat = parseCategory x
    if cat != Normal:
      return cat
  return Normal

proc exampletests(overwrite: bool; forward: string) =
  ## Run all the examples in the examples/ directory.
  const TestDir = "examples"
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  testDir c, TestDir, overwrite, Normal, forward
  echo c.total - c.failures, " / ", c.total, " examples successful in ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some examples failed."
  else:
    echo "SUCCESS."

proc nimonytests(overwrite: bool; forward: string) =
  ## Run all the nimonytests in the test-suite.
  const TestDir = "tests/nimony"
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  for x in walkDir(TestDir, relative = true):
    let cat = parseCategory x.path
    if x.kind == pcDir:
      testDir c, TestDir / x.path, overwrite, cat, forward
  echo c.total - c.failures, " / ", c.total, " tests successful in ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some tests failed."
  else:
    echo "SUCCESS."

proc runNifToolTests(tool, testDir, inputExt, expectedExt: string; overwrite: bool) =
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

proc controlflowTests(tool: string; overwrite: bool) =
  ## Run all the controlflow tests in the test-suite.
  runNifToolTests(tool, "tests/" & tool, ".nif", ".expected.nif", overwrite)

proc njTests(overwrite: bool) =
  ## Run all the NJ (Nimony Jump Elimination) tests.
  ## Tests are .nif files in src/njvl/tests/ with expected output in .nj.nif files.
  runNifToolTests("nj", "src/njvl/tests", ".nif", ".nj.nif", overwrite)

proc vlTests(overwrite: bool) =
  ## Run all the VL (Versioned Locations) tests.
  ## Tests are .nif files in src/njvl/tests/ with expected output in .vl.nif files.
  runNifToolTests("vl", "src/njvl/tests", ".nif", ".vl.nif", overwrite)

proc validatorTests() =
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
    "src/hexer/nifcgen.nim",
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

proc incrementalTests() =
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
  else:
    echo "SUCCESS."

proc testDirCmd(dir: string; overwrite: bool; forward: string) =
  var c = TestCounters(total: 0, failures: 0)
  let t0 = epochTime()
  testDir c, dir, overwrite, findCategory(dir), forward
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
  result = "bin"

proc robustMoveFile(src, dest: string) =
  if fileExists(src):
    moveFile src, dest

var release = false

proc nimcPrefix(): string =
  (if release: "nim c -d:release " else: "nim c ")

proc validatePassesFlag(): string =
  ## Enable the phase-aware IR validator only when running on CI. GitHub Actions
  ## (and most other CI providers) set `CI=true` in the environment, so we key
  ## off that: locally the validator stays opt-in via `NIMONY_VALIDATE=1`, which
  ## keeps iteration fast while guaranteeing the check on every PR.
  if getEnv("CI").len > 0 or getEnv("NIMONY_VALIDATE").len > 0:
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

proc buildControlflow(showProgress = false) =
  exec nimcPrefix() & "src/nimony/controlflow.nim", showProgress
  let exe = "controlflow".addFileExt(ExeExt)
  robustMoveFile "src/nimony/" & exe, binDir() / exe

proc buildContracts(showProgress = false) =
  exec nimcPrefix() & "src/nimony/contracts.nim", showProgress
  let exe = "contracts".addFileExt(ExeExt)
  robustMoveFile "src/nimony/" & exe, binDir() / exe

proc buildNj(showProgress = false) =
  exec nimcPrefix() & "src/njvl/nj.nim", showProgress
  let exe = "nj".addFileExt(ExeExt)
  robustMoveFile "src/njvl/" & exe, binDir() / exe

proc buildVl(showProgress = false) =
  exec nimcPrefix() & "src/njvl/vl.nim", showProgress
  let exe = "vl".addFileExt(ExeExt)
  robustMoveFile "src/njvl/" & exe, binDir() / exe

proc buildNifc(showProgress = false) =
  exec nimcPrefix() & "src/nifc/nifc.nim", showProgress
  let exe = "nifc".addFileExt(ExeExt)
  robustMoveFile "src/nifc/" & exe, binDir() / exe

proc buildHexer(showProgress = false) =
  exec nimcPrefix() & "src/hexer/hexer.nim", showProgress
  let exe = "hexer".addFileExt(ExeExt)
  robustMoveFile "src/hexer/" & exe, binDir() / exe

proc buildNifmake(showProgress = false) =
  exec nimcPrefix() & "src/nifmake/nifmake.nim", showProgress
  let exe = "nifmake".addFileExt(ExeExt)
  robustMoveFile "src/nifmake/" & exe, binDir() / exe

proc buildValidator(showProgress = false) =
  exec nimcPrefix() & "src/validator/validator.nim", showProgress
  let exe = "validator".addFileExt(ExeExt)
  robustMoveFile "src/validator/" & exe, binDir() / exe

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

  # Tier 17 tips. `hexer.nim` subsumes `nifcgen.nim` via its import set.
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

proc bootstrapTests() =
  ## Compile every module on `BootstrapModules` with `bin/nimony c`. Fails
  ## fast on the first regression so the offending module is obvious.
  let nimony = binDir() / "nimony".addFileExt(ExeExt)
  if not fileExists(nimony):
    quit "bootstrap: " & nimony & " not found; run `hastur build nimony` first"
  let t0 = epochTime()
  var failed: seq[string] = @[]
  for m in BootstrapModules:
    removeDir "nimcache"
    let subcmd = if m in RunnableBootstrapModules: " c -r " else: " c "
    let (output, ec) = execCmdEx(nimony.quoteShell & subcmd & m.quoteShell)
    if ec == 0:
      echo "OK   ", m
    else:
      echo "FAIL ", m
      echo output
      failed.add m
  echo failed.len, " / ", BootstrapModules.len, " bootstrap regressions in ",
       formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if failed.len > 0:
    quit "FAILURE: bootstrap regression(s): " & failed.join(", ")
  else:
    echo "SUCCESS."

proc execNifc(cmd: string) =
  exec "nifc", cmd

proc execHexer(cmd: string) =
  exec "hexer", cmd

proc nifctests(overwrite: bool) =
  let t1 = "tests/nifc/selectany/t1.nif"
  let t2 = "tests/nifc/selectany/t2.nif"
  let t3 = "tests/nifc/selectany/t3.nif"
  execNifc " c -r " & t1 & " " & t2 & " " & t3
  let app = "tests/nifc/app.c.nif"
  execNifc " c -r " & app

  let hello = "tests/nifc/hello.nif"
  execNifc " c -r " & hello
  execNifc " c -r --opt:speed " & hello
  execNifc " c -r --opt:size " & hello
  # TEST CPP
  execNifc " cpp -r " & hello
  execNifc " cpp -r --opt:speed " & hello

  let tryIssues = "tests/nifc/try.nif"
  execNifc " cpp -r " & tryIssues

  let issues = "tests/nifc/issues.nif"
  execNifc " c -r --linedir:on " & issues
  execNifc " cpp -r --linedir:off " & issues

proc hexertests(overwrite: bool) =
  let mod1 = "tests/hexer/mod1"
  let helloworld = "tests/hexer/hexer_helloworld"
  createIndex helloworld & ".nif", false, NoLineInfo
  createIndex mod1 & ".nif", false, NoLineInfo
  execHexer "c " & mod1 & ".nif"
  execHexer "c " & helloworld & ".nif"
  execNifc " c -r " & mod1 & ".c.nif " & helloworld & ".c.nif"

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

proc handleCmdLine =
  var primaryCmd = ""
  var args: seq[string] = @[]

  var flags: set[RecordFlag] = {}
  var overwrite = false
  var forward = ""
  for kind, key, val in getopt():
    case kind
    of cmdArgument:
      if primaryCmd.len == 0:
        primaryCmd = key.normalize
      else:
        args.add key
    of cmdLongOption, cmdShortOption:
      # `--cachedir` / `--jobs` can appear anywhere — they configure the
      # test runner regardless of position relative to the primary cmd.
      # Other long options stay tied to the pre-command position (or to
      # the `record` subcommand).
      let n = normalize(key)
      case n
      of "cachedir":
        if val.len == 0: writeHelp()
        nimcacheDir = val
      of "jobs", "j":
        if val == "auto" or val.len == 0:
          parallelJobs = countProcessors()
        else:
          try: parallelJobs = max(1, parseInt(val))
          except: writeHelp()
      of "no-build", "nobuild":
        skipBuild = true
      else:
        if primaryCmd.len == 0 or primaryCmd == "record":
          case n
          of "help", "h": writeHelp()
          of "version", "v": writeVersion()
          of "codegen": flags.incl RecordCodegen
          of "ast": flags.incl RecordAst
          of "overwrite": overwrite = true
          of "forward": forward = val
          of "release": release = true
          else: writeHelp()
        else:
          args.add key
          if val.len != 0:
            args[^1].add ':'
            args[^1].add val
    of cmdEnd: assert false, "cannot happen"
  if primaryCmd.len == 0:
    primaryCmd = "all"

  createDir binDir()

  case primaryCmd
  of "all":
    buildNimsem()
    buildNimony()
    buildNifc()
    buildHexer()
    buildNifmake()
    nimonytests(overwrite, forward)
    exampletests(overwrite, forward)
    #nifctests(overwrite)
    #hexertests(overwrite)
    buildControlflow()
    controlflowTests("controlflow", overwrite)
    buildContracts()
    controlflowTests("contracts", overwrite)
    buildNj()
    njTests(overwrite)
    buildVl()
    vlTests(overwrite)
    buildValidator()
    validatorTests()
    incrementalTests()
    bootstrapTests()

  of "validate", "validator":
    buildValidator()
    validatorTests()

  of "incremental":
    incrementalTests()

  of "bootstrap":
    buildNimony()
    bootstrapTests()

  of "controlflow", "cf":
    buildControlflow()
    controlflowTests("controlflow", overwrite)

  of "contracts":
    buildContracts()
    controlflowTests("contracts", overwrite)

  of "nj":
    buildNj()
    njTests(overwrite)

  of "vl":
    buildVl()
    vlTests(overwrite)

  of "build":
    const showProgress = true
    exec "git submodule update --init"
    case (if args.len > 0: args[0] else: "")
    of "", "all":
      buildNifler(showProgress)
      buildNimsem(showProgress)
      buildNimony(showProgress)
      buildNifc(showProgress)
      buildHexer(showProgress)
      buildNifmake(showProgress)
      buildNj(showProgress)
      buildVl(showProgress)
    of "nifler":
      buildNifler(showProgress)
    of "nimony":
      buildNimsem(showProgress)
      buildNimony(showProgress)
      buildHexer(showProgress)
    of "nifc":
      buildNifc(showProgress)
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
    else:
      writeHelp()
    removeDir "nimcache"

  of "nimony":
    buildNimony()
    nimonytests(overwrite, forward)
  of "examples":
    buildNimony()
    exampletests(overwrite, forward)
  of "nifc":
    buildNifc()
    nifctests(overwrite)

  of "hexer":
    buildHexer()
    hexertests(overwrite)
  of "test":
    if not skipBuild:
      buildNimony()
      buildNifc()
    if args.len > 0:
      if args[0].dirExists():
        testDirCmd args[0], overwrite, forward
      else:
        test args[0], overwrite, findCategory(args[0]), forward
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
      let cat = findCategory(args[1])
      if splitFile(args[1]).dir == "":
        record inp, "tests/nimony/basics" / outp, flags, cat
      else:
        record inp, outp, flags, cat
    else:
      quit "`record` takes two arguments"
  of "clean":
    removeDir "nimcache"
    removeDir "bin"
  of "sync":
    syncCmd(if args.len > 0: args[0] else: "")
  of "pull":
    pullpush("pull")
  of "push":
    pullpush("push")
  else:
    quit "invalid command: " & primaryCmd

handleCmdLine()

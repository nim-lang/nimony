## The built-in nimony test runner: one test file, one joined group, one
## directory. `hastur test <file>` and `hastur joined <dir>` land here, and so
## does every worker the parallel pool spawns.

import std / [syncio, os, osproc, strutils, times, algorithm]
import context, counters, category, compile, joined, parallel

# ---- the `.valgrind` golden -----------------------------------------------

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


proc testValgrind*(c: var TestCounters; file: string; overwrite: bool; cat: Category; exe: string) =
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


# ---- one test ------------------------------------------------------------

proc testFile*(c: var TestCounters; file: string; overwrite: bool; cat: Category; forward: string) =
  #echo "TESTING ", file
  let failuresBefore = c.failures
  inc c.total
  let nimonycmd = nimonyCmdFor(file, cat, forward)
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

# ---- one joined group ----------------------------------------------------
# Which tests a group contains, and why grouping them pays, is `joined.nim`.

proc joinedTest*(c: var TestCounters; dir: string; files: seq[string];
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

  let (compilerOutput, compilerExitCode) =
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

# ---- one directory, and the three commands that name a target -------------

proc testDir*(c: var TestCounters; dir: string; overwrite: bool; cat: Category; forward: string) =
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

proc test*(t: string; overwrite: bool; cat: Category; forward: string) =
  var c = TestCounters(total: 0, failures: 0)
  testFile c, t, overwrite, cat, forward
  if c.failures > 0:
    quit "FAILURE: Test failed."

proc testDirCmd*(dir: string; overwrite: bool; forward: string) =
  var c = TestCounters(total: 0, failures: 0)
  let t0 = epochTime()
  testDir c, dir, overwrite, categoryOfDir(dir), forward
  reportFailures c
  echo c.total - c.failures, " / ", c.total, " tests successful in ", formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some tests failed."
  else:
    echo "SUCCESS."

proc joinedDirCmd*(dir: string; overwrite: bool; forward: string) =
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

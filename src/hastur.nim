## Hastur - Tester tool for Nimony and its related subsystems (NIFC etc).
## (c) 2024-2025 Andreas Rumpf

when defined(windows):
  when defined(gcc):
    when defined(x86):
      {.link: "../icons/hastur.res".}
    else:
      {.link: "../icons/hastur_icon.o".}

import std / [syncio, assertions, parseopt, strutils, times, os, osproc, algorithm]

import lib / [nifindexes, lineinfos, argsfinder]
import gear2 / modnames

const
  Version = "0.6.0"
  Usage = "hastur - tester tool for Nimony Version " & Version & """

  (c) 2024-2025 Andreas Rumpf
Usage:
  hastur [options] [command] [arguments]

Commands:
  build [all|nimony|nifler|hexer|nifc|nifmake|nj|vl]   build selected tools (default: all).
  bootstrap            compile every module on the bootstrap list with nimony.
  all                  run all tests (also the default action).
  nimony               run Nimony tests.
  examples             run examples (examples/ directory).
  nifc                 run NIFC tests.
  nj                   run NJ (Nimony Jump Elimination) tests.
  vl                   run VL (Versioned Locations) tests.
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

proc toCommand(cat: Category): string =
  case cat
  of Basics: "m"
  of Tracked: "check --silentMake"
  of Normal, Compat, Valgrind: "c --silentMake"

proc execNimony(cmd: string; cat: Category): (string, int) =
  result = execLocal("nimony", toCommand(cat) & " " & cmd)

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
  result = if ext == ".s.nif": "nimcache" / name.addFileExt(ext)
           else: "nimcache" / name / name.addFileExt(ext)

proc generatedExeFile(orig: string): string =
  let name = modnames.moduleSuffix(orig, pathsForFile(orig))
  result = "nimcache" / name / orig.splitFile.name.addFileExt(ExeExt)

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

proc testDir(c: var TestCounters; dir: string; overwrite: bool; cat: Category; forward: string) =
  var files: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim"):
      files.add x.path
  sort files
  if cat in {Compat, Basics}:
    removeDir "nimcache"
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

proc checkTagsTests() =
  ## Run check_tags over compiler pass source files to verify NIF construction
  ## conforms to the grammar in doc/tags.md.
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
  # Compiler passes must be violation-free
  for f in passFiles:
    inc c.total
    let (msgs, exitcode) = execLocal("check_tags", os.quoteShell(f))
    if exitcode != 0:
      failure c, f, "check_tags: no violations", msgs
  # fake_pass.nim must produce the expected violations
  const fakePassDir = "tests/check_tags"
  for x in walkDir(fakePassDir, relative = true):
    if x.kind == pcFile and x.path.endsWith(".nim"):
      inc c.total
      let t = fakePassDir / x.path
      let expectedFile = t.changeFileExt(".expected")
      let (msgs, exitcode) = execLocal("check_tags", os.quoteShell(t))
      if not expectedFile.fileExists():
        failure c, t, "expected file " & expectedFile & " missing", ""
      else:
        let expected = readFile(expectedFile).strip
        # Extract just the violation lines from the output
        var got = ""
        for line in msgs.splitLines:
          if line.startsWith("  ") or line.contains("violation"):
            if got.len > 0: got.add "\n"
            got.add line
        if got.strip.replace("\\", "/") != expected.strip.replace("\\", "/"):
          failure c, t, expected, got
  echo c.total - c.failures, " / ", c.total, " check_tags tests successful in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some check_tags tests failed."
  else:
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

proc buildCheckTags(showProgress = false) =
  exec nimcPrefix() & "src/nimony/check_tags.nim", showProgress
  let exe = "check_tags".addFileExt(ExeExt)
  robustMoveFile "src/nimony/" & exe, binDir() / exe

# ---------------------------------------------------------------------------
# Bootstrapping progress (see https://github.com/nim-lang/nimony/issues/1788).
#
# Each module listed here is known to compile with the `nimony c` command.
# New modules are added as tier-by-tier bootstrapping proceeds; the
# `hastur bootstrap` target walks this list to catch regressions.
# ---------------------------------------------------------------------------

const BootstrapModules = [
  # Tier 1 -- pure leaves (no project-internal imports):
  "src/models/tags.nim",
  "src/lib/stringviews.nim",
  "src/lib/tinyhashes.nim",
  "src/lib/symparser.nim",
  "src/lib/bitabs.nim",
  "src/lib/lineinfos.nim",
  "src/lib/nifbuilder.nim",
  "src/nimony/features.nim",
  "src/nimony/intervals.nim",
  "src/nimony/xints.nim",
  # Tier 2 -- tag enums + simple deps on tier 1.
  "src/models/callconv_tags.nim",
  "src/models/njvl_tags.nim",
  "src/models/nifindex_tags.nim",
  "src/models/nifc_tags.nim",
  "src/models/nifler_tags.nim",
  "src/models/nimony_tags.nim",
  "src/lib/nifreader.nim",
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
    let (output, ec) = execCmdEx(nimony.quoteShell & " c " & m.quoteShell)
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
      if primaryCmd.len == 0 or primaryCmd == "record":
        case normalize(key)
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
    buildCheckTags()
    checkTagsTests()
    bootstrapTests()

  of "checktags":
    buildCheckTags()
    checkTagsTests()

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

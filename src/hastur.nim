## Hastur - Tester tool for Nimony and its related subsystems (NIFC etc).
## (c) 2024 Andreas Rumpf

import std / [syncio, assertions, parseopt, strutils, times, os, osproc, algorithm]

import gear2 / modnames

const
  Version = "0.6"
  Usage = "hastur - tester tool for Nimony Version " & Version & """

  (c) 2024 Andreas Rumpf
Usage:
  hastur [options] [command] [arguments]

Commands:
  all                  run all tests (also the default action).
  nimony               run Nimony tests.
  nifc                 run NIFC tests.
  test <file>          run test <file>.
  record <file> <tout> track the results to make it part of the test suite.

Arguments are forwarded to the Nimony compiler.

Options:
  --overwrite           overwrite the selected test results
  --ast                 track the contents of the AST too
  --codegen             track the contents of the code generator too
  --version             show the version
  --help                show this help
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

proc markersToCmdLine(s: seq[LineInfo]): string =
  result = ""
  for x in items(s):
    result.add " --track:" & $x.line & ":" & $x.col & ":" & x.filename

proc execLocal(exe, cmd: string): (string, int) =
  let bin = "." / exe.addFileExt(ExeExt)
  result = osproc.execCmdEx(bin & " " & cmd)

proc execNimony(cmd: string): (string, int) =
  result = execLocal("nimony", cmd)

proc generatedFile(orig, ext: string): string =
  let name = modnames.moduleSuffix(orig, [])
  result = "nifcache" / name.addFileExt(ext)

proc testFile(c: var TestCounters; file: string; overwrite, useTrack: bool) =
  inc c.total
  var nimonycmd = "m --isMain"
  if useTrack:
    nimonycmd.add markersToCmdLine extractMarkers(readFile(file))
  let (compilerOutput, compilerExitCode) = execNimony(nimonycmd & " " & quoteShell(file))

  let msgs = file.changeFileExt(".msgs")

  var expectedExitCode = 0
  if msgs.fileExists():
    let msgSpec = readFile(msgs).strip
    let success = msgSpec == compilerOutput.strip
    if not success:
      if overwrite:
        writeFile(msgs, compilerOutput)
      failure c, file, msgSpec, compilerOutput
    expectedExitCode = if msgSpec.contains(ErrorKeyword): 1 else: 0
  if compilerExitCode != expectedExitCode:
    failure c, file, "compiler exitcode " & $expectedExitCode, compilerOutput & "\nexitcode " & $compilerExitCode

  if compilerExitCode == 0:
    let cfile = file.changeFileExt(".nim.c")
    if cfile.fileExists():
      let nimcacheC = generatedFile(file, ".c")
      diffFiles c, file, cfile, nimcacheC, overwrite

    when false:
      # XXX Enable when we have a code generator
      let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell file.changeFileExt(ExeExt))
      if testProgramExitCode != 0:
        failure c, file, "test program exitcode 0", "exitcode " & $testProgramExitCode
      let output = file.changeFileExt(".output")
      if output.fileExists():
        let outputSpec = readFile(output).strip
        let success = outputSpec == testProgramOutput.strip
        if not success:
          if overwrite:
            writeFile(output, testProgramOutput)
          failure c, file, outputSpec, testProgramOutput

    let ast = file.changeFileExt(".nif")
    if ast.fileExists():
      let nif = generatedFile(file, ".2.nif")
      diffFiles c, file, ast, nif, overwrite

proc testDir(c: var TestCounters; dir: string; overwrite, useTrack: bool) =
  var files: seq[string] = @[]
  for x in walkDir(dir):
    if x.kind == pcFile and x.path.endsWith(".nim"):
      files.add x.path
  sort files
  for f in items files:
    testFile c, f, overwrite, useTrack

proc nimonytests(overwrite: bool) =
  ## Run all the nimonytests in the test-suite.
  const TestDir = "tests/nimony"
  let t0 = epochTime()
  var c = TestCounters(total: 0, failures: 0)
  for x in walkDir(TestDir, relative = true):
    if x.kind == pcDir:
      testDir c, TestDir / x.path, overwrite, (x.path == "track")
  echo c.total - c.failures, " / ", c.total, " tests successful in ", formatFloat(epochTime() - t0, precision=2), "s."
  if c.failures > 0:
    quit "FAILURE: Some tests failed."
  else:
    echo "SUCCESS."

proc test(t: string; overwrite, useTrack: bool) =
  var c = TestCounters(total: 0, failures: 0)
  testFile c, t, overwrite, useTrack
  if c.failures > 0:
    quit "FAILURE: Test failed."
  else:
    echo "SUCCESS."

proc exec(cmd: string) =
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

proc record(file, test: string; flags: set[RecordFlag]) =
  # Records a new test case.
  let (compilerOutput, compilerExitCode) = osproc.execCmdEx("nimony m " & quoteShell(file))
  if compilerExitCode == 1:
    let idx = compilerOutput.find(ErrorKeyword)
    assert idx >= 0, "compiler output did not contain: " & ErrorKeyword
    copyFile file, test
    # run the test again so that the error messages contain the correct paths:
    let (finalCompilerOutput, finalCompilerExitCode) = osproc.execCmdEx("nimony m " & quoteShell(test))
    assert finalCompilerExitCode == 1, "the compiler should have failed once again"
    gitAdd test
    addTestSpec test.changeFileExt(".msgs"), finalCompilerOutput
  else:
    when false:
      # XXX We don't have a backend yet so no `.output` files can be extracted
      let (testProgramOutput, testProgramExitCode) = osproc.execCmdEx(quoteShell file.changeFileExt(ExeExt))
      assert testProgramExitCode == 0, "the test program had an invalid exitcode; unsupported"
      addTestSpec test.changeFileExt(".output"), testProgramOutput

    addTestCode test, file
    if {RecordCodegen, RecordAst} * flags != {}:
      let (finalCompilerOutput, finalCompilerExitCode) = osproc.execCmdEx("nimony m " & quoteShell(test))
      assert finalCompilerExitCode == 0, finalCompilerOutput

    if RecordCodegen in flags:
      let nimcacheC = generatedFile(test, ".c")
      addTestCode test.changeFileExt(".nim.c"), nimcacheC

    if RecordAst in flags:
      let nif = generatedFile(test, ".2.nif")
      addTestCode test.changeFileExt(".nif"), nif

proc buildNimony() =
  exec "nim c src/nimony/nimony.nim"
  let exe = "nimony".addFileExt(ExeExt)
  moveFile "src/nimony/" & exe, exe

proc buildNifc() =
  exec "nim c src/nifc/nifc.nim"
  let exe = "nifc".addFileExt(ExeExt)
  moveFile "src/nifc/" & exe, exe

proc buildGear3() =
  exec "nim c src/gear3/gear3.nim"
  let exe = "gear3".addFileExt(ExeExt)
  moveFile "src/gear3/" & exe, exe

proc execNifc(cmd: string) =
  exec "nifc", cmd

proc execGear3(cmd: string) =
  exec "gear3", cmd

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

proc gear3tests(overwrite: bool) =
  let mod1 = "tests/gear3/mod1"
  let helloworld = "tests/gear3/gear3_helloworld"
  execGear3 mod1 & ".nif"
  execGear3 helloworld & ".nif"
  execNifc " c -r " & mod1 & ".c.nif " & helloworld & ".c.nif"

proc handleCmdLine =
  var primaryCmd = ""
  var args: seq[string] = @[]

  var flags: set[RecordFlag] = {}
  var overwrite = false
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
        else: writeHelp()
      else:
        args.add key
        if val.len != 0:
          args[^1].add ':'
          args[^1].add val
    of cmdEnd: assert false, "cannot happen"
  if primaryCmd.len == 0:
    primaryCmd = "all"

  case primaryCmd
  of "all":
    buildNimony()
    buildNifc()
    buildGear3()
    nimonytests(overwrite)
    nifctests(overwrite)
    gear3tests(overwrite)

  of "nimony":
    buildNimony()
    nimonytests(overwrite)
  of "nifc":
    buildNifc()
    nifctests(overwrite)

  of "gear3":
    buildGear3()
    gear3tests(overwrite)
  of "test":
    buildNimony()
    buildNifc()
    if args.len > 0:
      test args[0], overwrite, args[0].contains("track")
    else:
      quit "`test` takes an argument"
  of "record":
    buildNimony()
    if args.len == 2:
      let inp = args[0].addFileExt(".nim")
      let outp = args[1].addFileExt(".nim")
      if splitFile(args[1]).dir == "":
        record inp, "tests/nimony/basics" / outp, flags
      else:
        record inp, outp, flags
    else:
      quit "`record` takes two arguments"
  else:
    quit "invalid command: " & primaryCmd

handleCmdLine()

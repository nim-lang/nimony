## The per-directory custom suites a `tests/<dir>/setup.nim` drives: the NIF
## tool goldens (nj, vl, controlflow, contracts), the validator, hexer, dagon
## and pnak.

import std / [syncio, os, osproc, strutils, times]
import ".." / lib / nifindexes
from ".." / lib / nifpools import NoLineInfo
import context, counters, builders

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

proc execLengc*(cmd: string) =
  exec "lengc", cmd

proc execHexer*(cmd: string) =
  exec "hexer", cmd

proc hexertests*(overwrite: bool) =
  let mod1 = "tests/hexer/mod1"
  let helloworld = "tests/hexer/hexer_helloworld"
  createIndex helloworld & ".nif", false, NoLineInfo
  createIndex mod1 & ".nif", false, NoLineInfo
  execHexer "c " & mod1 & ".nif"
  execHexer "c " & helloworld & ".nif"
  execLengc " c -r " & mod1 & ".c.nif " & helloworld & ".c.nif"

proc runDagonTest*(c: var TestCounters; testFile: string) =
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

proc runPnakTest*(c: var TestCounters; testFile: string) =
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

## `hastur wasmdiff` — the native backend as an executable ORACLE for the wasm
## backend.
##
## Every fixture under `tests/ithaqua/` is one Nim source pushed through BOTH
## C-free pipelines. There is no golden file to record and no expected output to
## keep in sync: the two legs must simply agree, byte for byte on stdout and on
## the exit code. That is what makes this cheap to extend — a new fixture is a
## `.nim` file and nothing else — and it is also why it catches what a golden
## suite cannot: a shared front-end bug shows up as agreement, and a disagreement
## names the backend that drifted.
##
##   native leg: `nimony n <f>` -> a static, libc-free ELF; run it.
##   wasm leg:   `nimony w --out:<work>/out.wasm <f>` (hexer -> dce -> ithaqua,
##               orchestrated by nifmake), then `node run_wasm.js out.wasm`.
##
## The oracle cuts both ways: fixtures whose NATIVE leg is the wrong one get
## quarantined too, as `tests/ithaqua/nativebugs/` once held. They are kept as
## sources, out of the sweep, so the reproducers do not get lost; when the oracle
## is fixed they move back up into the sweep.

import std / [syncio, os, osproc, strutils, times, algorithm]
import context, builders

proc soleNimcacheSubdir(nimcache: string): string =
  ## A single `nimony` build drops exactly one module-hash directory inside the
  ## `--nimcache` it was given. Return it (empty if none / more than one, which
  ## the caller treats as a failed compile). Each leg gets its own `--nimcache`
  ## dir precisely so this stays unambiguous — the subdir name is the module
  ## suffix we also use to locate the native exe.
  result = ""
  if not dirExists(nimcache): return
  var count = 0
  for x in walkDir(nimcache):
    if x.kind == pcDir:
      result = x.path
      inc count
  if count != 1: result = ""

proc runFixtureProgram(cmd: string; secs: int): tuple[output: string, exitCode: int, timedOut: bool] =
  ## Run a compiled fixture (the native ELF, or `node out.wasm`) capturing its
  ## stdout ONLY (`options={}` leaves stderr on hastur's own stderr), so the
  ## comparison is over real program output rather than merged streams. A
  ## MISCOMPILED wasm program can loop forever, so the run is wrapped in the
  ## `timeout` coreutil when it's on PATH: exit 124 then means it was killed for
  ## exceeding `secs` and is reported as a hang, never as matching output.
  let killer = findExe("timeout")
  let wrapped =
    if killer.len > 0: killer.quoteShell & " " & $secs & " " & cmd
    else: cmd
  let (outp, ec) = execCmdEx(wrapped, options = {})
  result = (outp, ec, killer.len > 0 and ec == 124)

proc wasmdiffCmd*() =
  if not skipBuild:
    buildNimony()
    buildHexer()
    buildShoggoth()
    buildArkham()
    buildNifasm()
    buildIthaqua()
  let dir = "tests/ithaqua"
  let nimony = binDir() / "nimony".addFileExt(ExeExt)
  let runnerJs = dir / "run_wasm.js"
  let nodeExe = findExe("node")
  if nodeExe.len == 0:
    quit "wasmdiff: `node` not found on PATH (needed to run the wasm leg)"
  if not fileExists(runnerJs):
    quit "wasmdiff: missing node host shim " & runnerJs

  var files: seq[string] = @[]
  for x in walkDir(dir):
    # Top level only — `wasmgaps/` (and any future `nativebugs/`) holds
    # quarantined repros of bugs on one side or the other and is deliberately
    # not swept.
    if x.kind == pcFile and x.path.endsWith(".nim") and
       x.path.extractFilename notin ["setup.nim", "_hastur_joined.nim"]:
      files.add x.path
  sort files
  if files.len == 0:
    quit "wasmdiff: no fixtures found under " & dir

  let workBase = getTempDir() / "hastur-wasmdiff"
  removeDir workBase
  let t0 = epochTime()
  var failures = 0
  for file in files:
    let stem = file.splitFile.name
    var failMsg = ""

    # --- native leg (the oracle) ------------------------------------------
    let nativeNc = workBase / stem / "native-nc"
    createDir nativeNc
    let (nCompOut, nCompEc) = execCmdEx(
      nimony.quoteShell & " n --nimcache:" & nativeNc.quoteShell & " " &
        file.quoteShell)
    var nativeOut = ""
    var nativeEc = 0
    var nativeOk = false
    if nCompEc != 0:
      failMsg = "native compile failed (exit " & $nCompEc & "):\n" & nCompOut
    else:
      let sub = soleNimcacheSubdir(nativeNc)
      let exe = sub / stem.addFileExt(ExeExt)
      if sub.len == 0 or not fileExists(exe):
        failMsg = "native exe not found under " & nativeNc
      else:
        let (o, ec, timedOut) = runFixtureProgram(exe.quoteShell, 30)
        if timedOut:
          failMsg = "native oracle timed out (>30s) — bad fixture, not a diff"
        else:
          nativeOut = o
          nativeEc = ec
          nativeOk = true

    # --- wasm leg ---------------------------------------------------------
    var wasmOut = ""
    var wasmEc = 0
    var wasmOk = false
    if nativeOk:
      let wasmNc = workBase / stem / "wasm-nc"
      createDir wasmNc
      let outWasm = workBase / stem / "out.wasm"
      let (wCompOut, wCompEc) = execCmdEx(
        nimony.quoteShell & " w --nimcache:" & wasmNc.quoteShell &
          " --out:" & outWasm.quoteShell & " " & file.quoteShell)
      if wCompEc != 0 or not fileExists(outWasm):
        failMsg = "wasm compile failed (exit " & $wCompEc & "):\n" & wCompOut
      else:
        let (o, ec, timedOut) = runFixtureProgram(
          nodeExe.quoteShell & " " & runnerJs.quoteShell & " " &
            outWasm.quoteShell, 15)
        if timedOut:
          failMsg = "wasm execution timed out (>15s) — likely an infinite " &
            "loop in miscompiled wasm"
        else:
          wasmOut = o
          wasmEc = ec
          wasmOk = true

    # --- compare ----------------------------------------------------------
    if failMsg.len == 0 and nativeOk and wasmOk:
      if nativeOut != wasmOut:
        failMsg = "stdout mismatch\n" &
          "--- native (oracle) ---\n" & nativeOut &
          "--- wasm ---\n" & wasmOut & "---\n"
      elif nativeEc != wasmEc:
        failMsg = "exit-code mismatch: native " & $nativeEc & " vs wasm " & $wasmEc

    if failMsg.len == 0:
      echo "PASS ", file
    else:
      echo "FAIL ", file
      echo failMsg
      inc failures

  echo files.len - failures, " / ", files.len, " wasmdiff fixtures matched in ",
    formatFloat(epochTime() - t0, ffDecimal, precision=2), "s."
  if failures > 0:
    quit "FAILURE: " & $failures & " wasmdiff fixture(s) differ."
  else:
    echo "SUCCESS."

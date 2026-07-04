## Custom hastur runner for the JavaScript backend (`lengjs`).
##
## Each `t*.nim` here is a real program compiled end-to-end and run under
## `node`, its stdout diffed against the sibling `.output`:
##
##   nimony c --bits:32   →   the lowered Leng IR (`.c.nif`) of every module
##   lengjs <mod>.c.nif   →   one `.js` artifact per module
##   runtime.js + artifacts + `main(0, [])`   →   one bundle
##   node bundle.js       →   stdout compared to `<test>.output`
##
## `--bits:32` is not incidental: the JS target IS a 32-bit platform, so `int`
## maps to a JS Number and only `int64`/`uint64` become BigInt (see the PR).
## That same flag makes the trailing C link fail on a 64-bit host — harmless
## here: the `.c.nif` we consume is emitted by hexer *before* the C backend
## runs, so a real frontend error is the one that leaves no `.c.nif` behind.
##
## Run it via hastur:  `hastur tests/jsbackend`  (add `--overwrite` to
## regenerate the `.output` goldens).

import std / [os, strutils, osproc, algorithm]
import "../../src/hastur"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0: toolchainDir = arg("bindir")
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")
let overwrite = "--overwrite" in commandLineParams()
let dir = if arg("dir").len > 0: arg("dir") else: getCurrentDir()

# The JS backend needs `node` on PATH; without it the whole suite can only be a
# no-op, so say so loudly rather than reporting phantom passes.
if findExe("node").len == 0:
  quit "FAILURE: `node` not found on PATH — the JS backend tests need Node.js."

buildLengjs()
buildJslink()

let nimony = toolExe("nimony")
let lengjs = toolExe("lengjs")
let runtime = dir / "runtime.js"

type Res = enum rPass, rFail

proc runOne(nimFile: string): Res =
  ## Compile one `.nim` through nimony+lengjs, bundle, run under node, and diff
  ## stdout against the sibling `.output`. Returns rPass/rFail (and prints why).
  let name = nimFile.splitFile.name
  let cache = nimcacheDir / "jsbackend" / name
  removeDir cache
  createDir cache

  # 1. Front end + hexer → `.c.nif` per module. `--define:nimNativeAlloc` compiles
  #    the stdlib against Nim's OWN ported allocator (`system/alloc.nim`) instead
  #    of the mimalloc C binding, so the heap is real Nim code over the runtime's
  #    `mmap`/`munmap` — the libc-free config the JS target wants (same as the
  #    native backend). The 32-bit C link fails on a 64-bit host; we deliberately
  #    ignore the exit code and judge success by whether any `.c.nif` was produced
  #    (a real sema/hexer error produces none).
  let (compileOut, _) = execCmdEx(nimony.quoteShell &
    " c --bits:32 --define:nimNativeAlloc --silentMake --nimcache:" & cache.quoteShell & " " &
    nimFile.quoteShell)

  var cnifs: seq[string] = @[]
  for path in walkDirRec(cache):
    if path.endsWith(".c.nif"): cnifs.add path
  if cnifs.len == 0:
    echo "FAILURE ", nimFile, ": no .c.nif produced (compile error)\n", compileOut
    return rFail
  sort cnifs

  # 2. Each module → a `.js` artifact.
  var artifacts: seq[string] = @[]
  for c in cnifs:
    let js = c[0 ..< c.len - ".c.nif".len] & ".js"
    let (ljOut, ljCode) = execCmdEx(lengjs.quoteShell & " " & c.quoteShell &
                                    " " & js.quoteShell)
    if ljCode != 0:
      echo "FAILURE ", nimFile, ": lengjs failed on ", c, "\n", ljOut
      return rFail
    artifacts.add js

  # Optional per-test JS environment: if `<test>.env.js` exists it is prepended
  # to the bundle (e.g. a jsdom DOM installed under node_modules) and node runs
  # with node_modules on NODE_PATH. A test that needs an env whose deps aren't
  # installed is SKIPPED — the same way the whole suite is when `node` is absent
  # — not failed, so a bare checkout stays green.
  let envFile = nimFile.changeFileExt(".env.js")
  let needsEnv = fileExists(envFile)
  if needsEnv and not dirExists(dir / "node_modules"):
    echo "SKIP ", nimFile, ": needs a JS env — run `npm install` in ", dir
    return rPass

  # 3. Bundle: (optional env,) runtime, then the module artifacts (each drops its
  #    own `"use strict";`), then the console entry point. This is the by-hand
  #    form of what `jslink` does from the compiler's link manifest.
  var bundle = ""
  if needsEnv:
    bundle.add readFile(envFile)
    if not bundle.endsWith("\n"): bundle.add "\n"
  bundle.add readFile(runtime)
  if not bundle.endsWith("\n"): bundle.add "\n"
  for a in artifacts:
    for line in readFile(a).splitLines:
      if line == "\"use strict\";": continue
      bundle.add line
      bundle.add "\n"
  bundle.add "main(0, []);\n"
  let bundlePath = cache / "bundle.js"
  writeFile(bundlePath, bundle)

  # 4. Run under node, compare stdout to the golden.
  let nodeCmd = (if needsEnv: "NODE_PATH=" & (dir / "node_modules").quoteShell & " " else: "") &
                "node " & bundlePath.quoteShell
  let (nodeOut, nodeCode) = execCmdEx(nodeCmd)
  if nodeCode != 0:
    echo "FAILURE ", nimFile, ": node exited ", nodeCode, "\n", nodeOut
    return rFail

  let outFile = nimFile.changeFileExt(".output")
  let got = nodeOut.strip(leading = false)
  if not fileExists(outFile):
    if overwrite:
      writeFile(outFile, got & "\n")
      echo "RECORDED ", nimFile
      return rPass
    echo "FAILURE ", nimFile, ": no .output golden (run with --overwrite)\n", got
    return rFail
  let want = readFile(outFile).strip(leading = false)
  if got != want:
    if overwrite:
      writeFile(outFile, got & "\n")
      echo "OVERWROTE ", nimFile
      return rPass
    echo "FAILURE ", nimFile, "\n--- expected ---\n", want,
         "\n--- got ---\n", got
    return rFail
  echo "SUCCESS ", nimFile
  return rPass

var tests: seq[string] = @[]
for x in walkDir(dir):
  if x.kind == pcFile and x.path.endsWith(".nim") and
     x.path.splitFile.name.startsWith("t"):
    tests.add x.path
sort tests

var failures = 0
for t in tests:
  if runOne(t) == rFail: inc failures

echo tests.len - failures, " / ", tests.len, " JS backend tests successful."
if failures > 0:
  quit "FAILURE: Some JS backend tests failed."

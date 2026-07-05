## Custom hastur runner for the WebAssembly backend (`lengwasm`).
##
## Each `t*.nim` here is compiled to a real `.wasm` module and run under Node's
## `WebAssembly` engine — which *validates* the bytecode before executing it, so a
## malformed module fails loudly rather than silently. The sibling `t*.js` driver
## instantiates the module (sharing a `WebAssembly.Memory` as the linear-memory
## `ArrayBuffer`), calls its exported functions, and prints; stdout is diffed
## against the sibling `.output`:
##
##   nimony c --bits:32   →   the lowered Leng IR (`.c.nif`) of the main module
##   lengwasm <mod>.c.nif →   one `.wasm` artifact
##   node t.js <wasm>     →   instantiate, call exports, print
##   stdout               →   compared to `<test>.output`
##
## `--bits:32` is not incidental: the WASM target is a 32-bit platform (pointers
## are 4-byte offsets into linear memory). The trailing C link fails on a 64-bit
## host — harmless: the `.c.nif` we consume is emitted by hexer *before* the C
## backend runs, so a real frontend error is the one that leaves no `.c.nif`.
##
## Run it via hastur:  `hastur tests/wasmbackend`  (add `--overwrite` to
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

# The WASM backend needs `node` (its WebAssembly engine) on PATH; without it the
# suite can only be a no-op, so say so loudly rather than reporting phantom passes.
if findExe("node").len == 0:
  quit "FAILURE: `node` not found on PATH — the WASM backend tests need Node.js."

buildLengwasm()

let nimony = toolExe("nimony")
let lengwasm = toolExe("lengwasm")

type Res = enum rPass, rFail

proc mainCnif(cache: string): string =
  ## The main module's `.c.nif` — by nimony's convention its basename equals its
  ## containing directory's (the mangled module name); the stdlib modules it pulls
  ## in (`sys*` etc.) do not. That's the one carrying this test's `exportc` procs.
  for path in walkDirRec(cache):
    if path.endsWith(".c.nif"):
      let base = path.extractFilename
      let modName = base[0 ..< base.len - ".c.nif".len]   # strip the double ext
      if modName == path.parentDir.extractFilename:
        return path
  result = ""

proc runOne(nimFile: string): Res =
  let name = nimFile.splitFile.name
  let driver = nimFile.changeFileExt(".js")
  if not fileExists(driver):
    echo "FAILURE ", nimFile, ": no sibling driver ", driver.extractFilename
    return rFail
  let cache = nimcacheDir / "wasmbackend" / name
  removeDir cache
  createDir cache

  # 1. Front end + hexer → `.c.nif`. `--define:nimNativeAlloc` compiles against
  #    Nim's own ported allocator (libc-free, as the native/JS targets want). The
  #    32-bit C link fails on a 64-bit host; we ignore the exit code and judge by
  #    whether the main `.c.nif` was produced (a real sema/hexer error produces none).
  discard execCmdEx(nimony.quoteShell &
    " c --bits:32 --define:nimNativeAlloc --silentMake --nimcache:" & cache.quoteShell &
    " " & nimFile.quoteShell)

  let cnif = mainCnif(cache)
  if cnif.len == 0:
    echo "FAILURE ", nimFile, ": no main .c.nif produced (compile error)"
    return rFail

  # 2. Main module → a `.wasm` artifact.
  let wasm = cache / (name & ".wasm")
  let (lwOut, lwCode) = execCmdEx(lengwasm.quoteShell & " " & cnif.quoteShell &
                                  " " & wasm.quoteShell)
  if lwCode != 0:
    echo "FAILURE ", nimFile, ": lengwasm failed\n", lwOut
    return rFail

  # 3. Run the driver under node (it instantiates the module and calls its exports).
  let (nodeOut, nodeCode) = execCmdEx("node " & driver.quoteShell & " " & wasm.quoteShell)
  if nodeCode != 0:
    echo "FAILURE ", nimFile, ": node exited ", nodeCode, "\n", nodeOut
    return rFail

  # 4. Compare stdout to the golden.
  let outFile = nimFile.changeFileExt(".output")
  let got = nodeOut.strip(leading = false)
  if not fileExists(outFile):
    if overwrite:
      writeFile(outFile, got & "\n"); echo "RECORDED ", nimFile; return rPass
    echo "FAILURE ", nimFile, ": no .output golden (run with --overwrite)\n", got
    return rFail
  let want = readFile(outFile).strip(leading = false)
  if got != want:
    if overwrite:
      writeFile(outFile, got & "\n"); echo "OVERWROTE ", nimFile; return rPass
    echo "FAILURE ", nimFile, "\n--- expected ---\n", want, "\n--- got ---\n", got
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

echo tests.len - failures, " / ", tests.len, " WASM backend tests successful."
if failures > 0:
  quit "FAILURE: Some WASM backend tests failed."

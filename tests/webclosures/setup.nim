## Runner for closure calls on `nimony w`: wasm's `call_indirect` traps on a
## signature mismatch, so a closure must always be called with its env.
import std / [os, osproc, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let dir = if arg("dir").len > 0: arg("dir") else: "tests/webclosures"
let bindir = absolutePath(if arg("bindir").len > 0: arg("bindir") else: "bin")
let cachedir = if arg("cachedir").len > 0: arg("cachedir") else: "nimcache"
let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
let hastur = (bindir / "hastur".addFileExt(ExeExt)).quoteShell
let work = absolutePath(cachedir / "webclosures")
let hostJs = absolutePath(dir / "host.js")
let node = findExe("node")
var failures = 0

proc ok(msg: string) = echo "SUCCESS tests/webclosures: ", msg

proc fail(msg: string; detail = "") =
  echo "FAILURE tests/webclosures: ", msg
  if detail.len > 0: echo detail
  inc failures

if node.len == 0:
  echo "SKIP tests/webclosures: `node` not found on PATH"
  quit 0

if not fileExists(bindir / "jorogumo".addFileExt(ExeExt)):
  if "--no-build" in commandLineParams():
    quit "FAILURE tests/webclosures: " & bindir / "jorogumo" & " is missing; run `hastur build jorogumo`"
  let (bout, bcode) = execCmdEx(hastur & " build jorogumo")
  if bcode != 0: quit "FAILURE tests/webclosures: build jorogumo\n" & bout

removeDir work
createDir work

proc buildWasm(name: string): tuple[output: string, exitCode: int] =
  execCmdEx(nimony & " w --silentMake --nimcache:" & quoteShell(work / name) &
            " --out:" & quoteShell(work / (name & ".wasm")) & " " &
            quoteShell(absolutePath(dir / (name & ".nim"))))

block run:
  let (o, c) = buildWasm("closures")
  if c != 0:
    fail "closures: build failed", o
    break run
  let (r, rc) = execCmdEx(node.quoteShell & " " & hostJs.quoteShell & " " &
                          quoteShell(work / "closures.wasm"))
  if rc == 0 and r == "12 11 13 14 15\n12 11 13 14\n": ok "every closure form runs"
  else: fail "closures: wrong run (exit " & $rc & ")", r

block refused:
  let (o, c) = buildWasm("procvalue")
  if c != 0 and "cannot be converted to a closure" in o and "procvalue.nim(9" in o:
    ok "a proc value converted to a closure is refused"
  else: fail "procvalue: expected the refusal", o

removeDir work
if failures > 0: quit 1

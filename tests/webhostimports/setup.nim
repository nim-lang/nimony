## Custom runner for `nimony w --hostImports` (and `--exportAll`).
##
## `--hostImports` makes jorogumo turn a bodyless `importc` proc into an `env`
## import the host page provides; without it that proc is refused. The option is
## part of the backend's cache key, so flipping it in ONE nimcache must rebuild
## the output each time. `host.js` is the node host: it supplies the imports,
## and lists a module's imports and exports for the checks below.
##
## `--exportAll` goes through the same path (a key word + a jorogumo flag) and
## is the toggle case with an output that exists in both modes: a program with
## no bridge functions, built with and without the `dbg$` exports.
import std / [os, osproc, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let dir = if arg("dir").len > 0: arg("dir") else: "tests/webhostimports"
let bindir = absolutePath(if arg("bindir").len > 0: arg("bindir") else: "bin")
let cachedir = if arg("cachedir").len > 0: arg("cachedir") else: "nimcache"
let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
let hastur = (bindir / "hastur".addFileExt(ExeExt)).quoteShell
let work = absolutePath(cachedir / "webhostimports")
let hostJs = absolutePath(dir / "host.js")
let node = findExe("node")
var failures = 0

proc ok(msg: string) = echo "SUCCESS tests/webhostimports: ", msg

proc fail(msg: string; detail = "") =
  echo "FAILURE tests/webhostimports: ", msg
  if detail.len > 0: echo detail
  inc failures

if node.len == 0:
  echo "SKIP tests/webhostimports: `node` not found on PATH"
  quit 0

if not fileExists(bindir / "jorogumo".addFileExt(ExeExt)):
  # Not part of `build all`; the sibling nativenif checkout provides it.
  if "--no-build" in commandLineParams():
    quit "FAILURE tests/webhostimports: " & bindir / "jorogumo" & " is missing; run `hastur build jorogumo`"
  let (bout, bcode) = execCmdEx(hastur & " build jorogumo")
  if bcode != 0: quit "FAILURE tests/webhostimports: build jorogumo\n" & bout

removeDir work
createDir work

proc buildWasm(src, nimcache, outFile, flags: string): tuple[output: string, exitCode: int] =
  removeFile outFile
  execCmdEx(nimony & " w --silentMake " & flags & " --nimcache:" & quoteShell(work / nimcache) &
            " --out:" & quoteShell(outFile) & " " & quoteShell(src))

proc listing(wasm: string): string =
  let (o, c) = execCmdEx(node.quoteShell & " " & hostJs.quoteShell & " --imports " & quoteShell(wasm))
  if c != 0: fail "listing " & wasm, o
  result = o

let hostSrc = absolutePath(dir / "host.nim")
let plainSrc = absolutePath(dir / "plain.nim")

proc checkHost(nimcache, flags, label: string) =
  ## Built WITH host imports: has the `env` imports and runs under host.js.
  let outFile = work / (nimcache & ".wasm")
  let (o, c) = buildWasm(hostSrc, nimcache, outFile, flags)
  if c != 0 or not fileExists(outFile):
    fail label & ": build failed", o
    return
  let l = listing(outFile)
  if "env.host_add" notin l or "env.host_note" notin l:
    fail label & ": host imports missing", l
    return
  let (r, rc) = execCmdEx(node.quoteShell & " " & hostJs.quoteShell & " " & quoteShell(outFile))
  if rc == 0 and r == "note 7\nsum: 42\n": ok label
  else: fail label & ": wrong run (exit " & $rc & ")", r

proc checkRefused(nimcache, label: string) =
  ## Built WITHOUT the option: the bodyless importc is refused, no output.
  let outFile = work / (nimcache & ".wasm")
  let (o, c) = buildWasm(hostSrc, nimcache, outFile, "")
  if c != 0 and not fileExists(outFile) and "no host binding" in o:
    ok label
  else: fail label & ": expected the refusal", o

# (a) with the option: env imports, and it runs
checkHost "a", "--hostImports", "--hostImports: env imports present, program runs"
# (b) without: refused as before
checkRefused "b", "no option: bodyless importc is refused"
# (c) toggle in one nimcache: on, off, on
checkHost "c", "--hostImports", "toggle 1/3 (on)"
checkRefused "c", "toggle 2/3 (off) in the same nimcache"
checkHost "c", "--hostImports:on", "toggle 3/3 (on again)"

# the deprecated environment alias selects the same mode (and says so)
block alias:
  putEnv "ITHAQUA_HOST_IMPORTS", "1"
  let outFile = work / "alias.wasm"
  let (o, c) = buildWasm(hostSrc, "alias", outFile, "")
  delEnv "ITHAQUA_HOST_IMPORTS"
  if c == 0 and "deprecated" in o and "env.host_add" in listing(outFile):
    ok "ITHAQUA_HOST_IMPORTS=1 alias"
  else: fail "ITHAQUA_HOST_IMPORTS=1 alias", o

# --exportAll toggles the same way, with an output in both modes
proc dbgExports(nimcache, flags, label: string; want: bool) =
  let outFile = work / (nimcache & ".wasm")
  let (o, c) = buildWasm(plainSrc, nimcache, outFile, flags)
  if c != 0 or not fileExists(outFile):
    fail label & ": build failed", o
    return
  if ("export dbg$" in listing(outFile)) == want: ok label
  else: fail label & ": dbg$ exports " & (if want: "missing" else: "present")
dbgExports "x", "--exportAll", "--exportAll: dbg$ exports (1/3)", true
dbgExports "x", "", "--exportAll off in the same nimcache (2/3)", false
dbgExports "x", "--exportAll", "--exportAll again (3/3)", true

removeDir work
if failures > 0: quit 1

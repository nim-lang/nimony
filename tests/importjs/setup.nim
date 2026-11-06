## Custom runner for the `importjs` tests: compile each `.nim` with the JS back
## end (`nimony j`) and run the generated `.js` under `node` against
## `<test>.output`. Without `node` on PATH the suite is skipped.
import std / [os, osproc, strutils, algorithm]
import "../../src/hastur/kit"

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

if arg("bindir").len > 0:
  toolchainDir = arg("bindir")
  skipBuild = true
if arg("cachedir").len > 0: nimcacheDir = arg("cachedir")

if not skipBuild:
  buildNimony()
  buildShoggoth()
# `build all` does not build jorogumo, so this suite builds its own tool even
# under the tree walk's `--no-build`. Without the sibling checkout there is none.
if not dirExists(NativenifDir):
  echo "[importjs] ", NativenifDir, " not found; skipping"
  quit 0
buildJorogumo()

let node = findExe("node")
if node.len == 0:
  echo "[importjs] node not found; skipping"
  quit 0
let dir = if arg("dir").len > 0: arg("dir") else: getCurrentDir()
var c = TestCounters(total: 0, failures: 0)
var files: seq[string] = @[]
for x in walkDir(dir):
  if x.kind == pcFile and x.path.endsWith(".nim") and x.path.extractFilename != "setup.nim":
    files.add x.path
sort files
for file in files:
  inc c.total
  let cacheArg = if nimcacheDir != "nimcache": "--nimcache:" & quoteShell(nimcacheDir) & " " else: ""
  let (msgs, code) = execLocal("nimony", "j --silentMake --isMain " & cacheArg & quoteShell(file))
  if code != 0:
    failure c, file, "jorogumo build exitcode 0", removeMakeErrors(msgs) & "\nexitcode " & $code
    continue
  let js = generatedFile(file, ".c.nif", BackendDirJs).parentDir / file.splitFile.name & ".js"
  let (output, runCode) = execCmdEx(quoteShell(node) & " " & quoteShell(js))
  let spec = readFile(file.changeFileExt(".output")).strip
  if runCode != 0 or output.strip != spec:
    failure c, file, spec, output & "\nexitcode " & $runCode
reportFailures c
echo c.total - c.failures, " / ", c.total, " importjs tests successful."
if c.failures > 0:
  quit "FAILURE: Some importjs tests failed."
else:
  echo "SUCCESS."

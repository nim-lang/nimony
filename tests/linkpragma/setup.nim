## Custom runner for the `{.link.}` pragma: it needs a prebuilt object, which
## must come from the compiler the tests use, so it cannot be checked in.
## Compile `deps/answer.c`, put `deps/tlink.nim` next to the object and run it.
import std / [os, osproc, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let dir = if arg("dir").len > 0: arg("dir") else: "tests/linkpragma"
let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let cachedir = if arg("cachedir").len > 0: arg("cachedir") else: "nimcache"
let forward = arg("forward")
var cc = "gcc"
for f in forward.splitWhitespace:
  if f.startsWith("--cc:"): cc = f.substr(5)

let work = absolutePath(cachedir / "linkpragma")
createDir work
copyFile dir / "deps" / "tlink.nim", work / "tlink.nim"

let ccCmd = cc.quoteShell & " -c " & quoteShell(dir / "deps" / "answer.c") &
            " -o " & quoteShell(work / "answer.o")
let (ccOut, ccCode) = execCmdEx(ccCmd)
if ccCode != 0:
  quit "FAILURE: " & ccCmd & "\n" & ccOut

let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
let cmd = nimony & " c -r --silentMake --nimcache:" & quoteShell(work / "nimcache") &
          " " & forward & " " & quoteShell(work / "tlink.nim")
let (output, code) = execCmdEx(cmd)
if code == 0 and output.strip.endsWith("42"):
  echo "SUCCESS tests/linkpragma: `{.link.}` linked a prebuilt object"
else:
  quit "FAILURE: " & cmd & "\n" & output

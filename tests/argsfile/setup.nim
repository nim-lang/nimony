## Custom runner for a main module named without a directory: `nimony c
## targsfile.nim`, started in `deps/`, must read `deps/nimony.args` like
## `nimony c ./targsfile.nim` does (#2972).
import std / [os, osproc, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let dir = if arg("dir").len > 0: arg("dir") else: "tests/argsfile"
let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let cachedir = if arg("cachedir").len > 0: arg("cachedir") else: "nimcache"
let forward = arg("forward")

let nimony = absolutePath(bindir / "nimony".addFileExt(ExeExt)).quoteShell
let cmd = nimony & " c -r --silentMake --nimcache:" &
          quoteShell(absolutePath(cachedir / "argsfile")) & " " & forward & " targsfile.nim"
let (output, code) = execCmdEx(cmd, workingDir = dir / "deps")
if code == 0 and output.strip.endsWith("args file read"):
  echo "SUCCESS tests/argsfile: nimony.args next to targsfile.nim"
else:
  echo "FAILURE: ", cmd, "\n", output
  quit 1

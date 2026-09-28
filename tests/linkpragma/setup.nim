## Custom runner for linking foreign objects: `{.link.}` needs a prebuilt object,
## which must come from the compiler the tests use, so it cannot be checked in.
## Compile `deps/answer.c`, put `deps/tlink.nim` next to the object and run it.
##
## On Linux/x86-64 the native backend is checked too: `nimony n -d:useLibc`
## finishes the program with the system linker (nifasm writes an object), so it
## runs `.link`, `.compile` and a variadic libc call.
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
copyFile dir / "deps" / "tprintf.nim", work / "tprintf.nim"

let ccCmd = cc.quoteShell & " -c " & quoteShell(dir / "deps" / "answer.c") &
            " -o " & quoteShell(work / "answer.o")
let (ccOut, ccCode) = execCmdEx(ccCmd)
if ccCode != 0:
  quit "FAILURE: " & ccCmd & "\n" & ccOut

let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
var failures = 0

proc check(backend, file, expected: string; nimcache: string) =
  let cmd = nimony & " " & backend & " -r --silentMake --nimcache:" &
            quoteShell(work / nimcache) & " " & forward & " " & quoteShell(file)
  let (output, code) = execCmdEx(cmd)
  if code == 0 and output.strip.endsWith(expected):
    echo "SUCCESS tests/linkpragma: `nimony ", backend, "` ", file.extractFilename
  else:
    echo "FAILURE: ", cmd, "\n", output
    inc failures

check "c", work / "tlink.nim", "42", "nc_c"
when defined(linux) and defined(amd64):
  if fileExists(bindir / "arkham".addFileExt(ExeExt)):
    check "n -d:useLibc", work / "tlink.nim", "42", "nc_n"
    check "n -d:useLibc", "tests/nimony/pragmas/tcompilepragma.nim", "322", "nc_compile"
    check "n -d:useLibc", work / "tprintf.nim", "42 1.5 libc", "nc_printf"
    # Without libc the freestanding image cannot link a foreign object.
    let cmd = nimony & " n --silentMake --nimcache:" & quoteShell(work / "nc_free") &
              " " & forward & " " & quoteShell(work / "tlink.nim")
    let (output, code) = execCmdEx(cmd)
    if code != 0 and "-d:useLibc" in output:
      echo "SUCCESS tests/linkpragma: `nimony n` without libc names -d:useLibc"
    else:
      echo "FAILURE: ", cmd, "\n", output
      inc failures
if failures > 0: quit 1

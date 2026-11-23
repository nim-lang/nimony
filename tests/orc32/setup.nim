## `--mm:orc` on a 32-bit CPU: the system module (Nim's ORC uses the cell's
## 64-bit header word as an `int`) must get through `check`. Needs no cross
## compiler, so it runs on any host.
import std/[os, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let root = currentSourcePath().parentDir.parentDir.parentDir
let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
let cache = root / "nimcache_static" / "orc32"
createDir(cache)
let probe = (root / "tests" / "orc32" / "probe.nim").quoteShell
let log = cache / "check.log"

for cpu in ["i386", "arm"]:
  let command = nimony & " --cpu:" & cpu & " --mm:orc --nimcache:" &
                (cache / cpu).quoteShell & " check " & probe
  if execShellCmd(command & " > " & log.quoteShell & " 2>&1") != 0:
    quit "FAILURE: " & command & "; see " & log
echo "orc32: check passed for i386 and arm"

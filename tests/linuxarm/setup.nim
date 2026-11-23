## std/posix on 32-bit ARM: sem-check a std/os program for Linux and Android
## without a cross C compiler.
import std/[os, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let root = currentSourcePath().parentDir.parentDir.parentDir
let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
let probe = (root / "tests" / "linuxarm" / "probe.nim").quoteShell

for targetOS in ["linux", "android"]:
  let cache = root / "nimcache_static" / "linuxarm" / targetOS
  createDir(cache)
  let log = cache / "check.log"
  let command = nimony & " --os:" & targetOS & " --cpu:arm --nimcache:" &
                cache.quoteShell & " check " & probe
  if execShellCmd(command & " > " & log.quoteShell & " 2>&1") != 0:
    quit "FAILURE: " & command & "; see " & log
echo "linux/arm std/posix: check passed"

## `--app:lib`: the library exports a `NimMain` that runs its top-level code.
## Builds a shared library and a C host, so it runs on Linux only.
import std/[os, osproc, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

when defined(linux):
  let root = currentSourcePath().parentDir.parentDir.parentDir
  let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
  let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
  let dir = root / "tests" / "applib"
  let cache = root / "nimcache_static" / "applib"
  removeDir(cache)
  createDir(cache)
  let log = cache / "build.log"
  let build = nimony & " c --app:lib --nimcache:" & cache.quoteShell & " " &
              (dir / "applib.nim").quoteShell
  if execShellCmd(build & " > " & log.quoteShell & " 2>&1") != 0:
    quit "FAILURE: " & build & "; see " & log
  var lib = ""
  for f in walkDirRec(cache):
    if f.extractFilename == "libapplib.so": lib = f
  if lib.len == 0: quit "FAILURE: no libapplib.so under " & cache
  let host = cache / "host"
  let link = "cc " & (dir / "host.c").quoteShell & " -o " & host.quoteShell &
             " -L" & lib.parentDir.quoteShell & " -lapplib -Wl,-rpath," &
             lib.parentDir.quoteShell
  if execShellCmd(link & " >> " & log.quoteShell & " 2>&1") != 0:
    quit "FAILURE: " & link & "; see " & log
  let (output, code) = execCmdEx(host.quoteShell)
  if code != 0 or output != "module init called\nhi from lib\n":
    quit "FAILURE: host exited with " & $code & " and printed:\n" & output
  echo "applib: NimMain passed"
else:
  echo "applib: skipped (needs a Linux host)"

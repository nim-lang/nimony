## Check the illumos allocator policy without rebuilding shared compiler tools.
import std/[os, strutils]

proc arg(name: string): string =
  let prefix = "--" & name & ":"
  for p in commandLineParams():
    if p.startsWith(prefix): return p[prefix.len .. ^1]
  result = ""

let root = currentSourcePath().parentDir.parentDir.parentDir
let bindir = if arg("bindir").len > 0: arg("bindir") else: "bin"
let nimony = (bindir / "nimony".addFileExt(ExeExt)).quoteShell
let cache = root / "nimcache_static" / "illumosalloc"
createDir(cache)
let probe = (root / "tests" / "illumosalloc" / "probe.nim").quoteShell
let log = cache / "driver.log"
let target = " --os:illumos --cpu:amd64 --bits:64 --nimcache:" & cache.quoteShell

# Rejection happens in the driver, so it can be checked on any host.
for flags in ["-d:useMimalloc", "-d:useLibc -d:useMimalloc",
              "-d:nimNativeAlloc -d:useMimalloc"]:
  let code = execShellCmd(nimony & target & " " & flags & " check " & probe &
                         " > " & log.quoteShell & " 2>&1")
  if code == 0 or not readFile(log).contains("mimalloc is unsupported on illumos"):
    quit "FAILURE: expected illumos mimalloc rejection; see " & log

when (defined(sunos) or defined(solaris) or defined(illumos)) and defined(amd64):
  let exe = cache / "probe"
  for flags in ["", "-d:useLibc -d:expectLibcIo", "-d:useLibcIo -d:expectLibcIo",
                "-d:useLibc -d:nimNativeAlloc -d:expectLibcIo"]:
    let command = nimony & target & " --cc:gcc --out:" & exe.quoteShell &
                  " " & flags & " c " & probe
    if execShellCmd(command & " > " & log.quoteShell & " 2>&1") != 0:
      quit "FAILURE: " & command & "; see " & log
    if execShellCmd(exe.quoteShell & " >> " & log.quoteShell & " 2>&1") != 0:
      quit "FAILURE: allocator policy/runtime check; see " & log
  echo "illumos allocator: native allocation and independent libc IO passed"
else:
  echo "illumos allocator: mimalloc rejection passed (runtime checks require illumos/amd64)"

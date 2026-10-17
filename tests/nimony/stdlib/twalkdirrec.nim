# std/dirs walkDirRec, copyDir and copyDirWithPermissions. Special files
# (FIFOs, sockets, devices) are not created here: `skipSpecial` is only
# passed through. The moveDir cross-device fallback needs two filesystems
# and is not exercised.

import std/[assertions, algorithm, os, syncio, dirs, files, symlinks]

proc collect(dir: Path; yieldFilter = {pcFile}; followFilter = {pcDir};
             relative = true): seq[string] {.raises.} =
  result = @[]
  for p in walkDirRec(dir, yieldFilter, followFilter, relative):
    var x = $p
    for c in mitems(x):
      if c == '\\': c = '/'
    result.add x
  sort result

proc show(s: seq[string]) =
  var line = ""
  for i, x in s:
    if i > 0: line.add ' '
    line.add x
  echo line

proc main() {.raises.} =
  let base = path(getTempDir()) / path("nimony_twalkdirrec")
  removeDir(base)
  let src = base / path("src")
  createDir(src / path("a/b"))
  createDir(src / path("empty"))
  writeFile($(src / path("top")), "top")
  writeFile($(src / path("a/f1")), "f1")
  writeFile($(src / path("a/b/f2")), "f2")

  # an outside tree reached only through a directory symlink
  let outside = base / path("outside")
  createDir(outside)
  writeFile($(outside / path("x")), "x")
  when defined(posix):
    createSymlink(outside, src / path("dlink"))
    createSymlink(path("top"), src / path("flink"))

  show collect(src)
  show collect(src, yieldFilter = {pcDir})
  show collect(src, yieldFilter = {pcFile, pcDir}, followFilter = {})
  when defined(posix):
    show collect(src, yieldFilter = {pcLinkToFile, pcLinkToDir})
    show collect(src, followFilter = {pcDir, pcLinkToDir})
  else:
    echo "dlink flink"
    echo "a/b/f2 a/f1 dlink/x top"

  # relative = false yields paths joined with `dir`
  var full = 0
  for p in walkDirRec(src):
    assert ($p).len > ($src).len and fileExists($p)
    inc full
  echo full

  # checkDir only matters for a missing root
  var n = 0
  for p in walkDirRec(base / path("missing")): inc n
  echo n
  var err = Success
  try:
    for p in walkDirRec(base / path("missing"), checkDir = true): inc n
  except ErrorCode as e:
    err = e
  echo err == NameNotFound

  # copyDir: nested tree and empty subdirectory; symlinks stay links
  when defined(posix):
    setFilePermissions(src / path("a/f1"),
                       {fpUserRead, fpUserWrite, fpUserExec})
  let dst = base / path("dst")
  copyDir(src, dst)
  echo readFile($(dst / path("a/b/f2")))
  echo dirExists($(dst / path("empty")))
  when defined(posix):
    echo symlinkExists($(dst / path("dlink")))
    echo $expandSymlink(dst / path("flink"))
    echo fpUserExec in getFilePermissions(dst / path("a/f1"))
  else:
    echo "true"
    echo "top"
    echo "false"

  # copyDirWithPermissions keeps the exec bit
  let dst2 = base / path("dst2")
  copyDirWithPermissions(src, dst2)
  echo readFile($(dst2 / path("top")))
  echo dirExists($(dst2 / path("empty")))
  when defined(posix):
    echo symlinkExists($(dst2 / path("dlink")))
    echo fpUserExec in getFilePermissions(dst2 / path("a/f1"))
  else:
    echo "true"
    echo "true"

  # copying into an existing destination overwrites files (an existing
  # symlink would make `createSymlink` fail, as in Nim)
  writeFile($(src / path("a/f1")), "new")
  copyDir(src / path("a"), dst / path("a"))
  echo readFile($(dst / path("a/f1")))

  removeDir(base)
  echo dirExists($base)

try:
  main()
except:
  quit "twalkdirrec: unexpected error"

import std/[dirs, paths, syncio, assertions]

# Exercises createDir/removeDir/removeFile and walkDir (opendir/readdir/closedir
# on POSIX, FindFirstFile/FindNextFile/FindClose on Windows). Also serves as the
# regression test for the freestanding (-d:nimNativeIo) directory iteration that
# reimplements those on top of getdents64(2) on Linux.

proc main {.raises.} =
  let base = path("twalkdir_testdir")
  removeDir(base)        # clean up a stale run; ignore "does not exist"
  createDir(base)
  createDir(base / path("sub"))
  writeFile($(base / path("a.txt")), "aaa")
  writeFile($(base / path("b.txt")), "bbb")

  var files = 0
  var dirCount = 0
  for kind, p in walkDir(base, relative = true):
    case kind
    of pcFile, pcLinkToFile: inc files
    of pcDir, pcLinkToDir: inc dirCount
  assert files == 2
  assert dirCount == 1

  # relative = false yields the full path joined with `base`.
  var seenA = false
  for kind, p in walkDir(base):
    if $p == $(base / path("a.txt")): seenA = true
  assert seenA

  removeDir(base / path("sub"))
  removeFile(base / path("a.txt"))
  removeFile(base / path("b.txt"))
  removeDir(base)
  echo "ok"

try:
  main()
except:
  quit "walkDir test failed"

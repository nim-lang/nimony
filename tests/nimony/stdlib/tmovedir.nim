# std/dirs moveDir: a rename within one filesystem. The cross-device
# fallback (copyDir + removeDir) is not exercised: it needs two filesystems.
import std/[assertions, dirs, os, syncio]

proc main() {.raises.} =
  let base = path(getTempDir()) / path("nimony_tmovedir")
  discard tryRemoveFile(base / path("b/f"))
  discard tryRemoveFile(base / path("a/f"))
  removeDir(base / path("a"))
  removeDir(base / path("b"))
  removeDir(base / path("c"))
  removeDir(base)
  createDir(base / path("a"))
  writeFile($(base / path("a/f")), "content")

  moveDir(base / path("a"), base / path("b"))
  echo dirExists($(base / path("a")))
  echo readFile($(base / path("b/f")))

  # a missing source
  var err = Success
  try:
    moveDir(base / path("a"), base / path("c"))
  except ErrorCode as e:
    err = e
  echo err == NameNotFound
  echo dirExists($(base / path("c")))

  removeFile(base / path("b/f"))
  removeDir(base / path("b"))
  removeDir(base)

try:
  main()
except:
  quit "tmovedir: unexpected error"

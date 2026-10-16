# std/tempfiles: genTempPath, createTempFile and createTempDir.

import std/[assertions, syncio, appdirs, dirs, files, strutils]
from std/os import fileExists, dirExists
import std/tempfiles

proc isNameChar(c: char): bool =
  c in {'a'..'z', 'A'..'Z', '0'..'9'}

proc checkName(p: Path; dir: Path; prefix, suffix: string) =
  assert parentDir(p) == dir
  let name = $lastPathPart(p)
  assert name.startsWith(prefix)
  assert name.endsWith(suffix)
  assert name.len == prefix.len + 8 + suffix.len
  for i in prefix.len ..< name.len - suffix.len:
    assert isNameChar(name[i])

proc main() {.raises.} =
  # a private base directory, created by the module itself under getTempDir()
  let base = createTempDir("nimony_ttempfiles_", "")
  assert dirExists($base)
  checkName(base, getTempDir(), "nimony_ttempfiles_", "")
  echo "base dir created"

  # genTempPath: format only, nothing is created
  let g1 = genTempPath("pre_", "_suf", base)
  let g2 = genTempPath("pre_", "_suf", base)
  checkName(g1, base, "pre_", "_suf")
  assert g1 != g2
  assert not fileExists($g1) and not dirExists($g1)
  echo "genTempPath ok"

  # createTempDir: fresh, distinct directories under `base`
  let d1 = createTempDir("dir_", ".d", base)
  let d2 = createTempDir("dir_", ".d", base)
  assert d1 != d2
  assert dirExists($d1) and dirExists($d2)
  checkName(d1, base, "dir_", ".d")
  echo "createTempDir ok"

  # createTempFile: an open, writable and readable File
  var (f, p) = createTempFile("file_", ".tmp", base)
  checkName(p, base, "file_", ".tmp")
  assert fileExists($p)
  f.write "foo"
  f.setFilePos 0
  assert readAll(f) == "foo"
  close f
  assert readFile($p) == "foo"
  let (f2, p2) = createTempFile("file_", ".tmp", base)
  assert p2 != p
  close f2
  echo "createTempFile ok"

  when defined(posix):
    assert getFilePermissions(p) == {fpUserRead, fpUserWrite}
    assert getFilePermissions(d1) == {fpUserRead, fpUserWrite, fpUserExec}
  echo "permissions ok"

  # a missing directory is an error, not a retry loop
  var raised = false
  try:
    discard createTempDir("x", "", base / path("missing"))
  except ErrorCode as e:
    raised = e == NameNotFound
  assert raised
  raised = false
  try:
    discard createTempFile("x", "", base / path("missing"))
  except ErrorCode as e:
    raised = e == NameNotFound
  assert raised
  echo "missing dir raises"

  removeFile(p)
  removeFile(p2)
  removeDir(d1)
  removeDir(d2)
  removeDir(base)
  assert not dirExists($base)
  echo "cleanup ok"

try:
  main()
except:
  quit "ttempfiles: unexpected error"

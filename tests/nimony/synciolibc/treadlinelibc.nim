## Regression test: syncio's `readLine` under -d:useLibc (the `fgets` path,
## 80-byte chunks) neither adds the chunk's NUL terminator to a long line nor
## reads past it on a last line without a newline.

import std/[assertions, os, syncio, files, strutils]

proc main() {.raises.} =
  let name = getTempDir() / "nimony_treadlinelibc.txt"
  let long = repeat('a', 79) & "BCDEFGHIJ"
  let l79 = repeat('b', 79)
  let l80 = repeat('c', 80)
  writeFile(name, long & "\n" & l79 & "\n" & l80 & "\nabc")
  var f: File
  let opened = open(f, name)
  assert opened
  var line = ""
  assert readLine(f, line)
  assert line.len == 88
  assert line == long
  assert readLine(f, line)
  assert line.len == 79
  assert line == l79
  assert readLine(f, line)
  assert line.len == 80
  assert line == l80
  assert readLine(f, line)
  assert line.len == 3
  assert line == "abc"
  assert not readLine(f, line)
  close(f)
  discard tryRemoveFile(path(name))
  echo "ok"

try:
  main()
except:
  quit "treadlinelibc: unexpected error"

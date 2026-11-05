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

proc show(s: string): string =
  ## `s` with NUL escaped and runs of a byte written as `a*78`.
  result = ""
  var i = 0
  while i < s.len:
    let c = s[i]
    var run = 1
    while i + run < s.len and s[i + run] == c: inc run
    if c == '\0': result.add "\\0"
    else: result.add c
    if run > 1:
      result.add "*"
      result.add $run
    i = i + run

proc lines(content: string): string {.raises.} =
  ## Writes `content` to a file and reads it back with `readLine(f, var s)`:
  ## each line as `len:"text"`, then `EOF` once readLine returns false.
  let name = getTempDir() / "nimony_treadlinelibc_case.txt"
  writeFile(name, content)
  var f: File
  let opened = open(f, name)
  assert opened
  result = ""
  var line = ""
  var n = 0
  while n < 10 and readLine(f, line):
    result.add $line.len
    result.add ":\""
    result.add show(line)
    result.add "\" "
    inc n
  result.add "EOF"
  close(f)
  discard tryRemoveFile(path(name))

proc cases() {.raises.} =
  echo lines("")
  echo lines("\n")
  echo lines(repeat('a', 78) & "\n")
  echo lines(repeat('a', 78))  # the only case that ends inside a chunk
  echo lines(repeat('a', 79))
  echo lines(repeat('a', 158))
  echo lines("a\0b\n")
  echo lines("q\0")
  echo lines("a\0\nx")
  echo lines(repeat('a', 77) & "\0")
  echo lines(repeat('a', 78) & "\0")  # Nim 2 drops this trailing NUL

try:
  main()
  cases()
except:
  quit "treadlinelibc: unexpected error"

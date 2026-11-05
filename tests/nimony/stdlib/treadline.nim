# syncio's line reading: `readLine` (both overloads) and `lines` return each
# line without its `\n`, and without a `\r` right before that `\n`; a final
# line without a `\n` is returned as is; NUL bytes are plain data.
# `addReadLine` appends the raw line: it drops only the `\n`.
# synciolibc/treadlinelibc.nim includes this file to pin -d:useLibc to the
# same output.

import std/[assertions, os, syncio, files, strutils]

const Branch = when defined(nimNativeIo): "native" else: "libc"

proc tempName(): string =
  result = getTempDir() / ("nimony_treadline_" & Branch & ".txt")

proc main() {.raises.} =
  let name = tempName()
  writeFile(name, "first\r\n\nlast")
  var f: File
  let opened = open(f, name)
  assert opened
  assert readLine(f) == "first"
  assert readLine(f) == ""
  assert readLine(f) == "last"
  var atEnd = false
  try:
    discard readLine(f)
  except ErrorCode as e:
    atEnd = e == EndOfStreamError
  assert atEnd
  close(f)

  writeFile(name, "a\r\nb")
  let reopened = open(f, name)
  assert reopened
  var s = "x"
  assert addReadLine(f, s)
  assert s == "xa\r"
  assert addReadLine(f, s)
  assert s == "xa\rb"
  assert not addReadLine(f, s)
  assert s == "xa\rb"
  close(f)
  discard tryRemoveFile(path(name))

proc show(s: string): string =
  ## `s` with NUL and CR escaped and runs of a byte written as `a*78`.
  result = ""
  var i = 0
  while i < s.len:
    let c = s[i]
    var run = 1
    while i + run < s.len and s[i + run] == c: inc run
    if c == '\0': result.add "\\0"
    elif c == '\r': result.add "\\r"
    else: result.add c
    if run > 1:
      result.add "*"
      result.add $run
    i = i + run

proc entry(res: var string; line: string) =
  res.add $line.len
  res.add ":\""
  res.add show(line)
  res.add "\" "

type Reader = enum
  byVar, byString, byLines, byAdd

proc readAll(content: string; how: Reader): string {.raises.} =
  ## Writes `content` to a file and reads it back with `how`: each line as
  ## `len:"text"`, then `EOF`.
  let name = tempName()
  writeFile(name, content)
  var f: File
  let opened = open(f, name)
  assert opened
  result = ""
  var n = 0
  case how
  of byVar:
    var line = ""
    while n < 10 and readLine(f, line):
      entry result, line
      inc n
  of byString:
    var atEnd = false
    while n < 10 and not atEnd:
      try:
        let line = readLine(f)
        entry result, line
        inc n
      except ErrorCode as e:
        assert e == EndOfStreamError
        atEnd = true
  of byLines:
    for line in lines(f):
      entry result, line
  of byAdd:
    var line = ""
    while n < 10 and addReadLine(f, line):
      entry result, line
      line = ""
      inc n
  result.add "EOF"
  close(f)
  discard tryRemoveFile(path(name))

proc check(content: string) {.raises.} =
  ## Prints the lines, then the raw lines if `addReadLine` differs.
  let lines = readAll(content, byVar)
  assert readAll(content, byString) == lines
  assert readAll(content, byLines) == lines
  let raw = readAll(content, byAdd)
  if raw == lines: echo lines
  else: echo lines, " | raw ", raw

proc cases() {.raises.} =
  check ""
  check "\n"
  check "\n\n"
  check "abc"
  check repeat('a', 78) & "\n"
  check repeat('a', 78)
  check repeat('a', 79)
  check repeat('a', 126)  # under -d:useLibc, ends inside an fgets chunk
  check repeat('a', 127)
  check repeat('a', 128)
  check repeat('a', 127) & "\n"
  check repeat('a', 300) & "\nb"
  check repeat('a', 79) & "BCDEFGHIJ\n" & repeat('b', 79) & "\n" &
    repeat('c', 80) & "\nabc"
  check "a\0b\n"
  check "q\0"
  check "a\0\nx"
  check repeat('a', 77) & "\0"
  check repeat('a', 78) & "\0"
  check "\0\r\n"
  check "a\r\nb\r\n"
  check "a\r"
  check "\r"
  check "\r\n"
  check "a\rb\n"
  check "a\r\r\n"
  check "a\n\r"
  check repeat('a', 126) & "\r\nb"

try:
  main()
  cases()
except:
  quit "treadline: unexpected error"

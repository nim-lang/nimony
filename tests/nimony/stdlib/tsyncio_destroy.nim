# A `File` closes itself when its last reference goes: what is still buffered
# is written out and the handle is closed, as `close` does. Before, a file
# written and never closed stayed empty, and leaving `lines(filename)` early,
# or raising between `open` and `close`, leaked the handle. And a closed
# `File` kept its handle's number: a second `close`, or a write, reached the
# file that the OS had given that number to since.
#
# The leak checks use POSIX's rule that `open` returns the lowest free
# descriptor: after loops that close everything they open, the next file gets
# the number it got before them. Windows makes no such promise about handle
# values, so there the checks are skipped.

import std/[syncio, paths, appdirs, files]

proc writeUnclosed(name: string) =
  var f: File
  if open(f, name, fmWrite):
    f.write "first\nsecond\n"   # far less than the buffer: still in memory

proc firstLine(name: string): string =
  result = ""
  for line in lines(name):
    result = line
    break                       # leaves before the end of the file

proc countLines(name: string): int =
  ## `lines` leaves the closing to the destructor, also at the end of the file.
  result = 0
  for line in lines(name): inc result

proc raisesWhileOpen(name: string) {.raises.} =
  var f: File
  if open(f, name, fmRead):
    raise ValueError

proc nextHandle(name: string): int =
  ## The handle number the next file gets.
  result = -1
  when defined(posix):
    var f: File
    if open(f, name, fmRead):
      result = int(getFileHandle(f))
      close(f)

proc closeTwice(name: string): bool =
  ## The second `close` must not close the file that got the handle's number.
  result = false
  var a: File
  if open(a, name, fmRead):
    close(a)
    var b: File
    if open(b, name, fmRead):
      close(a)
      var line = ""
      result = readLine(b, line) and line == "first"
      close(b)

proc writeAfterClose(name, other: string): bool {.raises.} =
  ## What is written to a closed `File` must not reach the file that got the
  ## handle's number.
  result = false
  var a: File
  if open(a, name, fmWrite):
    close(a)
    var b: File
    if open(b, other, fmWrite):
      a.write "for a\n"
      flushFile(a)
      close(b)
      result = readFile(other).len == 0

proc main() {.raises.} =
  let name = $(getTempDir() / path("nimony_tsyncio_destroy.txt"))
  let other = $(getTempDir() / path("nimony_tsyncio_destroy2.txt"))
  writeUnclosed(name)
  echo readFile(name).len

  writeFile(name, "first\nsecond\n")
  var before = nextHandle(name)
  var seen = 0
  for i in 0 ..< 10:
    if firstLine(name) == "first": inc seen
  echo seen, " ", nextHandle(name) == before

  before = nextHandle(name)
  var total = 0
  for i in 0 ..< 10: total += countLines(name)
  echo total, " ", nextHandle(name) == before

  before = nextHandle(name)
  for i in 0 ..< 10:
    try: raisesWhileOpen(name)
    except: discard
  echo nextHandle(name) == before

  echo closeTwice(name)
  echo writeAfterClose(other, name)
  discard tryRemoveFile(path(name))
  discard tryRemoveFile(path(other))

try:
  main()
except:
  quit "tsyncio_destroy: unexpected error"

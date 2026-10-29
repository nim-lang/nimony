# syncio's `readLine(f: File): string`: line endings are stripped, an empty
# line is not the end, and the end of the file raises `EndOfStreamError`.

import std/[assertions, os, syncio, files]

proc main() {.raises.} =
  let name = getTempDir() / "nimony_treadline.txt"
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
  discard tryRemoveFile(path(name))

try:
  main()
except:
  quit "treadline: unexpected error"

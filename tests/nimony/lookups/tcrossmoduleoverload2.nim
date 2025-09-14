import std / syncio

proc write(f: File; s: cstring) =
  discard writeBuffer(f, s, s.len)

var s = cstring "abc"
echo s

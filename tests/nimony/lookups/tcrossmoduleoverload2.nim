import std / syncio

proc write*(f: File; s: cstring) =
  discard writeBuffer(f, s, s.len)

proc main =
  var s90 = cstring "abc"
  echo s90

main()

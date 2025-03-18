import std/syncio

proc classify(s: string) =
  case s[0]
  of '_': echo "an identifier"
  of '0', '9': echo "a number"
  else: discard

classify("9123345")

proc classify1(s: string) {.requires: s.len > 0.} =
  case s[0]
  of '_': echo "an identifier"
  of '0' .. '9': echo "a number"
  else: discard

classify1("9123345")

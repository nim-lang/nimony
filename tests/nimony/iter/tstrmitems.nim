# `mitems` on a string: the buffer is copy-on-write, so the walk makes it
# unique first, and a long string's inline prefix cache is synced when the walk
# ends, also on `break` (#2924).
import std/[syncio, assertions, hashes]

proc main =
  # a heap string shared by `var b = a`
  var a = newStringOfCap(64)
  a.add "a much longer string that lives on the heap"
  var b = a
  for c in mitems(b):
    if c == 'a': c = 'A'
  echo a
  echo b

  # a long literal lives in static storage
  var lit = "a much longer string that lives in static storage"
  for c in mitems(lit):
    c = 'X'
    break
  let want = "X much longer string that lives in static storage"
  assert lit == want
  assert hash(lit) == hash(want)
  echo lit

  # short, inline strings
  var s = "ab"
  var t = s
  for c in mitems(t): c = 'z'
  echo s, " ", t

  # empty
  var e = ""
  for c in mitems(e): c = 'q'
  assert e.len == 0

main()

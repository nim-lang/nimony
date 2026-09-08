import std/syncio

# A LONG string keeps its bytes in a heap block reached through `more`, and the
# payload field is a `flexarray` — a type with no size, so it can never be the
# type of a temp: `(var :t (flexarray (c 8)) …)` is `NC8 t[] = …` in C, which
# gcc rejects outright, and a copy of an unknown number of bytes for arkham.
#
# Reading the payload inside a loop is what asks CSE for one: once loop
# unswitching has split the SSO test out of the loop, the long-string copy
# reads `(dot (deref s.more) data)` on a path where it is loop-invariant, and
# CSE hoists it into exactly such a temp unless `classifyLoad` refuses. It did
# refuse objects, unions and arrays — and not flexarrays.
#
# Both string lengths are covered on purpose: the long one drives the heap
# payload, the short one the inline SSO buffer, i.e. both copies of the
# unswitched loop.

proc countMatches(s, t: string): int =
  result = 0
  for i in 0 ..< s.len:
    if s[i] == t[i]: inc result

proc main =
  let a = "a fairly long string, well past the SSO limit"
  var b = a
  b[b.len-1] = 'X'                     # differs in exactly one position
  echo countMatches(a, b) == a.len - 1
  echo a == b
  echo a == a
  let s = "tiny"                       # the SSO side of the same code
  echo countMatches(s, s) == s.len
  echo s == "tiny"

main()

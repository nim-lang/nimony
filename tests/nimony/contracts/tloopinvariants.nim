# Loop invariants the contract prover infers rather than reads off the write
# directions: a bound that holds on entry and that every back-edge restores is
# kept for the body and for the code after the loop.

{.feature: "staticContracts".}

import std/assertions

proc skipSpaces(s: string; start: int): int =
  # `j` only moves under `j < s.len`, so `j <= s.len` survives the loop
  result = 0
  if start < 0 or start > s.len: return
  var j = start
  while j < s.len and s[j] == ' ': inc j
  for k in start ..< j:
    result = result + ord(s[k])

proc lastOfRun(s: string): char =
  # the break leaves `j < s.len`, the guard `j == s.len`: either way `j - 1`
  # indexes `s` once `j > 0`
  result = ' '
  var j = 0
  while j < s.len:
    if s[j] == '.': break
    inc j
  if j > 0:
    result = s[j - 1]

proc fillFrom(dest: var seq[int]; src: seq[int]) =
  # two indexes that move together keep their distance
  if dest.len < src.len: return
  var i = 0
  var o = 0
  while i < src.len:
    dest[o] = src[i]
    inc i
    inc o

proc joinDerived(s: string; i: int): char =
  # one arm states `i < s.len` outright, the other only through `k`
  result = ' '
  if i < 0: return
  var k = 0
  if i < s.len:
    k = i
  else:
    return
  result = s[k]

proc nameCopy(src: ptr UncheckedArray[char]): int =
  # the guard's own bound is a candidate: `i <= 255` after `while i < 255`
  var name = default(array[256, char])
  var i = 0
  while i < 255 and src[i] != '\0':
    name[i] = src[i]
    inc i
  name[i] = '\0'
  result = i

proc matchLen(s: string; i: int): int = (if i mod 3 == 0: 2 else: 0)

proc skipMatches(s: string): int =
  # `inc i, L` under `L > 0` moves `i` up, and `0 <= i` survives it
  result = 0
  var i = 0
  while i < s.len:
    let L = matchLen(s, i)
    if L > 0:
      inc i, L
    else:
      result = result + ord(s[i])
      i = i + 1

assert skipSpaces("  ab", 0) == 64
assert lastOfRun("ab.c") == 'b'
var d = @[0, 0, 0]
fillFrom(d, @[1, 2])
assert d == @[1, 2, 0]
assert joinDerived("xy", 1) == 'y'
var raw = default(array[4, char])
raw[0] = 'a'
raw[1] = 'b'
assert nameCopy(cast[ptr UncheckedArray[char]](addr raw)) == 2
assert skipMatches("abcd") == ord('c')

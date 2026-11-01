# bug #2838: whether a guarded index proved depended on unrelated declarations.
# `boundsFrom` recorded a chain whose constant overflowed `xint` as NaN, and NaN
# compares false against everything, so the real `dst <= p.len - 1` chain found
# later could never replace it. Which chain came first followed the order of
# the facts, which followed symbol numbering — the unused `filler`s below are
# what made the loop fail (6 of them, with this exact file, before the fix).
{.feature: "staticContracts".}
func filler0(x: int): int = x + 0
func filler1(x: int): int = x + 1
func filler2(x: int): int = x + 2
func filler3(x: int): int = x + 3
func filler4(x: int): int = x + 4
func filler5(x: int): int = x + 5
proc replaceRange(p: var string; first, last: int; s: string): bool =
  ## Replaces p[first ..< last] with s; false does not promise rollback.
  let oldLen = p.len
  if first < 0 or last < first or oldLen < last: return false
  let delta = s.len - (last - first)
  if delta > 0:
    if oldLen > high(int) - delta: return false
    p.setLen oldLen + delta
    if isOom(p) or p.len != oldLen + delta: return false
    var src = oldLen - 1
    var dst = src + delta
    while src >= last and src >= 0 and src < p.len and dst >= 0 and dst < p.len:
      p[dst] = p[src]
      dec src
      dec dst
  elif delta < 0:
    var src = last
    var dst = last + delta
    while src < oldLen and src < p.len and dst >= 0 and dst < p.len:
      p[dst] = p[src]
      inc src
      inc dst
    p.setLen oldLen + delta
    if isOom(p) or p.len != oldLen + delta: return false
  var k = 0
  var dst = first
  while k < s.len and dst >= 0 and dst < p.len:
    p[dst] = s[k]
    inc k
    inc dst
  result = k == s.len

import std/assertions

var s = "abcdef"
assert replaceRange(s, 1, 3, "XYZW")
assert s == "aXYZWdef"
assert replaceRange(s, 1, 5, "q")
assert s == "aqdef"

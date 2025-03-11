#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Interval computations for `case` statement checks.

import std / [assertions]

proc containsOrIncl*[T](s: var seq[(T, T)]; c: T): bool =
  mixin succ, pred
  var patched = false
  for i in 0..<s.len:
    let (x, y) = s[i]
    if x <= c and c <= y:
      return true
    elif succ(y) == c and not patched:
      s[i][1] = c
      # but continue the search loop!
      patched = true
    elif pred(x) == c and not patched:
      s[i][0] = c
      # but continue the search loop!
      patched = true
  if not patched:
    s.add (c, c)
  return false

proc contains*[T](s: seq[(T, T)]; c: T): bool =
  for i in 0..<s.len:
    let (x, y) = s[i]
    if x <= c and c <= y:
      return true
  return false

proc doesOverlapOrIncl*[T](s: var seq[(T, T)]; c, d: T): bool =
  mixin succ, pred
  assert c <= d
  if c == d:
    return containsOrIncl(s, c)

  for i in 0..<s.len:
    let (x, y) = s[i]
    # X..Y and C..D overlap iff (X <= D and C <= Y)
    if x <= d and c <= y:
      s[i][0] = min(x, c)
      s[i][1] = max(y, d)
      return true
  s.add (c, d)
  return false

proc excl*[T](s: var seq[(T, T)]; c, d: T) =
  mixin succ, pred
  let len = s.len
  var i = 0
  while i < len:
    let (x, y) = s[i]
    # X..Y and C..D overlap iff (X <= D and C <= Y)
    if x <= d and c <= y:
      let split = (succ(d), y)
      if c > x:
        s[i][1] = pred(c)
        if split[0] <= split[1]:
          s.add split
        inc i
      else:
        if split[0] <= split[1]:
          s[i] = split
          inc i
        else:
          s.del i
    else:
      inc i

proc excl*[T](s: var seq[(T, T)]; without: seq[(T, T)]) =
  for i in 0..<without.len:
    let (x, y) = without[i]
    excl s, x, y

when isMainModule:
  var x = @[(1, 2), (5, 10)]
  #x.incl 4
  #echo x
  assert x.doesOverlapOrIncl(6, 12)
  assert not x.doesOverlapOrIncl(4, 4)
  #echo x
  assert not x.doesOverlapOrIncl(49, 400)
  x.excl 4, 4
  x.excl 51, 60
  assert $x == "@[(1, 2), (5, 12), (49, 50), (61, 400)]"

  assert x.containsOrIncl 330

  var y = @[(1, 2)]
  assert y.containsOrIncl(2)

  block:
    var x: seq[(int, int)] = @[]
    assert not x.doesOverlapOrIncl(0, 1) # false
    assert not x.containsOrIncl(3) # false
    #echo x.contains 2
    assert not x.containsOrIncl(2) # false
    #echo x

# What a write through a pointer cannot reach keeps its facts: a local whose
# address is never taken, and a `var` parameter across a pure call. Near
# misses of `tests/nimony/errmsgs/taliaswrite.nim`.

{.feature: "staticContracts".}

import std/syncio

const Big = 1000000000

func pure(a: int): int = a + 1

proc noAddr(a: int; arr: array[10, int]): int =
  var x = a
  var y = 0
  let q = addr y
  result = 0
  if x >= 0 and x < 10:
    q[] = Big
    result = arr[x]

var g = 2
proc setg() =
  {.cast(assumeSync).}:
    g = 1000000000

proc forGlobalBound(arr: array[10, int]): int =
  # an iterator gets its arguments once: the loop counts with what `g` held
  # at the `for`, whatever the body does to `g`
  result = 0
  {.cast(assumeSync).}:
    {.assume: 0 <= g.}
    for i in g ..< 4:
      setg()
      result += arr[i]

proc varParam(x: var int; arr: array[10, int]): int =
  result = 0
  if x >= 0 and x < 10:
    discard pure(x)
    result = arr[x]

proc viewLen(z: var openArray[int]) {.requires: 2 <= z.len.} =
  var big = Big
  copyMem(addr big, addr big, sizeof(int))
  z[1] = z[0]

var arr = [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]
var v = 3
var s = [1, 2, 3]
echo noAddr(3, arr)
echo varParam(v, arr)
echo forGlobalBound(arr)
viewLen(s)
echo s[1]

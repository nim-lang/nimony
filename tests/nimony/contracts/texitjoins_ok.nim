# The proven side of `tests/nimony/errmsgs/tforexit.nim` and `ttryentry.nim`:
# what holds on every way out of a `for` loop, or into an `except`/`finally`,
# is still known there. Facts about locations a loop or a `try` body never
# writes survive it.

{.feature: "staticContracts".}

import std/syncio

proc risky(x: int) {.raises.} =
  if x > 3: raise Failure

proc sumUntil3(s: seq[int]): int =
  result = 0
  if s.len > 2:
    for k in 0 ..< s.len:
      if s[k] == 3: break
      result += s[k]
    result += s[2]                # `s.len > 2` holds after both exits

proc firstAt(s: seq[int]; x: int): int =
  result = -1
  for k in 0 ..< s.len:
    if s[k] == x:
      result = k
      break
  if result >= 0 and result < s.len:
    result = s[result]            # proven under the guard, as for a `while`

proc guarded(s: seq[int]; x: int): int =
  result = 0
  if x >= 0 and x < s.len:
    var y = 0
    try:
      y = x
      risky(x)
      result = s[x]
    except:
      result = s[x]               # `x` and `s` are not written in the body
    finally:
      echo s[x]
    result += s[x]

# A `finally` (and so a `defer`) runs on every way out of its `try`, a
# `return` or a `break` included: what is initialized on each of those ways is
# initialized in it.

proc deferReadsResult(): int =
  defer: echo "defer ", result
  result = 3

proc earlyReturn(c: bool): int =
  defer: echo "defer ", result
  if c: return 5
  result = 6

proc viaReturn(): int =
  var y: int
  try:
    y = 4
    return y
  finally:
    echo "finally ", y

proc viaBreak(): int =
  var y: int
  for i in 0 .. 2:
    try:
      y = i + 7
      break
    finally:
      echo "finally ", y
  result = 0

# An exit taken before the `try` does not lead into its `finally`.

proc returnBeforeDefer(c: bool): int =
  var y: int
  if c: return 0
  defer: echo "defer ", y
  y = 1
  result = y

proc returnBeforeTry(c: bool): int =
  var y: int
  if c: return 0
  try:
    y = 4
    return y
  finally:
    echo "finally ", y

proc breakBeforeTry(): int =
  var y: int
  for i in 0 .. 2:
    if i == 5: break
    try:
      y = i + 7
      break
    finally:
      echo "finally ", y
  result = 0

proc bareReturnBeforeDefer(c: bool): int =
  result = 0
  var y: int
  if c: return
  defer: echo "defer ", y
  y = 2
  result = y

# An exit through a `finally` keeps every fact about what the `finally` does
# not write, and each `return` is judged on its own. The refused side is in
# `tests/nimony/errmsgs/ttryentry.nim`.

proc breakKeeps(): int =
  var i = 0
  block b:
    try:
      i = 7
      break b
    finally:
      echo "finally"
  {.assert: i == 7.}
  result = i

proc returnValue(): int {.ensures: result == 7.} =
  try:
    return 7
  finally:
    echo "finally"

proc deferKeeps(): int {.ensures: result == 3.} =
  var log = 0
  defer: log = 1
  result = 3

proc nested(): int {.ensures: result == 4.} =
  var y = 0
  try:
    try:
      result = 4
      return
    finally:
      y = 1
  finally:
    y = 2

proc eachReturn(x: int): int {.ensures: result == 1 or result == 2.} =
  try:
    risky(x)
    return 1
  except:
    return 2
  finally:
    echo "finally"

# Under a `finally`, `return x` is `result = x`: its value is judged in
# `result`, from what `x` says, including through the old `result`.

proc returnSum(s: seq[int]; a: range[0..5]): int {.ensures: result >= 0.} =
  try:
    return s.len + a
  finally:
    echo "finally"

proc returnProduct(a: range[0..5]): int {.ensures: result >= 0.} =
  try:
    return a * 2
  finally:
    echo "finally"

proc returnFromResult(): int {.ensures: result == 1.} =
  result = 0
  try:
    return result + 1
  finally:
    discard

type Box = object
  f: int

proc returnObject(v: range[0..5]): Box {.ensures: result.f >= 0.} =
  try:
    return Box(f: v)
  finally:
    echo "finally"

echo sumUntil3(@[1, 2, 3, 4]), " ", firstAt(@[5, 6, 7], 6), " ", firstAt(@[5], 9)
echo guarded(@[1, 2, 3], 1)
echo guarded(@[1, 2, 3, 4, 5], 4)
echo deferReadsResult()
echo earlyReturn(true)
echo earlyReturn(false)
echo viaReturn()
echo viaBreak()
echo returnBeforeDefer(false)
echo returnBeforeTry(false)
echo breakBeforeTry()
echo bareReturnBeforeDefer(false)
echo breakKeeps()
echo returnValue()
echo deferKeeps()
echo nested()
echo eachReturn(1)
echo eachReturn(9)
echo returnSum(@[1], 2)
echo returnProduct(2)
echo returnFromResult()
echo returnObject(3).f

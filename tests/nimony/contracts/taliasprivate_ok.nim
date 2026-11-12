# What the aliasing rules keep (doc/language.md, "Aliasing"); every index
# here is proven in a `staticContracts` module.
{.feature: "staticContracts".}
{.feature: "assumeSync".}
import std/syncio

var gs = @[1, 2, 3]
var arr: array[10, int]

proc log(msg: string) = discard

func pick(a: int; b: int): int = a + b

proc localAsVar(): int =
  # a local passed as `var` stays private: what `[]=` promises holds after it
  var s = @[1, 2, 3]
  result = 0
  for i in 0 ..< s.len:
    s[i] = s[i] + 1
    result = result + s[i]

proc funcKeepsBorrowed(v: var seq[int]): int =
  # a `func` without a `var` or pointer argument writes nothing behind a `ref`
  result = 0
  if v.len > 2:
    let k = pick(1, 2)
    result = v[2] + k

proc accessorKeepsGlobal(): int =
  # `len` is a pure accessor: no call to drop the global's fact
  result = 0
  if gs.len > 1:
    let n = gs.len
    if n > 1: result = gs[1]

proc copyKeeps(): int =
  # copied into a local, the global's value survives the call
  let s = gs
  result = 0
  if s.len > 2:
    log("x")
    result = s[2]

proc elementAddr(s: var seq[int]): int =
  # `addr s[0]` points into the buffer, not into `s`'s own storage
  result = 0
  if s.len > 1:
    let p = addr s[0]
    result = s[1] + p[]

proc secondVariable(a: int): int =
  # the address of `y`, a copy, leaves `x` alone
  var x = a
  var y = x
  let p = addr y
  p[] = 1000000000
  result = 0
  if x >= 0 and x < 10:
    result = arr[x]

var s2 = @[4, 5]
var s3 = @[4, 5, 6]
echo localAsVar(), " ", funcKeepsBorrowed(s3), " ", accessorKeepsGlobal(),
  " ", copyKeeps(), " ", elementAddr(s2), " ", secondVariable(3)

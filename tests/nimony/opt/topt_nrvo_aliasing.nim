# A >16B result built in place in the caller's buffer (arkham's NRVO,
# `lengaa.nrvoLocal`) is only sound when the caller passes a buffer the callee
# cannot reach (`lengaa.destMayBeObserved`). Every callee here writes `result`
# before it is done reading its input; each call site hands it a destination
# that input aliases: the argument itself, a global, a local whose address is
# taken. The `Inl` twins
# are small enough to be spliced: the inliner's destination forwarding asks the
# same question.
#
# Repro:   bin/nimony n --opt:speed tests/nimony/opt/topt_nrvo_aliasing.nim
{.feature: "assumeSync".}
import std/syncio

type Big = object
  a, b, c, d: int

proc show(x: Big) = echo x.a, " ", x.b, " ", x.c, " ", x.d

proc swapped(x: Big): Big {.noinline.} =
  # writes `result` before it is done reading `x`
  result = default(Big)
  result.a = x.d
  result.b = x.c
  result.c = x.b
  result.d = x.a

var gBig = Big(a: 1, b: 2, c: 3, d: 4)

proc fromGlobal(): Big {.noinline.} =
  result = default(Big)
  result.a = gBig.d
  result.b = gBig.c
  result.c = gBig.b
  result.d = gBig.a

proc throughPtr(p: ptr Big): Big {.noinline.} =
  result = default(Big)
  result.a = p.d
  result.b = p.c
  result.c = p.b
  result.d = p.a

proc early(x: int): Big {.noinline.} =
  result = Big(a: x, b: x, c: x, d: x)
  if x > 3: return Big(a: 9, b: 8, c: 7, d: 6)
  result.b = x * 2

proc nested(x: Big): Big {.noinline.} =
  # `result = g(...)` inside a proc that itself builds in place
  result = swapped(x)
  result.a = result.a + 100

proc nestedSelf(x: Big): Big {.noinline.} =
  result = Big(a: 1, b: 2, c: 3, d: 4)
  result = swapped(result)       # mentions the destination

proc addrOfResult(x: int): Big {.noinline.} =
  result = Big(a: x, b: 0, c: 0, d: 0)
  let p = addr result
  p.b = 5
  result.c = p.a + p.b

proc swappedInl(x: Big): Big =
  result = default(Big)
  result.a = x.d
  result.d = x.a

proc throughPtrInl(p: ptr Big): Big =
  result = default(Big)
  result.a = p.d
  result.d = p.a

proc inlined =
  var y = Big(a: 1, b: 2, c: 3, d: 4)
  y = swappedInl(y)
  show y
  var z = Big(a: 5, b: 6, c: 7, d: 8)
  let pz = addr z
  z = throughPtrInl(pz)
  show z

proc main =
  var y = Big(a: 1, b: 2, c: 3, d: 4)
  y = swapped(y)
  show y
  gBig = fromGlobal()
  show gBig
  var z = Big(a: 5, b: 6, c: 7, d: 8)
  let pz = addr z
  z = throughPtr(pz)
  show z
  show early(2)
  show early(5)
  show nested(Big(a: 1, b: 2, c: 3, d: 4))
  show nestedSelf(y)
  show addrOfResult(7)

main()
inlined()

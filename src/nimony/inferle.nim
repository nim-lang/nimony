#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Infer `le` ("less or equal") properties for compile-time array index checking
## and also for "not nil" checking.

import std/assertions
import xints

type
  VarId* = distinct int32  # convention: VarId(0) is always the constant 0!

proc `==`*(a, b: VarId): bool {.borrow.}

type
  LeXplusC* = object    # semantics: a <= b + c
    a*, b*: VarId
    c*: xint

  Facts* = object
    x: seq[LeXplusC]

  RestorePoint* = object
    xlen: int

const
  InvalidVarId* = VarId(-1)

proc isValid*(x: LeXplusC): bool {.inline.} =
  result = x.a != InvalidVarId and x.b != InvalidVarId and not isNaN(x.c)

proc len*(f: Facts): int {.inline.} = f.x.len
proc `[]`*(f: Facts; i: int): lent LeXplusC {.inline.} = f.x[i]

proc `$`*(f: LeXplusC): string =
  let a = if f.a == VarId(0): "0" else: "v" & $f.a.int
  let b = if f.b == VarId(0): "0" else: "v" & $f.b.int
  result = a & " <= " & b & " + " & $f.c

proc shrink*(f: var Facts; newLen: int) {.inline.} =
  f.x.shrink newLen

#[

We use the same inference engine for not-nil checking. We map nil to 0 and then
x != 0 is x > 0 which is x >= 1 which becomes 0 <= x - 1. This is convoluted
but it works.

]#

proc add*(f: var Facts; elem: LeXplusC) =
  f.x.add elem

proc isNotNil*(a: VarId): LeXplusC =
  LeXplusC(a: VarId(0), b: a, c: createXint(-1'i64))

proc isNil*(a: VarId): LeXplusC =
  LeXplusC(a: a, b: VarId(0), c: createXint(0'i64)) # a <= 0

proc save*(f: Facts): RestorePoint = RestorePoint(xlen: f.x.len)

proc restore*(f: var Facts; r: RestorePoint) =
  f.x.shrink r.xlen

proc addLeFact*(f: var Facts; a, b: VarId; c: xint = createXint(0'i64)) =
  # add to the knowledge base that `a <= b + c`.
  f.x.add LeXplusC(a: a, b: b, c: c)

proc query*(a, b: VarId; c: xint = createXint(0'i64)): LeXplusC =
  result = LeXplusC(a: a, b: b, c: c)

proc createFacts*(): Facts =
  result = Facts()
  # VarId(0) is always mapped to zero so we know that `v0 <= v0 + 0`:
  result.x.add LeXplusC(a: VarId(0), b: VarId(0), c: createXint(0'i64))

proc geXplusC*(f: LeXplusC): LeXplusC =
  # a >= b + c  --> b + c <= a  --> b <= a - c
  result = LeXplusC(a: f.b, b: f.a, c: -f.c)

proc ltXplusC*(f: LeXplusC): LeXplusC =
  # a < b + c  --> a <= b + c - 1
  result = LeXplusC(a: f.a, b: f.b, c: f.c - createXint(1'i64))

proc negateFact*(f: var LeXplusC) =
  # not (a <= b + c)
  # -->
  # a > b + c
  # a >= b + c + 1
  # a - c - 1 >= b
  # b <= a - c - 1
  f.c = -f.c - createXint(1'i64)
  swap f.a, f.b

proc negateFacts*(f: var Facts; start: int) =
  for i in start ..< f.x.len:
    negateFact(f.x[i])

proc variableChangedByDiff*(f: var Facts; x: VarId; diff: xint) =
  # after `inc x` we know that x is now bigger by 1 so all
  # Facts like `x <= b + c` are then `x <= b + c + 1`:
  for i in 0 ..< f.x.len:
    if f.x[i].a == x:
      if f.x[i].b == x: discard "nothing to do; x <= x + c <-> x+1 <= x+1 + c"
      else: f.x[i].c = f.x[i].c + diff
    elif f.x[i].b == x:
      f.x[i].c = f.x[i].c - diff

proc invalidateFactsAbout*(f: var Facts; x: VarId) =
  var i = 0
  while i < f.x.len:
    if f.x[i].a == x or f.x[i].b == x:
      del f.x, i
    else:
      inc i

proc simpleImplies(facts: Facts; v: LeXplusC): bool =
  for f in facts.x:
    if f.a == v.a and f.b == v.b:
      # if we know that  a <= b + 3 we can infer that a <= b + 4
      if f.c <= v.c: return true
  return false

import std/intsets

type
  Path = object
    visited: IntSet
    a: seq[int]

proc incl(p: var Path; elem: int) =
  p.visited.incl elem
  p.a.add elem

proc excl(p: var Path; elem: int) =
  p.visited.excl elem
  discard p.a.pop

#[

There is a single inference rule:

  a <= b + 4
  b <= c + 5

-->

  a <= c + 5 + 4

]#

proc traverseAllPathsUtil(facts: Facts; u: VarId; d: LeXplusC; nodeIndex: int;
                          p: var Path; res: var bool) =
  p.incl nodeIndex
  if u == d.b:
    # See if the solution suits us:
    var sum = createXint(0'i64)
    for j in p.a:
      sum = sum + facts.x[j].c
    if sum <= d.c: res = true
  else:
    for i in 0..high(facts.x):
      if i notin p.visited:
        if facts.x[i].a == u:
          traverseAllPathsUtil(facts, facts.x[i].b, d, i, p, res)
  excl p, nodeIndex

proc traverseAllPaths(facts: Facts; s: VarId; d: LeXplusC; res: var bool) =
  var path = Path()

  for i in 0..high(facts.x):
    if facts.x[i].a == s:
      traverseAllPathsUtil(facts, facts.x[i].b, d, i, path, res)

proc complexImplies(facts: Facts; v: LeXplusC): bool =
  result = false
  traverseAllPaths(facts, v.a, v, result)

proc implies*(facts: Facts; v: LeXplusC): bool =
  assert v.isValid
  result = simpleImplies(facts, v) or complexImplies(facts, v)

proc merge*(x: Facts; xstart: int; y: Facts; negate: bool): Facts =
  # computes thing we know on a joint point.
  # we know that `a <= b + c` or `a <= b + d` then we know
  # that `a <= b + max(c, d)`
  result = Facts()
  for i in 0 ..< xstart:
    for j in 0..<y.len:
      let ya = y[j]
      if x[i].a == ya.a and x[i].b == ya.b:
        result.x.add LeXplusC(a: x[i].a, b: x[i].b, c: max(x[i].c, ya.c))

  if negate and x.len - xstart > 1:
    # negation of (a and b) would be (not a or not b) so we cannot model that:
    discard "must lose information here"
  else:
    for i in xstart ..< x.len:
      for j in 0..<y.len:
        var ya = y[j]
        if negate:
          ya.negateFact()
        if x[i].a == ya.a and x[i].b == ya.b:
          result.x.add LeXplusC(a: x[i].a, b: x[i].b, c: max(x[i].c, ya.c))

when isMainModule:
  proc main =
    let a = VarId(1)
    let b = VarId(2)
    let d = VarId(3)
    let z = VarId(4)

    let facts = Facts(x: @[
      LeXplusC(a: a, b: b, c: createXint 0'i64),  # a <= b + 0
      LeXplusC(a: b, b: d, c: createXint 13'i64), # b <= d + 13
      LeXplusC(a: d, b: z, c: createXint 34'i64)  # d <= z + 24
      ])

    echo facts.implies LeXplusC(a: a, b: z, c: createXint 443'i64) # a <= z + 443 ?

  main()

  proc betterTest() =
    let a = VarId(1)
    let b = VarId(2)
    let d = VarId(3)
    let b2 = VarId(4)
    let z = VarId(0)

    var f = createFacts()
    # a is zero:
    f.addLeFact(a, z) # a <= 0
    f.addLeFact(z, a) # 0 <= a

    f.addLeFact(b, a) # b <= a
    f.addLeFact b2, b
    # d <= b2 <= b <= a

    f.addLeFact(d, b2) # d <= b2

    echo f.implies LeXplusC(a: d, b: b) # d <= b?

    # a <= 4?
    echo f.implies LeXplusC(a: a, b: z, c: createXint 4'i64)

    let p = VarId(9)
    f.add isNotNil(p)
    echo f.implies isNotNil(p) # true
    echo f.implies isNil(p) # false

  betterTest()

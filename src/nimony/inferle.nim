#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Infer `le` ("less or equal") properties for compile-time array index checking
## and also for "not nil" checking.

import std/[assertions, tables, hashes]
import xints

type
  VarId* = distinct int32  # convention: VarId(0) is always the constant 0!

func `==`*(a, b: VarId): bool {.borrow.}
func hash*(x: VarId): Hash {.borrow.}

type
  LeXplusC* = object    # semantics: a <= b + c
    a*, b*: VarId
    c*: xint

  JOp = enum jAdd, jMod, jDel
  JEntry = object
    op: JOp
    idx: int            ## slot index (jMod/jDel)
    val: LeXplusC       ## previous value at `idx` (jMod) / removed value (jDel)

  Facts* = object
    x: seq[LeXplusC]
    journal: seq[JEntry]   ## append-only undo log; populated only when `journaling`
    journaling: bool       ## opt-in: enables O(writes) checkpoint/rollback

  RestorePoint* = object
    snapshot: seq[LeXplusC]

const
  InvalidVarId* = VarId(-1)

# --- Journaled checkpoint / rollback (an undo log, not a whole-state copy) ---
#
# A `Tracker`-style journal (cf. `lengc/shoggoth/trackers.nim`): every mutation
# logs how to undo it, so saving/restoring across a branch is O(writes-since)
# instead of copying the whole fact set. This is what keeps a deeply-nested
# `if/elif` chain from costing O(depth * facts) memory.

proc enableJournaling*(f: var Facts) {.inline.} =
  f.journaling = true

proc checkpoint*(f: Facts): int {.inline.} =
  ## A token for `rollbackTo`. Cheap (just the log length).
  f.journal.len

proc snapshotFacts*(f: Facts): Facts =
  ## A plain (non-journaling) copy of the fact set — used only where two branch
  ## states must coexist (the `merge` at a real confluence), never per frame.
  Facts(x: f.x)

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

proc jAppend(f: var Facts; v: LeXplusC) {.inline.} =
  if f.journaling: f.journal.add JEntry(op: jAdd)
  f.x.add v

proc jSet(f: var Facts; i: int; v: LeXplusC) {.inline.} =
  if f.journaling: f.journal.add JEntry(op: jMod, idx: i, val: f.x[i])
  f.x[i] = v

proc jSwapRemove(f: var Facts; i: int) =
  ## Order-independent removal (facts are a set): move the last element into
  ## the hole. Reversible from the logged `(idx, removed)` alone.
  let removed = f.x[i]
  if f.journaling: f.journal.add JEntry(op: jDel, idx: i, val: removed)
  let last = f.x.len - 1
  if i != last: f.x[i] = f.x[last]
  f.x.setLen last

proc rollbackTo*(f: var Facts; cp: int) =
  ## Undo every mutation logged since `cp` (a `checkpoint`). O(writes-since).
  var i = f.journal.len - 1
  while i >= cp:
    let e = f.journal[i]
    case e.op
    of jAdd: f.x.setLen(f.x.len - 1)
    of jMod: f.x[e.idx] = e.val
    of jDel:
      if e.idx < f.x.len:
        f.x.add f.x[e.idx]      # move the swapped-in element back to the end
        f.x[e.idx] = e.val      # ... and restore the removed value
      else:
        f.x.add e.val           # idx had been the last slot; nothing was moved
    dec i
  f.journal.setLen cp

proc removeFactAt*(f: var Facts; i: int) {.inline.} =
  ## Journaled, order-independent removal of slot `i` (the element swapped into
  ## `i` must be re-examined by the caller).
  f.jSwapRemove i

proc clearJournaled*(f: var Facts) =
  ## Drop all facts except the `v0 <= v0` anchor — "know nothing" — journaled,
  ## so it is undoable inside an enclosing branch.
  var i = 0
  while i < f.x.len:
    if f.x[i].a == VarId(0) and f.x[i].b == VarId(0):
      inc i
    else:
      f.jSwapRemove(i)          # element swapped into `i` still needs checking

proc add*(f: var Facts; elem: LeXplusC) =
  f.jAppend elem

proc isNotNil*(a: VarId): LeXplusC =
  LeXplusC(a: VarId(0), b: a, c: createXint(-1'i64))

proc isNil*(a: VarId): LeXplusC =
  LeXplusC(a: a, b: VarId(0), c: createXint(0'i64)) # a <= 0

proc save*(f: Facts): RestorePoint = RestorePoint(snapshot: f.x)

proc restore*(f: var Facts; r: RestorePoint) =
  f.x = r.snapshot

proc addLeFact*(f: var Facts; a, b: VarId; c: xint = createXint(0'i64)) =
  # add to the knowledge base that `a <= b + c`.
  f.jAppend LeXplusC(a: a, b: b, c: c)

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
    var v = f.x[i]
    negateFact(v)
    f.jSet i, v

proc variableChangedByDiff*(f: var Facts; x: VarId; diff: xint) =
  # after `inc x` we know that x is now bigger by 1 so all
  # Facts like `x <= b + c` are then `x <= b + c + 1`:
  for i in 0 ..< f.x.len:
    if f.x[i].a == x:
      if f.x[i].b == x: discard "nothing to do; x <= x + c <-> x+1 <= x+1 + c"
      else:
        var v = f.x[i]; v.c = v.c + diff; f.jSet i, v
    elif f.x[i].b == x:
      var v = f.x[i]; v.c = v.c - diff; f.jSet i, v

proc invalidateFactsAbout*(f: var Facts; x: VarId) =
  var i = 0
  while i < f.x.len:
    if f.x[i].a == x or f.x[i].b == x:
      f.jSwapRemove i        # order-independent; element swapped into i rechecked
    else:
      inc i

proc simpleImplies(facts: Facts; v: LeXplusC): bool =
  for f in facts.x:
    if f.a == v.a and f.b == v.b:
      # if we know that  a <= b + 3 we can infer that a <= b + 4
      if f.c <= v.c: return true
  return false

#[
  Inference rule: a <= b + c1,  b <= d + c2  -->  a <= d + (c1+c2)
  We need: is there a path from v.a to v.b with total weight <= v.c?
  Use Bellman-Ford for shortest path (handles negative weights).
]#

proc complexImplies(facts: Facts; v: LeXplusC): bool =
  var dist = initTable[VarId, xint]()
  dist[v.a] = createXint(0'i64)
  let maxIter = max(facts.x.len, 1) * 2  # enough for longest path
  for _ in 0 ..< maxIter:
    var changed = false
    for f in facts.x:
      if f.a in dist:
        let newDist = dist.getOrDefault(f.a) + f.c
        if f.b notin dist or newDist < dist.getOrDefault(f.b):
          dist[f.b] = newDist
          changed = true
    if not changed: break
  result = v.b in dist and dist.getOrDefault(v.b) <= v.c

proc implies*(facts: Facts; v: LeXplusC): bool =
  assert v.isValid
  result = simpleImplies(facts, v) or complexImplies(facts, v)

proc consider(best: var Table[(VarId, VarId), xint]; a, b: VarId; c: xint) =
  let key = (a, b)
  if key notin best or c < best.getOrDefault(key):
    best[key] = c

proc merge*(x: Facts; xstart: int; y: Facts; negate: bool): Facts =
  # At a join we know facts that hold in BOTH branches.
  # For a<=b+c: intersection uses min(c1,c2) (strongest constraint).
  # Deduplicate: at most one fact per (a,b).
  var best = initTable[(VarId, VarId), xint]()

  for i in 0 ..< xstart:
    for j in 0..<y.len:
      let ya = y[j]
      if x[i].a == ya.a and x[i].b == ya.b:
        consider(best, x[i].a, x[i].b, min(x[i].c, ya.c))

  if negate and x.len - xstart > 1:
    discard "negation of (a and b) would be (not a or not b) so we cannot model that"
  else:
    for i in xstart ..< x.len:
      for j in 0..<y.len:
        var ya = y[j]
        if negate:
          ya.negateFact()
        if x[i].a == ya.a and x[i].b == ya.b:
          consider(best, x[i].a, x[i].b, min(x[i].c, ya.c))

  result = Facts()
  for key, c in best:
    result.x.add LeXplusC(a: key[0], b: key[1], c: c)

when isMainModule:
  import std/syncio
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

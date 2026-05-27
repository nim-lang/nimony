#
#
#           NIFC Branch-Aware Tracker
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A `Tracker[K, V]` is a `Table[K, V]` that knows about structured branches
## and forward jumps. It is the substrate for tree-walking optimizer passes
## that need flow-sensitive information across `if` / `case` joins and across
## `jmp` / `lab` boundaries.
##
## Merge rule at every join: a key keeps its value iff every contributing
## branch has the same value for it; on any disagreement (or when a branch
## did not touch the key but the pre-branch value differs) the key collapses
## to `default(V)`, which is treated as "absent".
##
## This rule covers the common presence-style facts directly:
##
## - `V = bool` for arcopt's "moved" set or a kill-verifier: AND of branches.
## - `V = Option[T]` for CSE-availability: `Some(t)` iff all branches agree.
## - any other type whose `default(V)` means "absent" and where intersection
##   by equality is the semantically right merge.
##
## Versioning (new-identity allocation on merge) is *not* served by this
## type — it is a fundamentally different operation and belongs in its own
## structure.
##
## ## Sibling groups
##
## `if` / `case` are walked as:
##
## ```
## t.openSiblings()
## for each branch:
##   t.enterSibling()
##   ...walk branch body...
##   t.leaveSibling()         # or t.gotoLabel(L) if the branch ends in a jump
## t.joinSiblings()
## ```
##
## ## Forward jumps and labels
##
## A branch that ends in `jmp L` calls `gotoLabel L` instead of
## `leaveSibling`; its delta is stashed under `L` and dropped from the
## current sibling group's contribution. When the walker reaches the matching
## `lab L`, it calls `landLabel L`, which folds the stashed deltas plus the
## current fall-through state together with the same intersection rule.
##
## ## Loops
##
## `clearAll` discards every tracked value (logging the writes, so the loss
## of information is itself revertible inside a sibling). Loops should call
## it on entry and on exit: the body may iterate zero or many times, so the
## tracker cannot soundly carry facts across the back-edge.

import std / [tables, sets, hashes, assertions]

type
  LabelId* = distinct uint32

proc `==`*(a, b: LabelId): bool {.borrow.}
proc hash*(a: LabelId): Hash {.borrow.}

type
  LogEntry[K, V] = object
    key: K
    prev, next: V

  SibGroup = object
    sibStarts: seq[int]   ## one entry per closed fall-through sibling; each
                          ## value is the index into `log` at which that
                          ## sibling's slice begins. The slice ends at the
                          ## next entry, or at the open sibling's start, or
                          ## at `log.len` if no sibling is currently open.
    sibOpen: bool
    curSibStart: int      ## valid only if `sibOpen`

  Tracker*[K, V] = object
    base: Table[K, V]                 ## current view (non-default entries only)
    log: seq[LogEntry[K, V]]          ## append-only journal of writes
    groups: seq[SibGroup]             ## stack of open `openSiblings` scopes
    labels: Table[LabelId, seq[Table[K, V]]]  ## incoming deltas per label

proc initTracker*[K, V](): Tracker[K, V] =
  Tracker[K, V]()

# ---- table-like surface ----------------------------------------------------

proc `[]`*[K, V](t: Tracker[K, V]; k: K): V {.inline.} =
  t.base.getOrDefault(k)

proc contains*[K, V](t: Tracker[K, V]; k: K): bool {.inline.} =
  k in t.base

proc `[]=`*[K, V](t: var Tracker[K, V]; k: K; v: V) =
  ## Set `k` to `v`. Writing `default(V)` removes the entry. Writing the
  ## value the key already holds is a no-op (no log entry is appended).
  let prev = t.base.getOrDefault(k)
  if prev == v: return
  t.log.add LogEntry[K, V](key: k, prev: prev, next: v)
  if v == default(V):
    t.base.del k
  else:
    t.base[k] = v

iterator pairs*[K, V](t: Tracker[K, V]): (K, V) =
  for k, v in t.base.pairs:
    yield (k, v)

proc clearAll*[K, V](t: var Tracker[K, V]) =
  ## Drop every tracked value to `default(V)`, logging each write so the
  ## change is undoable inside an enclosing sibling.
  var keys = newSeqOfCap[K](t.base.len)
  for k in t.base.keys: keys.add k
  for k in keys:
    t[k] = default(V)

# ---- internal helpers ------------------------------------------------------

proc revertFrom[K, V](t: var Tracker[K, V]; start: int) =
  ## Restore `base` to its state at log index `start` by walking the slice
  ## backward and replaying `prev` values. Does not shrink the log.
  for i in countdown(t.log.high, start):
    let e = t.log[i]
    if e.prev == default(V):
      t.base.del e.key
    else:
      t.base[e.key] = e.prev

proc finalValues[K, V](t: Tracker[K, V]; lo, hi: int): Table[K, V] =
  ## Last-write-wins map for log[lo ..< hi]. Computed by a backward walk.
  result = initTable[K, V]()
  for i in countdown(hi - 1, lo):
    let e = t.log[i]
    if e.key notin result:
      result[e.key] = e.next

# ---- sibling-group operations ---------------------------------------------

proc openSiblings*[K, V](t: var Tracker[K, V]) =
  t.groups.add SibGroup(sibStarts: @[], sibOpen: false, curSibStart: 0)

proc enterSibling*[K, V](t: var Tracker[K, V]) =
  assert t.groups.len > 0, "enterSibling outside any openSiblings"
  assert not t.groups[^1].sibOpen, "previous sibling not closed"
  t.groups[^1].sibOpen = true
  t.groups[^1].curSibStart = t.log.len

proc leaveSibling*[K, V](t: var Tracker[K, V]) =
  ## End the current sibling as a fall-through branch. Its writes are kept
  ## in the log so that `joinSiblings` can merge them.
  ##
  ## If the sibling is already closed, the branch terminated abnormally — a
  ## `gotoLabel` (an embedded `jmp`) consumed it, routed its state to the
  ## label and dropped it from this group. Such a branch does *not* reach
  ## the join, so there is nothing to fall through: treat the call as a
  ## no-op. This lets walkers call `leaveSibling` structurally after every
  ## branch body without first detecting whether the body ended in a jump.
  assert t.groups.len > 0, "leaveSibling outside any openSiblings"
  if not t.groups[^1].sibOpen: return
  let start = t.groups[^1].curSibStart
  t.revertFrom start
  t.groups[^1].sibStarts.add start
  t.groups[^1].sibOpen = false

proc joinSiblings*[K, V](t: var Tracker[K, V]) =
  ## Merge all fall-through siblings of the innermost group into one delta
  ## and apply it to the enclosing scope.
  assert t.groups.len > 0, "joinSiblings without openSiblings"
  assert not t.groups[^1].sibOpen, "current sibling not closed"
  let grp = t.groups.pop()
  let n = grp.sibStarts.len
  let groupLogStart =
    if n > 0: grp.sibStarts[0]
    else: t.log.len

  if n == 0:
    # No fall-through siblings; nothing reaches the join from this group.
    # Log already has nothing for this group (everything was reverted),
    # but defensively shrink.
    t.log.setLen groupLogStart
    return

  # End boundary for each sibling slice.
  var ends = newSeq[int](n)
  for i in 0 ..< n - 1:
    ends[i] = grp.sibStarts[i + 1]
  ends[n - 1] =
    if grp.sibOpen: grp.curSibStart   # unreachable given assert above
    else:
      # The last fall-through sibling's slice extends to the end of the log
      # (no further entries were added after its leaveSibling).
      t.log.len

  # Gather every key touched in any sibling.
  var touched = initHashSet[K]()
  for i in groupLogStart ..< t.log.len:
    touched.incl t.log[i].key

  # For each sibling, build its final-values view and intersect by equality.
  var merged = initTable[K, V]()
  var disagreed = initHashSet[K]()
  for s in 0 ..< n:
    let sibFinal = t.finalValues(grp.sibStarts[s], ends[s])
    let sibBaseFallback = t.base  # pre-group base (we have fully reverted)
    for k in touched:
      if k in disagreed: continue
      let v = sibFinal.getOrDefault(k, sibBaseFallback.getOrDefault(k))
      if s == 0:
        merged[k] = v
      elif merged.getOrDefault(k) != v:
        disagreed.incl k
        merged.del k

  # Drop the entire group slice; we are about to replay the merged delta
  # against the enclosing scope.
  t.log.setLen groupLogStart

  for k, v in merged.pairs:
    t[k] = v
  for k in disagreed:
    # Pre-group base may have held a non-default value here; clear it so
    # the post-join state matches "no agreement → default".
    if t.base.getOrDefault(k) != default(V):
      t[k] = default(V)

# ---- additive join (set-union for HashSet values) -------------------------

proc combineBranchesAdditiveValues*[K, T](t: var Tracker[K, HashSet[T]]) =
  ## Alternative to `joinSiblings`: at the join, each cell's post-state is
  ## the **union** of all contributing siblings' final sets (and the pre-
  ## group base value for siblings that did not touch the cell). Use when
  ## the cell's value is a monotonic accumulator — e.g., the set of
  ## `=wasMoved` positions that may pair with a downstream `=destroy`.
  ##
  ## The sibling-collection machinery is the same as `joinSiblings`; only
  ## the per-cell merge rule differs.
  assert t.groups.len > 0, "combineBranchesAdditiveValues without openSiblings"
  assert not t.groups[^1].sibOpen, "current sibling not closed"
  let grp = t.groups.pop()
  let n = grp.sibStarts.len
  let groupLogStart =
    if n > 0: grp.sibStarts[0]
    else: t.log.len

  if n == 0:
    t.log.setLen groupLogStart
    return

  var ends = newSeq[int](n)
  for i in 0 ..< n - 1:
    ends[i] = grp.sibStarts[i + 1]
  ends[n - 1] = t.log.len

  var touched = initHashSet[K]()
  for i in groupLogStart ..< t.log.len:
    touched.incl t.log[i].key

  var merged = initTable[K, HashSet[T]]()
  for k in touched:
    merged[k] = initHashSet[T]()

  for s in 0 ..< n:
    let sibFinal = t.finalValues(grp.sibStarts[s], ends[s])
    let sibBaseFallback = t.base
    for k in touched:
      let v = sibFinal.getOrDefault(k, sibBaseFallback.getOrDefault(k))
      for elem in v:
        merged[k].incl elem

  t.log.setLen groupLogStart

  for k, v in merged.pairs:
    t[k] = v

# ---- forward jumps and labels ---------------------------------------------

proc snapshotCurrent[K, V](t: Tracker[K, V]): Table[K, V] =
  ## A snapshot of `base`'s non-default entries — the absolute state at this
  ## program point.
  result = initTable[K, V]()
  for k, v in t.base.pairs:
    result[k] = v

proc gotoLabel*[K, V](t: var Tracker[K, V]; L: LabelId) =
  ## The current branch jumps to `L`. Stash an absolute snapshot of the
  ## current state under `L`. If we are inside an open sibling, that sibling
  ## is consumed: its log slice is dropped and it will not contribute to the
  ## enclosing `joinSiblings`.
  t.labels.mgetOrPut(L, @[]).add t.snapshotCurrent()
  if t.groups.len > 0 and t.groups[^1].sibOpen:
    let start = t.groups[^1].curSibStart
    t.revertFrom start
    t.log.setLen start
    t.groups[^1].sibOpen = false

proc landLabel*[K, V](t: var Tracker[K, V]; L: LabelId; arity: int = -1) =
  ## Merge every incoming snapshot of `L` with the current fall-through
  ## state. If `arity >= 0`, assert it matches the number of `gotoLabel`
  ## calls that targeted `L` (the fall-through path is counted separately).
  var incomings = t.labels.getOrDefault(L)
  t.labels.del L
  if arity >= 0:
    assert incomings.len == arity,
      "landLabel: expected " & $arity & " jumps, got " & $incomings.len
  # The current straight-line state is itself one incoming path.
  incomings.add t.snapshotCurrent()

  # Keys touched by any incoming.
  var touched = initHashSet[K]()
  for snap in incomings:
    for k in snap.keys: touched.incl k
  # Plus keys currently set (so they can be cleared on disagreement).
  for k in t.base.keys: touched.incl k

  # Intersect by equality.
  var merged = initTable[K, V]()
  var disagreed = initHashSet[K]()
  for i, snap in incomings:
    for k in touched:
      if k in disagreed: continue
      let v = snap.getOrDefault(k)
      if i == 0:
        merged[k] = v
      elif merged.getOrDefault(k) != v:
        disagreed.incl k
        merged.del k

  # Apply the merged state. Keys present in `base` but absent from `merged`
  # must be cleared.
  var toClear: seq[K] = @[]
  for k in t.base.keys:
    if k notin merged or merged[k] == default(V):
      toClear.add k
  for k in toClear:
    t[k] = default(V)
  for k, v in merged.pairs:
    if v != default(V) and t.base.getOrDefault(k) != v:
      t[k] = v

# ---- self-tests ------------------------------------------------------------

when isMainModule:
  block intersect_bool:
    var t = initTracker[int, bool]()
    t.openSiblings()
    t.enterSibling()
    t[1] = true
    t[2] = true
    t.leaveSibling()
    t.enterSibling()
    t[1] = true
    t[3] = true
    t.leaveSibling()
    t.joinSiblings()
    doAssert t[1] == true            # agreed
    doAssert t[2] == false           # only in branch 0
    doAssert t[3] == false           # only in branch 1

  block disagreement_drops:
    var t = initTracker[int, bool]()
    t[1] = true                      # pre-branch fact
    t.openSiblings()
    t.enterSibling()
    t[1] = false                     # branch 0 clears it
    t.leaveSibling()
    t.enterSibling()
    # branch 1 leaves it as true
    t.leaveSibling()
    t.joinSiblings()
    doAssert t[1] == false           # disagreement → default

  block nested:
    var t = initTracker[int, bool]()
    t.openSiblings()
    t.enterSibling()
    t.openSiblings()
    t.enterSibling()
    t[1] = true
    t.leaveSibling()
    t.enterSibling()
    t[1] = true
    t.leaveSibling()
    t.joinSiblings()                 # inner agrees on 1
    doAssert t[1] == true
    t.leaveSibling()
    t.enterSibling()
    t[1] = true
    t.leaveSibling()
    t.joinSiblings()                 # outer also agrees
    doAssert t[1] == true

  block goto_and_label:
    var t = initTracker[int, bool]()
    let L = LabelId(1)
    t.openSiblings()
    t.enterSibling()
    t[1] = true
    t.gotoLabel L                    # branch 0 jumps with 1=true
    t.enterSibling()
    t[1] = true
    t.leaveSibling()                 # branch 1 falls through with 1=true
    t.joinSiblings()                 # only branch 1 contributes → 1=true
    doAssert t[1] == true
    t.landLabel L, arity = 1         # merge in branch 0's snapshot
    doAssert t[1] == true            # both agreed → still true

  block goto_disagreement:
    var t = initTracker[int, bool]()
    let L = LabelId(2)
    t.openSiblings()
    t.enterSibling()
    t[1] = true
    t.gotoLabel L                    # branch 0 jumps with 1=true
    t.enterSibling()
    t[2] = true                      # branch 1 has 2 but not 1
    t.leaveSibling()
    t.joinSiblings()
    doAssert t[1] == false
    doAssert t[2] == true
    t.landLabel L, arity = 1
    doAssert t[1] == false           # disagreement: branch 0 had 1, fall-through didn't
    doAssert t[2] == false           # disagreement: fall-through had 2, branch 0 didn't

  block additive_union:
    var t = initTracker[int, HashSet[int]]()
    # Pre-group: cell 1 has {100}
    t[1] = toHashSet([100])
    t.openSiblings()
    t.enterSibling()
    # Branch 0 adds 200
    t[1] = toHashSet([100, 200])
    t.leaveSibling()
    t.enterSibling()
    # Branch 1 adds 300
    t[1] = toHashSet([100, 300])
    t.leaveSibling()
    t.combineBranchesAdditiveValues()
    let final = t[1]
    doAssert 100 in final and 200 in final and 300 in final, $final
    doAssert final.len == 3

  block additive_with_clear_in_one_branch:
    var t = initTracker[int, HashSet[int]]()
    t[1] = toHashSet([100])           # pre-group: {100}
    t.openSiblings()
    t.enterSibling()
    t[1] = initHashSet[int]()         # branch 0 clears
    t[1] = toHashSet([200])           # branch 0 adds 200
    t.leaveSibling()
    t.enterSibling()
    # branch 1 inherits {100}
    t.leaveSibling()
    t.combineBranchesAdditiveValues()
    let final = t[1]
    # union of {200} (A's final) and {100} (B inherits) = {100, 200}
    doAssert 100 in final and 200 in final, $final

  block clear_all:
    var t = initTracker[int, bool]()
    t[1] = true
    t[2] = true
    t.openSiblings()
    t.enterSibling()
    t.clearAll()
    doAssert t[1] == false
    t.leaveSibling()
    t.enterSibling()
    t.leaveSibling()
    t.joinSiblings()
    # Branch 0 cleared 1; branch 1 kept it. Disagreement → cleared.
    doAssert t[1] == false
    doAssert t[2] == false

  echo "trackers.nim: all self-tests passed"

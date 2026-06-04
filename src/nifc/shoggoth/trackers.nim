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
## t.openBranches()
## for each branch:
##   t.openBranch()
##   ...walk branch body...
##   t.closeBranch()         # or t.gotoLabel(L) if the branch ends in a jump
## t.closeBranches()
## ```
##
## ## Forward jumps and labels
##
## A branch that ends in `jmp L` calls `gotoLabel L` instead of
## `closeBranch`; its delta is stashed under `L` and dropped from the
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

when defined(nimony):
  # `Tracker[K, V]` uses `default(V)` and `==(V, V)` over an unconstrained
  # generic parameter. nimony eagerly semchecks generic bodies and can't yet
  # resolve those against the concrete overload set; the `untyped` feature
  # defers that resolution to instantiation time (where `V` is `bool` /
  # `HashSet[int]`). Needed so this module is self-compilable during boot
  # (it is pulled in via the hexer arcopt pass).
  {.feature: "untyped".}

type
  LabelId* = distinct uint32

func `==`*(a, b: LabelId): bool {.borrow.}
func hash*(a: LabelId): Hash {.borrow.}

type
  LogEntry[K, V] = object
    key: K
    prev, next: V

  SibGroup = object
    sibStarts: seq[int]   ## one entry per closed fall-through branch; each
                          ## value is the index into `log` at which that
                          ## branch's slice begins. The slice ends at the
                          ## next entry, or at the open branch's start, or
                          ## at `log.len` if no branch is currently open.
    sibOpen: bool
    curSibStart: int      ## valid only if `sibOpen`
    exhaustive: bool      ## true once `openFinalBranch` was called — tells
                          ## `closeBranches` that *some* branch in this group
                          ## is guaranteed to execute, so the join does *not*
                          ## need to add an implicit empty "no branch matched"
                          ## path. Without it, branches are non-exhaustive
                          ## (`if` without `else`, `case` without `else`) and
                          ## the join folds an implicit empty branch in so
                          ## inner-branch bindings can't leak past the group.

  Tracker*[K, V] = object
    base: Table[K, V]                 ## current view (non-default entries only)
    log: seq[LogEntry[K, V]]          ## append-only journal of writes
    groups: seq[SibGroup]             ## stack of open `openBranches` scopes
    labels: Table[LabelId, seq[Table[K, V]]]  ## incoming deltas per label
    pathDiverged: bool                ## did the current straight-line path
                                      ## already leave the block (return / raise
                                      ## / break / a fully-diverging inner
                                      ## construct)? Sticky within a path, reset
                                      ## at each `openBranch`. When set at
                                      ## `closeBranch`, the branch is dropped
                                      ## from the join instead of folded in.

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

proc consumeOpenSibling[K, V](t: var Tracker[K, V]) =
  ## If we are inside an open sibling, drop it: revert its writes and discard
  ## its log slice so it does NOT contribute to the enclosing `closeBranches`.
  ## A consumed sibling reaches no join along the fall-through edge. Shared by
  ## `gotoLabel` (state routes to a label instead) and `closeBranch` on a
  ## diverged path (state reaches no join at all).
  if t.groups.len > 0 and t.groups[^1].sibOpen:
    let start = t.groups[^1].curSibStart
    t.revertFrom start
    t.log.setLen start
    t.groups[^1].sibOpen = false

# ---- branch-group operations ----------------------------------------------
#
# Branching constructs (`if`/`case`/…) are walked as:
#
#   openBranches t
#   for each branch:
#     openBranch t          # or `openFinalBranch t` on an else / wildcard `of`
#     ...walk branch body...
#     closeBranch t         # or `gotoLabel t L` if the branch ended in `jmp`
#   closeBranches t
#
# `openFinalBranch` is `openBranch` plus a flag that tells `closeBranches`
# the group is exhaustive — no implicit "no branch matched" path. Without
# any `openFinalBranch` call the group is non-exhaustive and `closeBranches`
# folds in an empty implicit branch so per-branch writes (a binding to an
# inner-scope sym, a `wasMoved` bit, …) can't leak past the construct as if
# the branch always executed.

proc openBranches*[K, V](t: var Tracker[K, V]) =
  t.groups.add SibGroup(sibStarts: @[], sibOpen: false, curSibStart: 0,
                        exhaustive: false)

proc markDiverged*[K, V](t: var Tracker[K, V]) =
  ## The walker reached an unconditional `return` / `raise` / `break`: the
  ## current straight-line path leaves the enclosing block. The flag is sticky
  ## (further statements on this path are dead) and is read by `closeBranch`,
  ## which drops a diverged branch from the join. Reset at the next
  ## `openBranch`. `closeBranches` re-derives it for the enclosing path, so a
  ## construct whose every branch diverged propagates outward automatically —
  ## the walker never has to compute or thread divergence itself.
  t.pathDiverged = true

proc diverged*[K, V](t: Tracker[K, V]): bool {.inline.} =
  ## Whether the current path has diverged (see `markDiverged`).
  t.pathDiverged

proc openBranch*[K, V](t: var Tracker[K, V]) =
  assert t.groups.len > 0, "openBranch outside any openBranches"
  assert not t.groups[^1].sibOpen, "previous branch not closed"
  t.groups[^1].sibOpen = true
  t.groups[^1].curSibStart = t.log.len
  t.pathDiverged = false               # each branch is a fresh path

proc openFinalBranch*[K, V](t: var Tracker[K, V]) =
  ## Like `openBranch`, but marks the enclosing group as exhaustive — at
  ## least one branch always runs. Pass this for the `else` / wildcard `of`
  ## arm so `closeBranches` doesn't fold in an implicit empty branch.
  assert t.groups.len > 0, "openFinalBranch outside any openBranches"
  t.groups[^1].exhaustive = true
  openBranch(t)

proc closeBranch*[K, V](t: var Tracker[K, V]) =
  ## End the current branch.
  ##
  ## - If the path diverged (`markDiverged`), the branch reaches no join:
  ##   consume it — revert its writes and drop its slice so it does not
  ##   contribute to `closeBranches`. This is the "branches that jumped away
  ##   do not appear in the join list" rule from arcopt.md, applied to
  ##   `return`/`raise`/`break` as well as to fully-diverging inner constructs.
  ## - Otherwise keep its writes in the log so `closeBranches` can merge them.
  ##
  ## If the branch is already closed it terminated via `gotoLabel` (an embedded
  ## `jmp`), which consumed it; treat the call as a no-op. This lets walkers
  ## call `closeBranch` structurally after every branch body without first
  ## detecting how the body ended.
  assert t.groups.len > 0, "closeBranch outside any openBranches"
  if not t.groups[^1].sibOpen: return
  if t.pathDiverged:
    t.consumeOpenSibling()
    return
  let start = t.groups[^1].curSibStart
  t.revertFrom start
  t.groups[^1].sibStarts.add start
  t.groups[^1].sibOpen = false

proc maybeAddImplicitEmptyBranch[K, V](t: var Tracker[K, V]) =
  ## Internal: called from the close-branches paths. When the group is *not*
  ## exhaustive (no `openFinalBranch` was issued) and at least one branch
  ## reached the join, fold in an empty implicit branch — the "no branch
  ## matched" fall-through. With no writes of its own it contributes the
  ## pre-group base value for every key the real branches touched, so any
  ## key the real branches set to a different value disagrees with it and
  ## collapses to `default(V)` at the join.
  if t.groups.len == 0: return
  if t.groups[^1].exhaustive: return
  if t.groups[^1].sibStarts.len == 0: return
  openBranch(t)
  closeBranch(t)

proc closeBranches*[K, V](t: var Tracker[K, V]) =
  ## Merge all fall-through branches of the innermost group into one delta
  ## and apply it to the enclosing scope.
  assert t.groups.len > 0, "closeBranches without openBranches"
  assert not t.groups[^1].sibOpen, "current branch not closed"
  maybeAddImplicitEmptyBranch(t)
  let grp = t.groups.pop()
  let n = grp.sibStarts.len
  # The construct diverges on every path iff it was exhaustive (a branch always
  # runs) and no branch fell through to the join — every arm returned/broke/
  # jumped. Propagate that to the enclosing path so a containing branch is
  # itself dropped from its join.
  t.pathDiverged = grp.exhaustive and n == 0
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
      # (no further entries were added after its closeBranch).
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

proc closeBranchesAdditive*[K, T](t: var Tracker[K, HashSet[T]]) =
  ## Alternative to `closeBranches`: at the join, each cell's post-state is
  ## the **union** of all contributing branches' final sets (and the pre-
  ## group base value for branches that did not touch the cell). Use when
  ## the cell's value is a monotonic accumulator — e.g., the set of
  ## `=wasMoved` positions that may pair with a downstream `=destroy`.
  ##
  ## The branch-collection machinery is the same as `closeBranches`; only
  ## the per-cell merge rule differs. Exhaustiveness handling is identical:
  ## a non-exhaustive group gets an implicit empty branch (whose empty set
  ## contributes nothing to the union, but is included for symmetry).
  assert t.groups.len > 0, "closeBranchesAdditive without openBranches"
  assert not t.groups[^1].sibOpen, "current branch not closed"
  maybeAddImplicitEmptyBranch(t)
  let grp = t.groups.pop()
  let n = grp.sibStarts.len
  t.pathDiverged = grp.exhaustive and n == 0   # see `closeBranches`
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
        # `mgetOrPut` (not `[]`) to stay non-raising; `k` was pre-seeded above.
        merged.mgetOrPut(k, initHashSet[T]()).incl elem

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
  ## current state under `L`, then consume the open sibling (its state has
  ## been routed to `L`, so it must not also fall through to the join). The
  ## path after an unconditional jump is diverged.
  t.labels.mgetOrPut(L, @[]).add t.snapshotCurrent()
  t.consumeOpenSibling()
  t.pathDiverged = true

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
    if k notin merged or merged.getOrDefault(k) == default(V):
      toClear.add k
  for k in toClear:
    t[k] = default(V)
  for k, v in merged.pairs:
    if v != default(V) and t.base.getOrDefault(k) != v:
      t[k] = v
  # A landed label is a reachable join (incoming jumps target it), so the path
  # is live again even if the fall-through into it had diverged.
  t.pathDiverged = false

# ---- self-tests ------------------------------------------------------------

when isMainModule:
  block intersect_bool_exhaustive:
    # Two branches modelling `if c: … else: …` — exhaustive (one always runs),
    # so the second branch is `openFinalBranch`.
    var t = initTracker[int, bool]()
    t.openBranches()
    t.openBranch()
    t[1] = true
    t[2] = true
    t.closeBranch()
    t.openFinalBranch()
    t[1] = true
    t[3] = true
    t.closeBranch()
    t.closeBranches()
    doAssert t[1] == true            # agreed across both branches
    doAssert t[2] == false           # only in branch 0
    doAssert t[3] == false           # only in branch 1

  block non_exhaustive_drops_branch_writes:
    # `if c: x = 10` — without `openFinalBranch`, the group is non-exhaustive;
    # the implicit empty branch's pre-value disagrees with the branch's 10,
    # so x collapses to `default(int) = 0` at the join.
    var t = initTracker[int, int]()
    t[1] = 5                         # pre-branch fact
    t.openBranches()
    t.openBranch()
    t[1] = 10                        # only branch's write
    t.closeBranch()
    t.closeBranches()
    doAssert t[1] == 0               # implicit empty branch ⇒ default

  block disagreement_drops:
    # `if c: x = false else: …` (else doesn't touch x). With openFinalBranch
    # on the else, branches are exhaustive; the disagreement still collapses.
    var t = initTracker[int, bool]()
    t[1] = true                      # pre-branch fact
    t.openBranches()
    t.openBranch()
    t[1] = false                     # branch 0 clears it
    t.closeBranch()
    t.openFinalBranch()
    # branch 1 leaves it as true
    t.closeBranch()
    t.closeBranches()
    doAssert t[1] == false           # disagreement → default

  block nested:
    var t = initTracker[int, bool]()
    t.openBranches()
    t.openBranch()
    t.openBranches()
    t.openBranch()
    t[1] = true
    t.closeBranch()
    t.openFinalBranch()
    t[1] = true
    t.closeBranch()
    t.closeBranches()                # inner agrees on 1
    doAssert t[1] == true
    t.closeBranch()
    t.openFinalBranch()
    t[1] = true
    t.closeBranch()
    t.closeBranches()                # outer also agrees
    doAssert t[1] == true

  block goto_and_label:
    # Models `if c: …; goto L else: …; L:`. The else makes the if exhaustive.
    var t = initTracker[int, bool]()
    let L = LabelId(1)
    t.openBranches()
    t.openBranch()
    t[1] = true
    t.gotoLabel L                    # branch 0 jumps with 1=true
    t.openFinalBranch()
    t[1] = true
    t.closeBranch()                  # branch 1 falls through with 1=true
    t.closeBranches()                # only branch 1 contributes → 1=true
    doAssert t[1] == true
    t.landLabel L, arity = 1         # merge in branch 0's snapshot
    doAssert t[1] == true            # both agreed → still true

  block goto_disagreement:
    var t = initTracker[int, bool]()
    let L = LabelId(2)
    t.openBranches()
    t.openBranch()
    t[1] = true
    t.gotoLabel L                    # branch 0 jumps with 1=true
    t.openFinalBranch()
    t[2] = true                      # branch 1 has 2 but not 1
    t.closeBranch()
    t.closeBranches()
    doAssert t[1] == false
    doAssert t[2] == true
    t.landLabel L, arity = 1
    doAssert t[1] == false           # disagreement: branch 0 had 1, fall-through didn't
    doAssert t[2] == false           # disagreement: fall-through had 2, branch 0 didn't

  block additive_union:
    var t = initTracker[int, HashSet[int]]()
    # Pre-group: cell 1 has {100}
    t[1] = toHashSet([100])
    t.openBranches()
    t.openBranch()
    # Branch 0 adds 200
    t[1] = toHashSet([100, 200])
    t.closeBranch()
    t.openBranch()
    # Branch 1 adds 300
    t[1] = toHashSet([100, 300])
    t.closeBranch()
    t.closeBranchesAdditive()
    let final = t[1]
    doAssert 100 in final and 200 in final and 300 in final, $final
    doAssert final.len == 3

  block additive_with_clear_in_one_branch:
    var t = initTracker[int, HashSet[int]]()
    t[1] = toHashSet([100])           # pre-group: {100}
    t.openBranches()
    t.openBranch()
    t[1] = initHashSet[int]()         # branch 0 clears
    t[1] = toHashSet([200])           # branch 0 adds 200
    t.closeBranch()
    t.openBranch()
    # branch 1 inherits {100}
    t.closeBranch()
    t.closeBranchesAdditive()
    let final = t[1]
    # union of {200} (A's final) and {100} (B inherits) = {100, 200}
    doAssert 100 in final and 200 in final, $final

  block clear_all:
    var t = initTracker[int, bool]()
    t[1] = true
    t[2] = true
    t.openBranches()
    t.openBranch()
    t.clearAll()
    doAssert t[1] == false
    t.closeBranch()
    t.openBranch()
    t.closeBranch()
    t.closeBranches()
    # Branch 0 cleared 1; branch 1 kept it. Disagreement → cleared.
    doAssert t[1] == false
    doAssert t[2] == false

  block diverged_branch_excluded_from_join:
    # `if c: x=true; return else: y=true`. The elif diverges, so only the else
    # contributes: y survives unintersected, x does not leak, and the construct
    # itself does not diverge (the else falls through).
    var t = initTracker[int, bool]()
    t.openBranches()
    t.openBranch()
    t[1] = true                      # x
    t.markDiverged()                 # ... return
    t.closeBranch()                  # consumed — excluded from the join
    t.openFinalBranch()
    t[2] = true                      # y
    t.closeBranch()                  # falls through
    t.closeBranches()
    doAssert t[2] == true            # else's write kept (not diluted by elif)
    doAssert t[1] == false           # diverged elif's write did not leak
    doAssert not t.diverged          # else fell through ⇒ construct is live

  block diverged_branch_preserves_pre_construct_facts:
    # `x=true; if c: return`  (no else). The only branch diverges; the
    # non-exhaustive fall-through (c false) keeps the pre-`if` fact, and the
    # construct does not diverge.
    var t = initTracker[int, bool]()
    t[1] = true
    t.openBranches()
    t.openBranch()
    t.markDiverged()
    t.closeBranch()
    t.closeBranches()
    doAssert t[1] == true            # pre-if fact survives the conditional return
    doAssert not t.diverged

  block all_branches_diverge_propagates_outward:
    # An inner `if c: return else: return` diverges on every path; the tracker
    # must remember this so the enclosing branch is itself dropped from the
    # OUTER join — without the walker computing divergence.
    var t = initTracker[int, bool]()
    t.openBranches()                 # OUTER group
    t.openBranch()                   # outer branch 0
    t.openBranches()                 # inner if/else, both arms diverge
    t.openBranch(); t.markDiverged(); t.closeBranch()
    t.openFinalBranch(); t.markDiverged(); t.closeBranch()
    t.closeBranches()                # inner fully diverges ⇒ pathDiverged
    doAssert t.diverged              # propagated to the enclosing path
    t[1] = true                      # dead write on the diverged outer branch
    t.closeBranch()                  # outer branch 0 consumed (diverged)
    t.openFinalBranch()              # outer else
    t[2] = true
    t.closeBranch()
    t.closeBranches()
    doAssert t[2] == true            # only the live outer-else contributes
    doAssert t[1] == false           # dead branch's write excluded

  echo "trackers.nim: all self-tests passed"

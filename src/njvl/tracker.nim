#
#
#           Nimony Final-IR Exit-Summary Tracker
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A reusable **exit-summary tracker** for analyses over the Final IR
## (`doc/final_ir.md`, section *Analysis: exit summaries*).
##
## A pass over Final IR is a plain tree traversal: the only shape that *looks*
## like it needs a CFG is a branch where both arms leave to different targets
## (`if cond: jmp inner else: jmp outer`), so nothing falls through yet two
## labels each gain a predecessor. The `Tracker` makes that a non-issue: a
## subtree does not return a single successor but an **exit summary** —
##
## - `live`/`state`: the analysis state if control *falls through* off the end
##   (`live == false` ⇒ the subtree always leaves, there is no fall-through).
## - `exits`: the state captured at each way control *leaves* — keyed by where
##   it goes (`jmp L`, `return`, `raise`, the loop back-edge `continue`).
##
## The tracker is generic over:
## - `T` — the *property/state* the analysis computes (init-sets, `inferle`
##   facts, borrow sets, …). It is treated as a value with a single required
##   operation: a **join** (the lattice ⊔ at merge points), supplied at
##   construction.
## - `L` — the label identifier type (`SymId` in the real compiler; any
##   hashable type in tests).
##
## The transfer functions for *leaves* (a `store`, a `call`, an `assume`) live
## in the driver, which mutates `tr.state` directly. The tracker only owns the
## control-flow plumbing: the *bind* of the writer-over-exits effect (`seq`),
## the branch merge (`ite`), and the multi-join that consumes a key at the
## construct that binds it (`lab`, the proc root, an `except`).
##
## ### Mapping to the doc's composition rules
##
## | doc rule                       | tracker operation                       |
## |--------------------------------|-----------------------------------------|
## | leaf → `{fallthrough: σ'}`      | mutate `tr.state` in place              |
## | `jmp L` / `return` / `raise`    | `gotoLabel` / `gotoReturn` / `gotoRaise`|
## | `seq(s1, s2)`                   | just keep driving the same `tr`         |
## | `ite(c, t, e)`                  | `splitBranch` / `commitThen` / `mergeBranches` |
## | `(lab L)` multi-join           | `bindLabel`                             |
## | proc root binds `Return`        | `bindReturn`                            |
## | `except` consumes `Raise`       | `takeRaise` (+ `bindRaise` to re-merge) |
## | `finally` maps over every exit  | `mapStates`                             |
## | loop back-edge                 | `gotoContinue` + `dropContinue`         |

import std / [tables, hashes, sets]
# Concept constraints (`Keyable`/`Hashable`/`HasDefault`/`Equatable`) keep the
# generic procs checked-generics-clean under Nimony (strict generics resolve
# `hash`/`==`/`default` at definition time). Under host Nim these come from the
# universal-match shims in `compat2`.
include ".." / lib / compat2

type
  ExitKind* = enum
    ekLabel      ## a forward `jmp L`, loop-`break` (`jmp loopExit`) included
    ekReturn     ## the primitive `return`, bound by the proc root
    ekRaise      ## the primitive `raise`, bound by the nearest `except`
    ekContinue   ## the loop back-edge, bound by its `loop` header

  ExitKey*[L] = object
    ## Every way control leaves a subtree. `continue` is *not* an acyclic
    ## forward exit, but we key it here too so the loop header can pull it.
    kind*: ExitKind
    label*: L      ## meaningful only when `kind == ekLabel`

  JoinProc*[T] = proc (a, b: T): T {.nimcall.}
    ## The lattice join (⊔). Must be commutative, associative and idempotent —
    ## the multi-join relies on `join(x, x) == x`. A plain `nimcall` (the join
    ## never captures) — Nimony rejects a `.closure` field here, and it is also
    ## one fewer indirection.

  JournalEntry[T, L] = object
    ## One undo record for a mutation of `exits` — `(key, its prior presence,
    ## its prior value)`. Reused inside `Branch.thenDelta`, where `had` instead
    ## means "present in the then-branch's exits" and `old` is the then value.
    key: ExitKey[L]
    had: bool
    old: T

  Tracker*[T, L] = object
    live*: bool                  ## is fall-through reachable here?
    state*: T                    ## the fall-through state (valid iff `live`)
    exits*: Table[ExitKey[L], T] ## accumulated state at each pending exit
    journal: seq[JournalEntry[T, L]] ## append-only undo log for `exits`, so an
                                     ## `ite` can roll the then-branch back to
                                     ## the baseline without copying the table
    join: JoinProc[T]

  Branch*[T, L] = object
    ## Snapshot taken at an `ite`, carried from `splitBranch` to `mergeBranches`.
    ## Instead of copying `exits` (O(table) per `ite`, the historic hot spot on
    ## deeply nested branches), we remember the journal length at the split and
    ## the then-branch's *delta* — making each `ite` cost O(its own mutations).
    baseLive: bool
    baseState: T
    cp: int                            ## journal length at `splitBranch`
    thenLive: bool
    thenState: T
    thenDelta: seq[JournalEntry[T, L]] ## the then-branch's net exit contributions

func hash*[L: Hashable](k: ExitKey[L]): Hash =
  result = hash(ord(k.kind))
  if k.kind == ekLabel:
    result = result !& hash(k.label)
  result = !$result

# `==` is left to the structural object equality: a custom `[L]`-generic `==`
# is ambiguous with system's generic `==[T]` under Nimony, and the non-label
# keys always carry `default(L)` in `label`, so field-wise equality already
# matches the intended "same kind (and same label, when a label)" semantics.

proc labelKey*[L](label: L): ExitKey[L] {.inline.} =
  ExitKey[L](kind: ekLabel, label: label)

# --------------------------------------------------------------- construction

proc initTracker*[T: HasDefault, L: Keyable and HasDefault](initial: T; join: JoinProc[T]): Tracker[T, L] =
  ## Start a tracker at a reachable program point with state `initial`.
  Tracker[T, L](live: true, state: initial,
                exits: initTable[ExitKey[L], T](), join: join)

# ------------------------------------------------------ journaled exits access

proc jPut[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; key: ExitKey[L]; val: sink T) =
  ## Set `exits[key] = val`, recording the prior cell for undo.
  if key in tr.exits:
    tr.journal.add JournalEntry[T, L](key: key, had: true, old: getOrDefault(tr.exits, key))
  else:
    tr.journal.add JournalEntry[T, L](key: key, had: false)
  tr.exits[key] = val

proc jDel[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; key: ExitKey[L]) =
  ## Delete `exits[key]` (if present), recording the prior value for undo.
  if key in tr.exits:
    tr.journal.add JournalEntry[T, L](key: key, had: true, old: getOrDefault(tr.exits, key))
    tr.exits.del key

# ----------------------------------------------------------------- leaving

proc leaveVia[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; key: ExitKey[L]) =
  ## Internal: record the current state at `key` and stop falling through.
  ## A `jmp`/`return`/`raise` from a dead point is unreachable — ignored.
  if tr.live:
    if key in tr.exits:
      jPut(tr, key, tr.join(getOrDefault(tr.exits, key), tr.state))
    else:
      jPut(tr, key, tr.state)
    tr.live = false

proc gotoLabel*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; label: L) {.inline.} =
  ## `(jmp L)` — a forward structural transfer (loop-`break` included).
  leaveVia(tr, labelKey(label))

proc gotoReturn*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]) {.inline.} =
  ## `return` — a primitive exit bound by the proc root.
  leaveVia(tr, ExitKey[L](kind: ekReturn))

proc gotoRaise*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]) {.inline.} =
  ## `raise` — a primitive exit bound by the nearest enclosing `except`.
  leaveVia(tr, ExitKey[L](kind: ekRaise))

proc gotoContinue*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]) {.inline.} =
  ## `continue` — the loop back-edge, bound by its `loop` header.
  leaveVia(tr, ExitKey[L](kind: ekContinue))

# ------------------------------------------------------------------ binding

proc bindKey[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; key: ExitKey[L]) =
  ## Consume `key`: join its accumulated state into fall-through (the multi-join)
  ## and remove it from the pending set. Because forward `jmp`s are seen before
  ## their `lab`, every contributor is already present at this point.
  if key in tr.exits:
    let landed = getOrDefault(tr.exits, key)
    jDel(tr, key)
    if tr.live:
      tr.state = tr.join(tr.state, landed)
    else:
      tr.state = landed
      tr.live = true

proc bindLabel*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; label: L) {.inline.} =
  ## `(lab L)` — the multi-join. `arity(L)` is exactly
  ## `ord(was live before) + count of jmps to L`, all carried by the summary.
  bindKey(tr, labelKey(label))

proc bindReturn*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]) {.inline.} =
  ## The proc root joins every `return` into the final fall-through state.
  bindKey(tr, ExitKey[L](kind: ekReturn))

proc bindRaise*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]) {.inline.} =
  ## Join any pending `raise` into fall-through (used after an `except` body
  ## has re-merged, or at the proc root for a `.raises` proc).
  bindKey(tr, ExitKey[L](kind: ekRaise))

proc dropContinue*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]) {.inline.} =
  ## The loop header consumes the back-edge. A one-pass forward analysis
  ## discards it (the cyclic join, if any, is the loop's local fixpoint);
  ## acyclic forward exits to `loopExit` resolve via `bindLabel`.
  jDel(tr, ExitKey[L](kind: ekContinue))

# ------------------------------------------------------------ except / finally

proc takeRaise*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]): tuple[reached: bool, state: T] =
  ## Pull (and remove) the accumulated `raise` state — the entry state for an
  ## `except` handler. `reached == false` ⇒ no `raise` crossed the `try` body.
  let key = ExitKey[L](kind: ekRaise)
  if key in tr.exits:
    result = (true, getOrDefault(tr.exits, key))
    jDel(tr, key)
  else:
    result = (false, default(T))

proc mapStates*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; f: proc (s: T): T) =
  ## `finally`: run the cleanup's transfer over *every* component — the
  ## fall-through and each pending exit alike. "The cleanup runs on every exit"
  ## as one line of the traversal; nested `try`s give LIFO for free.
  if tr.live:
    tr.state = f(tr.state)
  var keys: seq[ExitKey[L]] = @[]
  for k in tr.exits.keys: keys.add k
  for k in keys:
    jPut(tr, k, f(getOrDefault(tr.exits, k)))

# ---------------------------------------------------------------- branching

proc splitBranch*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]): Branch[T, L] =
  ## Begin an `ite`: remember the journal position so the else-branch can be
  ## rolled back to the same baseline the then-branch started from. No table
  ## copy — only a checkpoint.
  Branch[T, L](baseLive: tr.live, baseState: tr.state, cp: tr.journal.len)

proc commitThen*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; b: var Branch[T, L]) =
  ## Call after the then-branch: capture its *delta* (the distinct exit keys it
  ## touched, with their post-then values), then roll `exits` back to the
  ## baseline via the journal so the else-branch starts clean.
  b.thenLive = tr.live
  b.thenState = tr.state
  # Collect each distinct key the then-branch touched, newest first, recording
  # whether it is still an exit (and its value) after the then-branch.
  b.thenDelta = @[]
  var seen = initHashSet[ExitKey[L]]()
  var i = tr.journal.len - 1
  while i >= b.cp:
    let k = tr.journal[i].key
    if not containsOrIncl(seen, k):
      if k in tr.exits:
        b.thenDelta.add JournalEntry[T, L](key: k, had: true, old: getOrDefault(tr.exits, k))
      else:
        b.thenDelta.add JournalEntry[T, L](key: k, had: false)
    dec i
  # Undo every then-branch mutation, restoring `exits` to the baseline.
  while tr.journal.len > b.cp:
    let e = tr.journal[tr.journal.len - 1]
    tr.journal.setLen(tr.journal.len - 1)
    if e.had: tr.exits[e.key] = e.old
    else: tr.exits.del e.key
  tr.live = b.baseLive
  tr.state = b.baseState

proc mergeBranches*[T: HasDefault, L: Keyable and HasDefault](tr: var Tracker[T, L]; b: Branch[T, L]) =
  ## Call after the else-branch: join the then-branch (in `b`) with the current
  ## else-branch (in `tr`). `fallthrough` survives iff *some* arm falls through;
  ## `exits` are joined key-wise over the union. A branch that always leaves has
  ## no fall-through, so it drops out and the post-`if` state is exactly the
  ## surviving arm's — unifying guard-clause and if-else style.
  for e in b.thenDelta:
    if e.had:
      # the then-branch left an exit at `e.key` with value `e.old`; join it into
      # the else-branch's (journaled, so an enclosing `ite` can still undo it).
      if e.key in tr.exits:
        jPut(tr, e.key, tr.join(getOrDefault(tr.exits, e.key), e.old))
      else:
        jPut(tr, e.key, e.old)
  let elseLive = tr.live
  if b.thenLive and elseLive:
    tr.state = tr.join(b.thenState, tr.state)
    tr.live = true
  elif b.thenLive:
    tr.state = b.thenState
    tr.live = true
  else:
    tr.live = elseLive   # tr.state already holds the else-branch state

# ----------------------------------------------------------------- queries

proc reachable*[T, L](tr: Tracker[T, L]): bool {.inline.} =
  ## Is fall-through reachable here? `false` after an unconditional leave.
  tr.live

proc pending*[T, L: Keyable](tr: Tracker[T, L]; key: ExitKey[L]): bool {.inline.} =
  ## Is there an as-yet-unbound exit to `key`? A `raise` still pending at the
  ## proc root is a `.raises: []` violation — detected by the same traversal.
  key in tr.exits

when isMainModule:
  import std / [assertions, syncio]

  # Demo state: the set of locals proven initialized at a program point.
  # The lattice join is *intersection* (a var is init after a merge only if it
  # was init on every incoming path).
  type Inits = HashSet[int]
  proc isect(a, b: Inits): Inits = intersection(a, b)

  proc newTr(initial: Inits = initHashSet[int]()): Tracker[Inits, int] =
    initTracker[Inits, int](initial, isect)

  # 1) Guard-clause: `if cond: r = 1; jmp L else: r = 2`  /  L: use r
  #    r is init on both the jmp-out path and the fall-through, so init at L.
  block:
    var tr = newTr()
    var b = splitBranch(tr)
    tr.state.incl 1            # then: r = 1
    gotoLabel(tr, 99)          #       jmp L
    commitThen(tr, b)
    tr.state.incl 1            # else: r = 2 (same var "1" stands for r)
    mergeBranches(tr, b)
    bindLabel(tr, 99)          # L:
    assert tr.reachable
    assert 1 in tr.state, "r must be initialized at the merge"

  # 2) One-armed guard where only the jmp path inits r:
  #    `if cond: r = 1; jmp L`  (no else)  /  L:  -> r NOT guaranteed init
  block:
    var tr = newTr()
    var b = splitBranch(tr)
    tr.state.incl 1            # then: r = 1; jmp L
    gotoLabel(tr, 99)
    commitThen(tr, b)
    # empty else (fall-through, r still uninit)
    mergeBranches(tr, b)
    bindLabel(tr, 99)
    assert tr.reachable
    assert 1 notin tr.state, "r is only init on the jmp path, not the fall-through"

  # 3) Both arms leave to *different* targets — nothing falls through.
  #    `if cond: jmp inner else: jmp outer`
  block:
    var tr = newTr()
    var b = splitBranch(tr)
    gotoLabel(tr, 1)           # then: jmp inner
    commitThen(tr, b)
    gotoLabel(tr, 2)           # else: jmp outer
    mergeBranches(tr, b)
    assert not tr.reachable, "no fall-through survives"
    assert tr.pending(labelKey(1)) and tr.pending(labelKey(2))
    bindLabel(tr, 1)
    assert tr.reachable        # inner is reached from the then arm
    bindLabel(tr, 2)           # outer reached, joins in

  # 4) `return` survives all the way to the proc root.
  block:
    var tr = newTr()
    tr.state.incl 7
    gotoReturn(tr)
    assert not tr.reachable
    assert tr.pending(ExitKey[int](kind: ekReturn))
    bindReturn(tr)
    assert tr.reachable
    assert 7 in tr.state

  # 5) Loop: body either `jmp loopExit` (break, inits nothing) or loops back
  #    via `continue`. After the loop only the break-exit state survives.
  block:
    var tr = newTr()
    tr.state.incl 5            # something init before the loop
    # loop body:
    var b = splitBranch(tr)
    gotoLabel(tr, 88)          # if cond: jmp loopExit (break)
    commitThen(tr, b)
    tr.state.incl 6            # else: body work, var 6 init this iteration
    gotoContinue(tr)           # back-edge
    mergeBranches(tr, b)
    assert not tr.reachable    # loop never falls through
    dropContinue(tr)           # header consumes the back-edge
    bindLabel(tr, 88)          # (lab loopExit) after the loop
    assert tr.reachable
    assert 5 in tr.state
    assert 6 notin tr.state, "iteration-local init must not leak past the loop"

  echo "tracker: all exit-summary cases OK"

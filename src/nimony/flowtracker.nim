#
#           Nimony Final-IR Flow Tracker
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A **journaled** exit-summary tracker for the Final-IR nil/init analysis.
##
## It supersedes the generic value-`Tracker` (`njvl/tracker.nim`) for this pass:
## the analysis state — the definite-assignment **init-set** and the `inferle`
## **facts** — is mutated *in place* under an undo journal, so a branch costs
## O(writes-since-checkpoint), never a whole-state copy. The old tracker copied
## the init-`HashSet` twice on *every* `ite` (the `baseState`/`thenState`
## snapshots), which is O(module) per branch on deep `if/elif` chains; here a
## guard clause (`if x == nil: return`) costs a cheap rollback and *no* copy,
## and a real two-armed merge copies only the small per-branch *delta* of the
## init-set (plus the — normally tiny — fact set).
##
## The two components travel together as one `FlowState`:
## - `inits` — journaled definite-assignment set (add/remove + undo).
## - `facts` — the `inferle.Facts` engine (already journaled).
##
## Control flow maps to the same operations the generic tracker documented:
## `gotoLabel`/`gotoReturn`/`gotoRaise`/`gotoContinue` record the state at a
## leave and stop fall-through; `bindLabel`/`bindReturn`/`bindRaise` join the
## accumulated exit state back into fall-through; `splitBranch`/`commitThen`/
## `mergeBranches` drive an `ite`; `takeRaise`/`mapStates` serve `try`/`finally`.
## Facts are joined by intersection at every confluence (weakest common bound —
## `inferle.merge`), matching the init-set's intersection join.

import std/[tables, hashes, sets]
include ".." / lib / nifprelude   # SymId (and its `==`/`hash`), like the rest of nimony/
import inferle, xints
import ".." / njvl / tracker   # reuse ExitKind / ExitKey / labelKey

export ExitKind, ExitKey, labelKey

# --------------------------------------------------------------- init-set

type
  InitJOp = enum ijAdd, ijDel
  InitJEntry = object
    op: InitJOp
    sym: SymId

  Inits* = object
    ## Journaled definite-assignment set. `incl` (a local became initialized)
    ## and `excl` (a confluence removed it — init on one path only) are both
    ## logged so any branch/confluence can be rolled back without a set copy.
    s: HashSet[SymId]
    journal: seq[InitJEntry]
    journaling: bool

proc initInits*(): Inits = Inits(s: initHashSet[SymId]())
proc enableJournaling*(x: var Inits) {.inline.} = x.journaling = true

proc contains*(x: Inits; sym: SymId): bool {.inline.} = sym in x.s

proc incl*(x: var Inits; sym: SymId) =
  if sym notin x.s:
    x.s.incl sym
    if x.journaling: x.journal.add InitJEntry(op: ijAdd, sym: sym)

proc excl*(x: var Inits; sym: SymId) =
  if sym in x.s:
    x.s.excl sym
    if x.journaling: x.journal.add InitJEntry(op: ijDel, sym: sym)

proc checkpoint*(x: Inits): int {.inline.} = x.journal.len

proc rollbackTo*(x: var Inits; cp: int) =
  var i = x.journal.len - 1
  while i >= cp:
    let e = x.journal[i]
    case e.op
    of ijAdd: x.s.excl e.sym
    of ijDel: x.s.incl e.sym
    dec i
  x.journal.setLen cp

proc addedSince(x: Inits; cp: int): seq[SymId] =
  ## The net keys gained since `cp`: those added in the interval AND still
  ## present now. Baseline keys are never removed (definite-assignment is
  ## monotone; nested confluences only ever drop keys they themselves added),
  ## so a currently-present key added since `cp` is exactly a delta key — even
  ## if a nested branch added, dropped, then re-added it.
  result = @[]
  var seen = initHashSet[SymId]()
  for i in cp ..< x.journal.len:
    let e = x.journal[i]
    if e.op == ijAdd and e.sym in x.s and not containsOrIncl(seen, e.sym):
      result.add e.sym

proc snapshot(x: Inits): HashSet[SymId] = x.s   # a plain value copy

# --------------------------------------------------------------- facts helper

proc setFactsTo(f: var Facts; cp: int; target: Facts) =
  ## Make `f` equal `target` *journaled* — roll back to `cp`, then add/remove
  ## only the cells that differ, so an enclosing checkpoint stays valid.
  f.rollbackTo cp
  var want = initTable[(VarId, VarId), xint]()
  for k in 0 ..< target.len:
    let m = target[k]
    want[(m.a, m.b)] = m.c
  var i = 0
  while i < f.len:
    let cur = f[i]
    let key = (cur.a, cur.b)
    if key in want and want.getOrDefault(key) == cur.c:
      want.del key
      inc i
    else:
      removeFactAt(f, i)          # journaled removal; rechecks slot i
  for key, cc in want:
    f.add LeXplusC(a: key[0], b: key[1], c: cc)

# --------------------------------------------------------------- flow state

type
  FlowState* = object
    ## The whole analysis state, journaled in place.
    inits*: Inits
    facts*: Facts

  FlowCp* = object
    initsCp: int
    factsCp: int

  FlowSnap* = object
    ## A materialized state, stashed at a pending exit or held while the other
    ## arm of a genuine (both-fall-through) confluence is analyzed. Also the
    ## accumulator type the driver folds N-way (`case`/`try`) merges into.
    inits: HashSet[SymId]
    facts: Facts

proc initFlowState*(): FlowState =
  result = FlowState(inits: initInits(), facts: createFacts())
  result.inits.enableJournaling()
  result.facts.enableJournaling()

proc checkpoint*(fs: FlowState): FlowCp {.inline.} =
  FlowCp(initsCp: fs.inits.checkpoint, factsCp: fs.facts.checkpoint)

proc rollbackTo*(fs: var FlowState; cp: FlowCp) {.inline.} =
  fs.inits.rollbackTo cp.initsCp
  fs.facts.rollbackTo cp.factsCp

proc snapshot*(fs: FlowState): FlowSnap =
  FlowSnap(inits: fs.inits.snapshot, facts: snapshotFacts(fs.facts))

proc inheritInits*(dst: var FlowState; src: FlowState) =
  ## Seed `dst`'s init-set with `src`'s — used at a *closure* boundary so a
  ## captured outer local stays proven-initialized inside the nested body.
  for s in src.inits.snapshot:
    dst.inits.incl s

proc joinSnap*(a, b: FlowSnap): FlowSnap =
  ## The lattice join (⊔): a var is initialized only if init on both sides, and
  ## a fact holds only if it holds on both (weakest common bound).
  var inits = initHashSet[SymId]()
  # iterate the smaller set (a HashSet never shrinks its slot array, so sizing
  # the result off `.len` matters for large procs)
  if a.inits.len <= b.inits.len:
    for x in a.inits:
      if x in b.inits: inits.incl x
  else:
    for x in b.inits:
      if x in a.inits: inits.incl x
  FlowSnap(inits: inits, facts: merge(a.facts, 0, b.facts, false))

proc setTo*(fs: var FlowState; cp: FlowCp; snap: FlowSnap) =
  ## Journaled `fs := snap`, rolled back to `cp` first so the diff stays small.
  fs.inits.rollbackTo cp.initsCp
  var toDel: seq[SymId] = @[]
  for k in fs.inits.snapshot:
    if k notin snap.inits: toDel.add k
  for k in toDel: fs.inits.excl k
  for k in snap.inits:
    fs.inits.incl k
  setFactsTo(fs.facts, cp.factsCp, snap.facts)

# --------------------------------------------------------------- the tracker

type
  ExitJEntry = object
    key: ExitKey[SymId]
    had: bool
    old: FlowSnap

  FlowTracker* = object
    live*: bool                       ## is fall-through reachable here?
    exits: Table[ExitKey[SymId], FlowSnap]
    journal: seq[ExitJEntry]          ## undo log for `exits` (an `ite` uses it)

  Branch* = object
    ## Snapshot carried from `splitBranch` to `mergeBranches`.
    baseLive: bool
    cp: FlowCp
    ejcp: int
    thenLive: bool
    thenInits: seq[SymId]             ## the then-branch's init *delta*
    thenFacts: Facts                  ## the then-branch's facts (if it lives)
    thenDelta: seq[ExitJEntry]        ## the then-branch's net exit contributions

proc initFlowTracker*(): FlowTracker =
  FlowTracker(live: true, exits: initTable[ExitKey[SymId], FlowSnap]())

# ---- journaled `exits` access

proc jPut(tr: var FlowTracker; key: ExitKey[SymId]; val: sink FlowSnap) =
  if key in tr.exits:
    tr.journal.add ExitJEntry(key: key, had: true, old: getOrDefault(tr.exits, key))
  else:
    tr.journal.add ExitJEntry(key: key, had: false)
  tr.exits[key] = val

proc jDel(tr: var FlowTracker; key: ExitKey[SymId]) =
  if key in tr.exits:
    tr.journal.add ExitJEntry(key: key, had: true, old: getOrDefault(tr.exits, key))
    tr.exits.del key

# ---- leaving

proc leaveVia(tr: var FlowTracker; fs: FlowState; key: ExitKey[SymId]) =
  ## Record the current state at `key` (joining with any earlier arrival) and
  ## stop falling through. A leave from a dead point is unreachable — ignored.
  if tr.live:
    let snap = snapshot(fs)
    if key in tr.exits:
      jPut(tr, key, joinSnap(getOrDefault(tr.exits, key), snap))
    else:
      jPut(tr, key, snap)
    tr.live = false

proc gotoLabel*(tr: var FlowTracker; fs: FlowState; label: SymId) {.inline.} =
  leaveVia(tr, fs, labelKey(label))
proc gotoReturn*(tr: var FlowTracker; fs: FlowState) {.inline.} =
  leaveVia(tr, fs, ExitKey[SymId](kind: ekReturn))
proc gotoRaise*(tr: var FlowTracker; fs: FlowState) {.inline.} =
  leaveVia(tr, fs, ExitKey[SymId](kind: ekRaise))
proc gotoContinue*(tr: var FlowTracker; fs: FlowState) {.inline.} =
  leaveVia(tr, fs, ExitKey[SymId](kind: ekContinue))

# ---- binding

proc bindKey(tr: var FlowTracker; fs: var FlowState; key: ExitKey[SymId]) =
  ## Consume `key`: join its accumulated state into fall-through and drop it.
  ## The journaled `setTo` diffs from the current state (a checkpoint taken here
  ## rolls back nothing), so an enclosing branch's checkpoint stays valid.
  if key in tr.exits:
    let landed = getOrDefault(tr.exits, key)
    jDel(tr, key)
    let here = fs.checkpoint
    if tr.live:
      setTo(fs, here, joinSnap(snapshot(fs), landed))
    else:
      setTo(fs, here, landed)
      tr.live = true

proc bindLabel*(tr: var FlowTracker; fs: var FlowState; label: SymId) {.inline.} =
  bindKey(tr, fs, labelKey(label))
proc bindReturn*(tr: var FlowTracker; fs: var FlowState) {.inline.} =
  bindKey(tr, fs, ExitKey[SymId](kind: ekReturn))
proc bindRaise*(tr: var FlowTracker; fs: var FlowState) {.inline.} =
  bindKey(tr, fs, ExitKey[SymId](kind: ekRaise))

proc bindLoopExit*(tr: var FlowTracker; fs: var FlowState; label: SymId) =
  ## The `(lab loopExit)` after a `(loop)`. A loop is analyzed in one forward
  ## pass (no fixpoint), so break-site *facts* may be iteration-specific and are
  ## dropped — `fs.facts` keeps the conservative pre-loop set the driver rolled
  ## back to. Definite-assignment is monotone, so break-site *inits* ARE joined
  ## (a var initialized before every break is initialized after the loop).
  let key = labelKey(label)
  if key in tr.exits:
    let landed = getOrDefault(tr.exits, key)
    jDel(tr, key)
    if tr.live:
      var keep: seq[SymId] = @[]
      for k in fs.inits.snapshot:
        if k notin landed.inits: keep.add k
      for k in keep: fs.inits.excl k
    else:
      # only break paths reach here: install their joined init-set
      var cur: seq[SymId] = @[]
      for k in fs.inits.snapshot: cur.add k
      for k in cur:
        if k notin landed.inits: fs.inits.excl k
      for k in landed.inits: fs.inits.incl k
      tr.live = true

proc dropContinue*(tr: var FlowTracker) {.inline.} =
  ## The loop header consumes the back-edge; a one-pass forward analysis drops it.
  jDel(tr, ExitKey[SymId](kind: ekContinue))

proc pending*(tr: FlowTracker; key: ExitKey[SymId]): bool {.inline.} =
  key in tr.exits

# ---- except / finally

proc takeRaise*(tr: var FlowTracker): bool =
  ## Drop the accumulated `raise` state (an `except` handler resumes from the
  ## conservative pre-`try` state, so it needs only whether a raise was pending).
  let key = ExitKey[SymId](kind: ekRaise)
  result = key in tr.exits
  if result: jDel(tr, key)

# ---- branching

proc splitBranch*(tr: var FlowTracker; fs: FlowState): Branch =
  Branch(baseLive: tr.live, cp: fs.checkpoint, ejcp: tr.journal.len)

proc commitThen*(tr: var FlowTracker; fs: var FlowState; b: var Branch) =
  ## After the then-branch: capture its delta (init keys, facts, exit
  ## contributions), then roll `fs` and `exits` back to the baseline so the
  ## else-branch starts clean. No whole-state copy — only the small delta.
  b.thenLive = tr.live
  if b.thenLive:
    b.thenInits = fs.inits.addedSince(b.cp.initsCp)
    b.thenFacts = snapshotFacts(fs.facts)
  # distinct exit keys the then-branch touched, with their post-then values
  b.thenDelta = @[]
  var seen = initHashSet[ExitKey[SymId]]()
  var i = tr.journal.len - 1
  while i >= b.ejcp:
    let k = tr.journal[i].key
    if not containsOrIncl(seen, k):
      if k in tr.exits:
        b.thenDelta.add ExitJEntry(key: k, had: true, old: getOrDefault(tr.exits, k))
      else:
        b.thenDelta.add ExitJEntry(key: k, had: false)
    dec i
  # undo the then-branch's exit-table mutations
  while tr.journal.len > b.ejcp:
    let e = tr.journal[tr.journal.len - 1]
    tr.journal.setLen(tr.journal.len - 1)
    if e.had: tr.exits[e.key] = e.old
    else: tr.exits.del e.key
  # roll the state back to the baseline the then-branch started from
  fs.rollbackTo b.cp
  tr.live = b.baseLive

proc mergeBranches*(tr: var FlowTracker; fs: var FlowState; b: Branch) =
  ## After the else-branch: join the then-branch (in `b`) with the current
  ## else-branch (in `fs`/`tr`). Fall-through survives iff *some* arm falls
  ## through; a leaving arm drops out (guard-clause style).
  for e in b.thenDelta:
    if e.had:
      if e.key in tr.exits:
        jPut(tr, e.key, joinSnap(getOrDefault(tr.exits, e.key), e.old))
      else:
        jPut(tr, e.key, e.old)
  let elseLive = tr.live
  if b.thenLive and elseLive:
    # merged init-set = baseline + (thenDelta ∩ elseDelta); merged facts = ⊔.
    let elseDelta = fs.inits.addedSince(b.cp.initsCp)
    var thenSet = initHashSet[SymId]()
    for k in b.thenInits: thenSet.incl k
    let mergedFacts = merge(b.thenFacts, 0, fs.facts, false)
    fs.inits.rollbackTo b.cp.initsCp
    for k in elseDelta:
      if k in thenSet: fs.inits.incl k
    setFactsTo(fs.facts, b.cp.factsCp, mergedFacts)
    tr.live = true
  elif b.thenLive:
    # else left; the post-`if` state is the then-branch's.
    fs.rollbackTo b.cp
    for k in b.thenInits: fs.inits.incl k
    setFactsTo(fs.facts, b.cp.factsCp, b.thenFacts)
    tr.live = true
  else:
    tr.live = elseLive   # `fs` already holds the else-branch state

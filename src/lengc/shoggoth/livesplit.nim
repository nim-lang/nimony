#
#
#           NIFC Live-Range Splitting (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Renames a local's independent value ranges apart, so a value that merely
## *shares a name* with a value living across a call does not pay for that call.
##
## ```
## (var :x . (i +32) (call f))     (var :x . (i +32) (call f))
## (asgn y x)                      (asgn y x)
## (call g)                   ⇒    (call g)
## (asgn x (call h))               (var :`split.0.M . (i +32) (call h))
## (asgn z x)                      (asgn z `split.0.M)
## ```
##
## Arkham decides per local whether it may live in a caller-saved register, and
## the rule is one coarse interval per NAME: no call position may lie within
## `[declaration, last occurrence]` (`arkham/core/analyser.nim`). Above, `x`'s
## interval spans `(call g)`, so `x` takes a callee-saved register and the proc
## pays a `stp`/`ldp` pair — even though no *value* of `x` is ever live across
## `g`. The two assignments define two independent values that happen to share a
## slot because the frontend reused the name. Renaming them apart is the whole
## transformation: nothing is copied, nothing is spilled.
##
## The design, the corpus measurement that motivates it and the phases beyond
## this one are in `doc/internals/split_opt.md`. This module is **phase 1**.
##
## ## What phase 1 splits
##
## A split point is a whole-variable assignment `(asgn x <value>)` that is
## **unconditionally executed** between the occurrences before it and the
## occurrences after it. Then no reasoning about reaching definitions is needed
## at all: every occurrence textually before the assignment reads the old value,
## every occurrence from it onwards reads the new one, on every path. The
## rewrite is positional, and its soundness rests on four conditions, all of
## them checked here:
##
## 1. **Unconditional.** The assignment is nested only in `stmts` / `scope`
##    wrappers — never inside an `if` / `case` / `while` / `loop` / `try` /
##    `onerr` arm, which control flow may skip.
## 2. **Not jumped over.** No `jmp L` before it has its `lab L` after it. Leng's
##    jumps are forward and scoped (`doc/internals/final_ir.md`), so a jump can
##    only skip a *span*, and a split point inside one such span is refused.
##    `xelim` emits these for every short-circuit `and`/`or`, so this is not a
##    rare shape.
## 3. **Self-reference free.** The assigned value must not mention `x` itself
##    (`x = x + 1` reads the OLD value at a position the rewrite would rename).
## 4. **Storage not observed.** No `addr`/`haddr` roots at the local
##    (`addrRootOf`, the same gate `copyprop` and `scalarizer` use), so no
##    second name for the same storage can be observed. This is also what
##    removes `=wasMoved(haddr x)` locals and the const-ref aggregates
##    `constparams` addresses.
##
## Destructible locals are *candidates*, not exclusions: `=destroy` takes the
## object rather than its address, so `(call =destroy x)` is an ordinary use of
## whichever value is live there, and ARC destroys the old value BEFORE the
## reassignment that opens the next range — so each range ends with its own
## destruction and the rename copies that structure one-to-one.
##
## ## What it does NOT do
##
## **It never inserts a copy.** The textbook second half of live-range splitting
## cuts a genuinely-live range and inserts `x2 = x1`; that buys nothing here,
## since a value live across a call needs a callee-saved register whether it is
## called `x` or `x2`. Splitting pays exactly when the value is *dead* across
## the call, and then no copy is needed.
##
## **It splits only when that removes a call.** An unconditional renaming would
## grow the IR and the C backend's local count for nothing. The cost model is
## the interval test itself, run on the ranges the split would produce.
##
## Loops (a range carried across a back edge is one value however it looks
## textually) and `onerr` edges are phase 2 and 3; a body containing `onerr` is
## skipped whole, and a candidate with an occurrence inside a loop is dropped.

import std / [assertions, tables, os, syncio]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind
import ".." / ".." / "models" / tags         # VarTagId for synthesis
import patchsets

let splitStats = existsEnv("SHOGGOTH_SPLIT_STATS")
  ## `SHOGGOTH_SPLIT_STATS=1` writes one line per body to stderr saying how many
  ## candidates each gate removed. The question this pass has to keep answering is
  ## not "does it fire" but "is what it does not reach worth reaching", and that is
  ## a per-gate count, not a total.

var
  gCands, gAddrTaken, gInLoop, gNoDef, gRegion, gJumped, gNotWorth, gSplit: int

type
  SplitPoint = object
    stmtPos: int              ## position of the `(asgn …)` to replace with a `(var …)`
    valuePos: int             ## position of its value subtree

  Cand = object
    declPos: int              ## position of the `(var :x …)` declaration
    pragmaPos: int            ## position of the decl's pragma slot (copied verbatim)
    typePos: int              ## position of the decl's type slot (copied verbatim)
    valuePos: int             ## position of the decl's initializer (`-1` when there is none)
    occs: seq[int]            ## every `Symbol` occurrence, in walk order
    splits: seq[SplitPoint]   ## the candidate split points, in walk order
    region: seq[int]          ## the declaration's REGION PATH (see `Context.region`)
    wholeDefs: int            ## whole-variable assignments seen, split point or not
    inLoop: bool              ## disqualified BY the loop rule specifically (stats only)
    bad: bool                 ## disqualified (address-taken, declared twice, in a loop, …)

  Context = object
    orig: ptr TokenBuf
    suffix: string
    counter: int
    cands: Table[SymId, Cand]
    callPositions: seq[int]
    jmpSpans: seq[(int, int)]  ## (jmp position, matching lab position)
    pendingJmps: Table[SymId, int]  ## label → earliest jmp seen for it
    hasOnerr: bool
    region: seq[int]           ## the stack of conditionally-executed subtrees currently
                               ## open, each identified by its own token position: one entry
                               ## per CHILD of an `if`/`case`/`while`/… that the walk has
                               ## descended into. Two statements are unconditional with
                               ## respect to each other exactly when their paths are EQUAL —
                               ## a shorter path means one of them can be skipped while the
                               ## other runs, and two different paths of the same length are
                               ## sibling arms, which are mutually exclusive.
    loopDepth: int

const
  CondStmts = {IfS, CaseS, WhileS, LoopS, TryS, OnerrS, IteS, ItecS}
    ## Statements whose children control flow may skip or repeat: each child is
    ## its own region.
  LoopStmts = {WhileS, LoopS}
  NestStmts = {ScopeS}
    ## `scope` does not branch, but it BOUNDS a lifetime — the C backend emits it
    ## as `{ … }` (`genstmts.genScope`) and arkham gives it its own frame — and the
    ## split's fresh `(var …)` is declared exactly where the assignment was. A split
    ## point one scope deeper than its own declaration would therefore put the new
    ## name out of scope at the uses it has to reach, so a scope is a region
    ## boundary. `stmts` is NOT: it is statement grouping only, emitted with no
    ## braces at all (`genstmts.genStmt`) and sharing arkham's enclosing scope
    ## frame, so a declaration inside one outlives it.

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc addrRootOf(c: Cursor): SymId =
  ## The *storage* root of an `addr` operand — the local whose address is
  ## actually taken. Stops at a through-pointer step (`deref`/`pat`): `addr
  ## (*p).f` addresses the pointee, so `p`'s own storage is not addressed and
  ## `p` stays splittable. A verbatim copy of `copyprop.addrRootOf`, which is
  ## the exact test `scalarizer` uses too; an unmodelled spine falls back to the
  ## conservative `rootOf`, so the relaxation is one-directional.
  result = SymId(0)
  var n = c
  while true:
    case n.kind
    of Symbol: return symId(n)
    of TagLit:
      case n.exprKind
      of DerefC, PatC: return SymId(0)
      of DotC, AtC: inc n
      of ConvC, CastC:
        inc n; skip n
      else: return rootOf(n)
    else: return SymId(0)

proc mentionsImpl(n: var Cursor; s: SymId): bool =
  ## Advances `n` past the subtree; true iff it contains an occurrence of `s`.
  case n.kind
  of Symbol:
    result = symId(n) == s
    inc n
  of TagLit:
    result = false
    n.into:
      while n.hasMore:
        if mentionsImpl(n, s): result = true
  else:
    result = false
    inc n

proc mentions(n: Cursor; s: SymId): bool =
  var probe = n
  result = mentionsImpl(probe, s)

# ── collection ───────────────────────────────────────────────────────────────

proc disqualify(c: var Context; s: SymId) =
  if s != SymId(0):
    c.cands.mgetOrPut(s, Cand(declPos: -1, bad: true)).bad = true

proc recordDecl(c: var Context; n: Cursor) =
  ## `(var :x <pragmas> <type> <value>)`: open a candidate. A name declared
  ## twice cannot be told apart by SymId, so the second sighting kills it.
  var v = child0(n)
  if v.kind != SymbolDef: return
  let s = symId(v)
  let declPos = cursorToPosition(c.orig[], n)
  if c.cands.hasKey(s):
    c.cands[s].bad = true
    return
  var cand = Cand(declPos: declPos, pragmaPos: -1, typePos: -1, valuePos: -1)
  inc v
  if v.hasMore:
    cand.pragmaPos = cursorToPosition(c.orig[], v)
    skip v
  if v.hasMore:
    cand.typePos = cursorToPosition(c.orig[], v)
    skip v
  if v.hasMore:
    cand.valuePos = cursorToPosition(c.orig[], v)
  # A declaration inside a loop is per-iteration: phase 1 leaves those alone.
  # A declaration inside a conditional arm is fine — a split point in that SAME
  # arm is unconditional with respect to every occurrence of the local, which is
  # all the rewrite needs (see `Context.region`).
  if c.loopDepth > 0:
    cand.bad = true
    cand.inLoop = true
  cand.region = c.region
  c.cands[s] = cand

proc collect(c: var Context; n: var Cursor)

proc collectChildren(c: var Context; n: var Cursor) =
  n.into:
    while n.hasMore: collect(c, n)

proc collect(c: var Context; n: var Cursor) =
  case n.kind
  of Symbol:
    let s = symId(n)
    if c.cands.hasKey(s):
      c.cands[s].occs.add cursorToPosition(c.orig[], n)
      # An occurrence inside a loop the declaration sits outside of is carried
      # across the back edge: textual order does not show which value it reads,
      # so phase 1 drops the candidate rather than guess.
      if c.loopDepth > 0:
        c.cands[s].bad = true
        c.cands[s].inLoop = true
    inc n
  of TagLit:
    case n.stmtKind
    of AsgnS:
      # A split point is a whole-variable assignment: `(asgn <bare sym> <value>)`.
      # `(asgn (dot x f) …)` / `(asgn (at x i) …)` are PARTIAL writes and
      # `(store <expr> x)` has its operands reversed — both are ordinary uses of
      # the existing value, so they must not open a range.
      var lhs = child0(n)
      let stmtPos = cursorToPosition(c.orig[], n)
      if lhs.kind == Symbol and c.cands.hasKey(symId(lhs)):
        let s = symId(lhs)
        inc c.cands[s].wholeDefs
        var val = lhs
        inc val
        if val.hasMore and c.loopDepth == 0 and c.region == c.cands[s].region and
           not mentions(val, s):
          c.cands[s].splits.add SplitPoint(stmtPos: stmtPos,
                                           valuePos: cursorToPosition(c.orig[], val))
      collectChildren(c, n)
    of CallS:
      c.callPositions.add cursorToPosition(c.orig[], n)
      collectChildren(c, n)
    of JmpS:
      var t = child0(n)
      if t.kind == Symbol:
        let L = symId(t)
        let p = cursorToPosition(c.orig[], n)
        if not c.pendingJmps.hasKey(L): c.pendingJmps[L] = p
      skip n
    of LabS:
      var t = child0(n)
      if t.kind == SymbolDef:
        let L = symId(t)
        if c.pendingJmps.hasKey(L):
          c.jmpSpans.add (c.pendingJmps[L], cursorToPosition(c.orig[], n))
          c.pendingJmps.del L
      skip n
    of OnerrS:
      c.hasOnerr = true
      skip n
    of VarS:
      recordDecl(c, n)
      collectChildren(c, n)
    of GvarS, TvarS, ConstS, ProcS, TypeS:
      skip n                                   # not a local's business
    else:
      if n.stmtKind in NestStmts:
        c.region.add cursorToPosition(c.orig[], n)
        collectChildren(c, n)
        discard c.region.pop()
      elif n.stmtKind in CondStmts:
        let isLoop = n.stmtKind in LoopStmts
        if isLoop: inc c.loopDepth
        n.into:
          while n.hasMore:
            # Each child of a branching statement is its own region: the `if`'s
            # condition, each `elif`/`else` arm, a loop's body. Statements in two
            # different children never both run on one straight path.
            c.region.add cursorToPosition(c.orig[], n)
            collect(c, n)
            discard c.region.pop()
        if isLoop: dec c.loopDepth
      elif n.exprKind in {AddrC, HaddrC}:
        disqualify(c, addrRootOf(child0(n)))
        collectChildren(c, n)
      else:
        collectChildren(c, n)
  else:
    inc n

# ── cost model ───────────────────────────────────────────────────────────────

proc rangeLo(c: Context; nodePos, valuePos: int): int =
  ## Where a range's live interval OPENS, mirroring arkham's birth-point
  ## exemption: when the opening initializer is a root `(call …)`, the value's
  ## home receives its first write only after that call returned, so the call
  ## cannot clobber it and must not count as crossed. Any other initializer
  ## shape may stage a partial value in the destination, so it opens at the
  ## statement. `- 1` because the position just past the subtree can coincide
  ## with the next statement's own, and a call THERE is after the birth.
  result = nodePos
  if valuePos < 0: return
  var v = cursorAt(c.orig[], valuePos)
  if v.kind == TagLit and v.stmtKind == CallS:
    var e = v
    skip e
    result = cursorToPosition(c.orig[], e) - 1

proc crossesCall(c: Context; lo, hi: int): bool =
  ## Arkham's own interval test: a call position strictly after `lo` and at or
  ## before `hi` denies the range a caller-saved register. `lo` is the range's
  ## opening declaration/assignment, `hi` its last occurrence.
  for p in c.callPositions:
    if p > lo and p <= hi: return true
  false

proc spannedByJmp(c: Context; pos: int): bool =
  for (j, l) in c.jmpSpans:
    if j < pos and pos < l: return true
  false

proc worthSplitting(c: Context; cand: Cand; usable: seq[SplitPoint]): bool =
  ## True iff the split turns a call-crossing range into at least one range that
  ## crosses none. Splitting a range that was already call-free, or one that
  ## every piece still crosses, only grows the IR.
  if usable.len == 0 or cand.occs.len == 0: return false
  let lastOcc = cand.occs[^1]
  if not crossesCall(c, rangeLo(c, cand.declPos, cand.valuePos), lastOcc): return false
  var starts = @[cand.declPos]
  var los = @[rangeLo(c, cand.declPos, cand.valuePos)]
  for sp in usable:
    starts.add sp.stmtPos
    los.add rangeLo(c, sp.stmtPos, sp.valuePos)
  for i in 0 ..< starts.len:
    let lo = los[i]
    let hi = if i + 1 < starts.len: starts[i + 1] - 1 else: lastOcc
    var lastInRange = -1
    for o in cand.occs:
      if o > starts[i] and o <= hi: lastInRange = o
    if lastInRange < 0: continue          # an empty range costs nothing either way
    if not crossesCall(c, lo, lastInRange): return true
  false

# ── synthesis ────────────────────────────────────────────────────────────────

proc freshName(c: var Context): string =
  inc c.counter
  result = "`split." & $c.counter & "." & c.suffix

proc buildVarDecl(c: var Context; name: string; cand: Cand;
                  sp: SplitPoint; synth: var seq[TokenBuf]): int =
  ## `(var :name <pragmas> <type> <value>)`, with the declaration's own pragma
  ## and type subtrees copied verbatim — the new name denotes the same storage
  ## shape, so nothing about it is inferred.
  result = synth.len
  var buf = createTokenBuf(24, c.orig[].pool, c.orig[].tags)
  let value = cursorAt(c.orig[], sp.valuePos)
  buf.openTag TagId(ord(VarTagId))
  let li = rawLineInfo(value)
  if li.isValid: buf.appendLineInfo li
  buf.addSymDef name
  if cand.pragmaPos >= 0: buf.addSubtree cursorAt(c.orig[], cand.pragmaPos)
  else: buf.addDotToken()
  if cand.typePos >= 0: buf.addSubtree cursorAt(c.orig[], cand.typePos)
  else: buf.addDotToken()
  buf.addSubtree value
  buf.closeTag()
  synth.add ensureMove(buf)

proc buildSymUse(c: var Context; name: string; synth: var seq[TokenBuf]): int =
  result = synth.len
  var buf = createTokenBuf(2, c.orig[].pool, c.orig[].tags)
  buf.addSymUse name
  synth.add ensureMove(buf)

# ── entry point ──────────────────────────────────────────────────────────────

proc runLiveSplit*(buf: var TokenBuf; suffix = "ls"): int {.discardable.} =
  ## Split every candidate local in one body. Returns the number of ranges
  ## split off (i.e. fresh names minted), for the driver's statistics.
  result = 0
  var c = Context(orig: addr buf, suffix: suffix,
                  cands: initTable[SymId, Cand](),
                  pendingJmps: initTable[SymId, int]())
  block:
    var n = beginRead(buf)
    collect(c, n)
  if c.hasOnerr: return 0                 # phase 3

  var synth: seq[TokenBuf] = @[]
  var ps = initPatchset(addr buf)
  # `substs` is keyed by original position so the patchset stays a single
  # rebuild pass: `stmtPos` gets the fresh `(var …)`, each later occurrence the
  # fresh symbol.
  var declSubst: seq[(int, int)] = @[]    # (position, synth index)
  var useSubst: seq[(int, int)] = @[]

  for s, cand in c.cands.pairs:
    if cand.declPos < 0: continue
    inc gCands
    if cand.bad:
      if cand.inLoop: inc gInLoop else: inc gAddrTaken
      continue
    if cand.wholeDefs == 0: (inc gNoDef; continue)
    if cand.splits.len == 0: (inc gRegion; continue)
    var usable: seq[SplitPoint] = @[]
    for sp in cand.splits:
      if not spannedByJmp(c, sp.stmtPos): usable.add sp
    if usable.len == 0: (inc gJumped; continue)
    if not worthSplitting(c, cand, usable): (inc gNotWorth; continue)
    inc gSplit
    # Range i (i >= 1) runs from `usable[i-1].stmtPos` to just before the next
    # split point; every occurrence in it takes range i's fresh name. Range 0
    # keeps the original name, so the declaration and everything reaching the
    # proc's `ret` are untouched.
    var names: seq[string] = @[]
    for sp in usable:
      let nm = c.freshName()
      names.add nm
      declSubst.add (sp.stmtPos, c.buildVarDecl(nm, cand, sp, synth))
      inc result
    for o in cand.occs:
      var idx = -1
      for i in 0 ..< usable.len:
        if o > usable[i].stmtPos: idx = i
      if idx >= 0:
        useSubst.add (o, c.buildSymUse(names[idx], synth))

  if splitStats:
    stderr.writeLine "SPLITSTATS cands=" & $gCands & " addrtaken=" & $gAddrTaken &
      " inloop=" & $gInLoop & " nodef=" & $gNoDef & " region=" & $gRegion &
      " jumped=" & $gJumped & " notworth=" & $gNotWorth & " split=" & $gSplit
  if declSubst.len == 0 and useSubst.len == 0: return 0
  for (pos, idx) in declSubst: ps.addSubst(pos, cursorAt(synth[idx], 0))
  for (pos, idx) in useSubst: ps.addSubst(pos, cursorAt(synth[idx], 0))
  var newBuf = ps.apply()
  buf = ensureMove(newBuf)

# ── self-tests ───────────────────────────────────────────────────────────────

when isMainModule:
  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc split(src: string): string =
    var b = parse(src)
    runLiveSplit(b, "t")
    toString(b)

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  block splits_a_dead_range_off:
    # The motivating shape: `x`'s second value is born after the call that its
    # first value merely spans, so the two want different registers.
    let got = split("(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn y.0.M x.0.M) " &
                    "(call g.0.M) (asgn x.0.M (call h.0.M)) (asgn z.0.M x.0.M))")
    let want = canon("(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn y.0.M x.0.M) " &
                     "(call g.0.M) (var :`split.1.t . (i +32) (call h.0.M)) " &
                     "(asgn z.0.M `split.1.t))")
    doAssert got == want, got

  block no_call_no_split:
    # Nothing crosses a call, so the rename would buy nothing.
    let src = "(stmts (var :x.0.M . (i +32) 1) (asgn y.0.M x.0.M) " &
              "(asgn x.0.M 2) (asgn z.0.M x.0.M))"
    doAssert split(src) == canon(src), split(src)

  block self_reference_is_not_a_new_value:
    # `x = x + 1` READS the old value at a position the rewrite would rename.
    let src = "(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn y.0.M x.0.M) " &
              "(call g.0.M) (asgn x.0.M (add (i +32) x.0.M 1)) (asgn z.0.M x.0.M))"
    doAssert split(src) == canon(src), split(src)

  block conditional_split_point_is_refused:
    # The assignment may be skipped, so a later use is reached by BOTH values.
    let src = "(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn y.0.M x.0.M) " &
              "(call g.0.M) (if (elif c.0.M (stmts (asgn x.0.M (call h.0.M))))) " &
              "(asgn z.0.M x.0.M))"
    doAssert split(src) == canon(src), split(src)

  block address_taken_is_refused:
    # A second name for storage something else can reach through a pointer.
    let src = "(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn y.0.M x.0.M) " &
              "(call g.0.M) (asgn p.0.M (addr x.0.M)) (asgn x.0.M (call h.0.M)) " &
              "(asgn z.0.M x.0.M))"
    doAssert split(src) == canon(src), split(src)

  block jumped_over_split_point_is_refused:
    # `jmp L` skips the assignment, so the use after `lab L` may read either value.
    let src = "(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn y.0.M x.0.M) " &
              "(call g.0.M) (jmp L.0.M) (asgn x.0.M (call h.0.M)) (lab :L.0.M) " &
              "(asgn z.0.M x.0.M))"
    doAssert split(src) == canon(src), split(src)

  block deeper_scope_is_refused:
    # The fresh `(var …)` would die at the inner scope's end, before its use.
    let src = "(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn y.0.M x.0.M) " &
              "(call g.0.M) (scope (asgn x.0.M (call h.0.M))) (asgn z.0.M x.0.M))"
    doAssert split(src) == canon(src), split(src)

  block loop_use_is_refused:
    # A use inside a loop is re-read across the back edge: textual order does
    # not say which value it reads.
    let src = "(stmts (var :x.0.M . (i +32) (call f.0.M)) " &
              "(while c.0.M (stmts (asgn y.0.M x.0.M))) " &
              "(call g.0.M) (asgn x.0.M (call h.0.M)) (asgn z.0.M x.0.M))"
    doAssert split(src) == canon(src), split(src)

  block partial_write_is_not_a_split_point:
    # `(asgn (dot x f) …)` updates part of the SAME value.
    let src = "(stmts (var :x.0.M . T.0.M (call f.0.M)) (asgn y.0.M (dot x.0.M a.0 0)) " &
              "(call g.0.M) (asgn (dot x.0.M a.0 0) 1) (asgn z.0.M (dot x.0.M a.0 0)))"
    doAssert split(src) == canon(src), split(src)

  block two_split_points:
    let got = split("(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn a.0.M x.0.M) " &
                    "(call g.0.M) (asgn x.0.M (call h.0.M)) (asgn b.0.M x.0.M) " &
                    "(call g.0.M) (asgn x.0.M (call h.0.M)) (asgn c.0.M x.0.M))")
    let want = canon("(stmts (var :x.0.M . (i +32) (call f.0.M)) (asgn a.0.M x.0.M) " &
                     "(call g.0.M) (var :`split.1.t . (i +32) (call h.0.M)) " &
                     "(asgn b.0.M `split.1.t) (call g.0.M) " &
                     "(var :`split.2.t . (i +32) (call h.0.M)) (asgn c.0.M `split.2.t))")
    doAssert got == want, got

  echo "livesplit.nim: all self-tests passed"

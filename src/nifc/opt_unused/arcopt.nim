#
#
#           NIFC ARC Optimizer
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## arcopt eliminates redundant `=destroy(x)` calls and the `=wasMoved(x)`
## calls that paired with them. The pass is two-phase: a forward walk
## tracks per-symbol "moved" state and records position-keyed
## substitution patches via the shared [patchsets](patchsets.nim)
## module; then the buffer is rebuilt with every eliminated call
## collapsed to a single `DotToken` no-op statement.
##
## See [arcopt.md](../../../doc/arcopt.md) for the design.
##
## What this pass handles:
##
## - `=destroy(x)` is elided when on every reaching path a `=wasMoved(x)`
##   preceded it without an intervening use or assignment of `x`.
## - `=wasMoved(x)` calls that paired with an elided destroy are *also*
##   elided — including the branched case where different branches contain
##   different `=wasMoved` positions. The positions tracker is combined
##   across siblings by **set union** at joins
##   (`closeBranchesAdditive`).
## - Tainting: any `Symbol` use or assignment observes the currently
##   tracked positions for that symbol and marks them as `tainted`. A
##   tainted position is never elided even if it later appears in the
##   union at a destroy site — its effect was observed on some path, so
##   eliding it would change behavior.
## - `(if (elif … …) … (else …))` and `(case sel (of … …) … (else …))`
##   via sibling-group tracking on `Tracker[SymId, _]`.
## - `(jmp L)` / `(lab L)`: moved bits route via the tracker's label
##   merge; positions are conservatively cleared (label-merge with union
##   semantics is left for a follow-up).
## - `(while cond body)` / `(loop …)` conservatively: every tracked fact
##   is cleared on entry and on exit.

import std / [tables, sets, assertions]
include "../../lib" / nifprelude
# nifstreams / nifcursors come from the included nifprelude; bare-importing them
# resolves to the wrong path under nimony boot. The self-tests still qualify
# `nifstreams.openFromBuffer` via the included import.
import ".." / nifc_model
import "../.." / lib / symparser
import trackers, patchsets
# `nifrender` (render / assertRender) is only used by the host-Nim self-tests
# below; exclude it under nimony (guarded by `defined(nimony)`, which nifler can
# evaluate when building the boot dependency graph — `isMainModule` cannot) so
# nimony need not self-compile it during boot (it relies on host-only `doAssert`).
when not defined(nimony):
  import nifrender

type
  Context = object
    orig: ptr TokenBuf
    moved: Tracker[SymId, bool]                 ## "wasMoved seen on every path?"
    positions: Tracker[SymId, HashSet[int]]     ## per-symbol set of `=wasMoved`
                                                ## positions still paired with a
                                                ## potential downstream destroy.
                                                ## Combined by **union** at joins.
    tainted: HashSet[int]                       ## monotonic: positions observed
                                                ## by a `use`/`asgn`. Never
                                                ## elided even if they survive
                                                ## to a destroy.
    patchset: Patchset
    dotBuf: TokenBuf                            ## holds a single `DotToken` —
                                                ## the substitution target for
                                                ## every elided call.

proc createContext(orig: ptr TokenBuf): Context =
  result = Context(orig: orig,
                   moved: initTracker[SymId, bool](),
                   positions: initTracker[SymId, HashSet[int]](),
                   tainted: initHashSet[int](),
                   patchset: initPatchset(orig),
                   dotBuf: createTokenBuf(2))
  result.dotBuf.addDotToken()

# ---- per-symbol position helpers ------------------------------------------

proc addPosition(c: var Context; sym: SymId; pos: int) =
  ## Record a new wasMoved position for `sym`.
  var s = c.positions[sym]
  if pos in s: return
  s.incl pos
  c.positions[sym] = s

proc taintCurrent(c: var Context; sym: SymId) =
  ## Mark every position currently tracked for `sym` as observed (tainted).
  ## Subsequent destroy elision will skip these positions.
  let s = c.positions[sym]
  if s.len == 0: return
  for p in s: c.tainted.incl p
  c.positions[sym] = initHashSet[int]()        # equivalent to delete

# ---- branch-state forwarding to both trackers -----------------------------

proc openBranches(c: var Context) =
  c.moved.openBranches()
  c.positions.openBranches()

proc openBranch(c: var Context) =
  c.moved.openBranch()
  c.positions.openBranch()

proc openFinalBranch(c: var Context) =
  c.moved.openFinalBranch()
  c.positions.openFinalBranch()

proc closeBranch(c: var Context) =
  c.moved.closeBranch()
  c.positions.closeBranch()

proc closeBranches(c: var Context) =
  c.moved.closeBranches()              # intersection on bools
  c.positions.closeBranchesAdditive()  # union on sets

proc gotoLabel(c: var Context; L: LabelId) =
  ## Conservative for the positions tracker: clear all sets. The moved bit
  ## still routes via the label-merge mechanism. Refinement (additive
  ## landLabel) is left for a future iteration.
  c.moved.gotoLabel L
  c.positions.clearAll()

proc landLabel(c: var Context; L: LabelId) =
  c.moved.landLabel L
  c.positions.clearAll()

proc clearAllFacts(c: var Context) =
  c.moved.clearAll()
  c.positions.clearAll()

proc resetPerProc(c: var Context) =
  ## Fresh state at a proc boundary. NIFC locals (`x.0`, `result.0`, …) are
  ## unique only within a proc, so the SymId-keyed `moved`/`positions` trackers
  ## must not carry across procs or two procs' identically-named locals would
  ## alias. `tainted` is position-keyed (no cross-proc aliasing) but reset too.
  c.moved.clearAll()
  c.positions.clearAll()
  c.tainted = initHashSet[int]()

proc markDiverged(c: var Context) =
  ## The current path leaves the block (`return`/`raise`/`break`). The trackers
  ## remember it (see `Tracker.markDiverged`) and drop this branch from the
  ## enclosing join at `closeBranch`; full divergence propagates outward on its
  ## own, so the walker never computes divergence itself.
  c.moved.markDiverged()
  c.positions.markDiverged()

# ---- hook-symbol recognition ----------------------------------------------

type HookKind = enum
  NoHook, WasMovedHook, DestroyHook

proc classifyHook(symId: SymId): HookKind =
  ## `=destroy.0.Foo` → DestroyHook, `=wasMoved.0.Foo` → WasMovedHook.
  let raw = pool.syms[symId]
  var isGlobal = false
  let base = extractBasename(raw, isGlobal)
  case base
  of "=destroy": DestroyHook
  of "=wasMoved": WasMovedHook
  else: NoHook

proc hookTarget(arg: Cursor): SymId =
  ## For `=wasMoved`/`=destroy` arguments, find the underlying variable.
  ## Recognizes `x` and `(addr x)`. Returns `SymId(0)` when the target is
  ## not a plain symbol (the call is then left alone).
  var n = arg
  if n.kind == ParLe and n.exprKind == AddrC:
    inc n
    if n.kind == Symbol: return n.symId
    return SymId(0)
  if n.kind == Symbol:
    return n.symId
  return SymId(0)

# ---- elision via patchset --------------------------------------------------

proc elideAt(c: var Context; pos: int) =
  ## Record a substitution of the subtree at `pos` with a single
  ## `DotToken` (a no-op statement). Applied during the patchset's
  ## rebuild pass.
  c.patchset.addSubst(pos, cursorAt(c.dotBuf, 0))

# ---- main traversal --------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward decl

proc trExpr(c: var Context; n: var Cursor) =
  ## Walk one expression subtree. Every `Symbol` it references is an
  ## *observation* — clear the moved bit and taint any currently-tracked
  ## wasMoved positions so they cannot be elided later.
  case n.kind
  of Symbol:
    c.moved[n.symId] = false
    taintCurrent(c, n.symId)
    inc n
  of ParLe:
    n.loopInto:
      trExpr(c, n)
  else:
    inc n

proc handleHookCall(c: var Context; n: var Cursor): bool =
  ## If `n` is a `=wasMoved` / `=destroy` call on a plain symbol target,
  ## update tracker state (and possibly elide in-place), advance `n`, and
  ## return true. Otherwise return false (caller treats it as an ordinary
  ## call).
  var probe = n
  inc probe                            # past (call
  if probe.kind != Symbol: return false
  let hook = classifyHook(probe.symId)
  if hook == NoHook: return false
  inc probe                            # past callee symbol
  if not probe.hasMore: return false
  let target = hookTarget(probe)
  if target == SymId(0): return false
  let callPos = cursorToPosition(c.orig[], n)
  case hook
  of WasMovedHook:
    c.moved[target] = true
    addPosition(c, target, callPos)
  of DestroyHook:
    if c.moved[target]:
      elideAt(c, callPos)
      for p in c.positions[target]:
        if p notin c.tainted:
          elideAt(c, p)
    c.moved[target] = false
    c.positions[target] = initHashSet[int]()
  of NoHook: discard
  skip n
  return true

proc trCall(c: var Context; n: var Cursor) =
  if handleHookCall(c, n):
    return
  # Ordinary call: walk every child as an expression so symbol uses clear
  # their moved facts.
  n.loopInto:
    trExpr(c, n)

proc trAsgn(c: var Context; n: var Cursor) =
  ## `(asgn lhs rhs)` / `(store rhs lhs)`. Every plain symbol on either
  ## side becomes a use (clears its bits).
  n.loopInto:
    trExpr(c, n)

proc trIf(c: var Context; n: var Cursor) =
  ## `closeBranch` drops any branch whose body diverged; `closeBranches`
  ## propagates "every arm diverged" outward — see `trackers.markDiverged`.
  openBranches c
  n.loopInto:
    case n.substructureKind
    of ElifU:
      n.into:
        trExpr(c, n)                   # condition
        openBranch c
        tr(c, n)                       # body (may set the diverged flag)
        closeBranch c
    of ElseU:
      n.into:
        openFinalBranch c              # else makes the if exhaustive
        tr(c, n)
        closeBranch c
    else:
      skip n
  closeBranches c

proc trCase(c: var Context; n: var Cursor) =
  n.into:
    trExpr(c, n)                       # selector
    openBranches c
    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          skip n                       # ranges (constants — no clearing)
          openBranch c
          tr(c, n)
          closeBranch c
      of ElseU:
        n.into:
          openFinalBranch c
          tr(c, n)
          closeBranch c
      else:
        skip n
    closeBranches c

proc trLoop(c: var Context; n: var Cursor) =
  ## A loop is conservatively opaque: clear all facts on entry and exit.
  clearAllFacts c
  skip n
  clearAllFacts c

proc trJmp(c: var Context; n: var Cursor) =
  ## `(jmp L)` — snapshot current state under L.
  var probe = n
  inc probe
  if probe.kind == Symbol:
    gotoLabel(c, LabelId(probe.symId.uint32))
  skip n

proc trLab(c: var Context; n: var Cursor) =
  ## `(lab L)` — merge incoming snapshots with the current fall-through.
  var probe = n
  inc probe
  if probe.kind == SymbolDef:
    landLabel(c, LabelId(probe.symId.uint32))
  skip n

proc trBreakOrRet(c: var Context; n: var Cursor) =
  ## `break` / `ret` / `raise` leave the block: mark the path diverged so the
  ## enclosing branch is dropped from its join. Also clear facts so nothing
  ## leaks into following dead code on paths that are not inside a branch.
  skip n
  markDiverged c
  clearAllFacts c

proc tr(c: var Context; n: var Cursor) =
  ## Walk one statement/node, updating tracker state. Divergence is recorded in
  ## the trackers' `pathDiverged` flag (set by `markDiverged`/`gotoLabel`, reset
  ## per branch), not returned — so nesting and joins are handled by the
  ## tracker as it sees `openBranch`/`closeBranch`/`closeBranches` in order.
  if not n.hasMore: return
  case n.kind
  of ParLe:
    case n.stmtKind
    of ProcS:
      # `(proc name params rettype pragmas body)`. Analyse the body with fresh
      # per-proc state; NIFC has no nested procs, so no re-entrancy. Module
      # top-level reaches here through the enclosing `(stmts …)`.
      resetPerProc c
      n.into:
        while n.hasMore:
          if n.kind == ParLe and n.stmtKind in {StmtsS, ScopeS}:
            tr(c, n)             # the body
          else:
            skip n               # name / params / rettype / pragmas
    of CallS:                trCall(c, n)
    of AsgnS, StoreS:        trAsgn(c, n)
    of IfS:                  trIf(c, n)
    of CaseS:                trCase(c, n)
    of WhileS, LoopS:        trLoop(c, n)
    of JmpS:                 trJmp(c, n)
    of LabS:                 trLab(c, n)
    of BreakS, RetS, RaiseS: trBreakOrRet(c, n)
    of StmtsS, ScopeS:
      n.loopInto:
        tr(c, n)
    else:
      # Other ParLe-tagged node: walk it as an expression so any embedded
      # Symbol uses still clear their bits.
      trExpr(c, n)
  of Symbol:
    c.moved[n.symId] = false
    taintCurrent(c, n.symId)
    inc n
  else:
    inc n

# ---- public entry point ----------------------------------------------------

proc runArcopt*(buf: var TokenBuf; moduleSuffix = ""; bits = 0) =
  ## Two-phase: walk `buf` to identify elidable `=destroy(x)` (and the
  ## paired `=wasMoved(x)` calls) and record substitution patches; then
  ## rebuild `buf` with each elided call replaced by a single `DotToken`.
  ##
  ## Runs on already-generated NIFC, where `try`/`finally` has been lowered to
  ## explicit control flow — so there is no finally-specific handling to do.
  ## `moduleSuffix`/`bits` are accepted for pipeline call-site compatibility
  ## but unused: the pass works on absolute buffer positions and is
  ## width-agnostic. `tr` resets state at each `(proc …)`; a buffer that is a
  ## bare statement list (the self-tests) is analysed as a single body.
  var ctx = createContext(addr buf)
  var n = beginRead(buf)
  tr(ctx, n)
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests ------------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    var stream = nifstreams.openFromBuffer(src, "M")
    result = fromStream(stream)

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = render(buf)
    runArcopt buf
    assertRender(buf, before)

  # Seed enough symbols so that `=destroy.0.X` and `=wasMoved.0.X` are
  # interpretable. Symbol ids are interned on first reference.
  discard pool.syms.getOrIncl("=destroy.0.M")
  discard pool.syms.getOrIncl("=wasMoved.0.M")
  discard pool.syms.getOrIncl("x.0.M")
  discard pool.syms.getOrIncl("c.0.M")
  discard pool.syms.getOrIncl("use.0.M")

  block straight_line:
    var buf = parse("(stmts (call =wasMoved.0.M x.0.M) (call =destroy.0.M x.0.M))")
    runArcopt buf
    assertRender(buf, "(stmts . .)")

  block use_blocks_elision:
    # The use clears the moved bit → nothing elided.
    assertUnchanged(
      "(stmts (call =wasMoved.0.M x.0.M) (call use.0.M x.0.M) (call =destroy.0.M x.0.M))")

  block both_branches_moved:
    var buf = parse(
      "(stmts (if (elif c.0.M (call =wasMoved.0.M x.0.M)) (else (call =wasMoved.0.M x.0.M))) (call =destroy.0.M x.0.M))")
    runArcopt buf
    assertRender(buf, """
(stmts
(if
(elif c.0.M .)
(else .)).)""")

  block one_branch_uses:
    # Branch A uses x → moved becomes (true ∩ false) = false → no elision.
    assertUnchanged(
      "(stmts (call =wasMoved.0.M x.0.M) (if (elif c.0.M (call use.0.M x.0.M)) (else (stmts))) (call =destroy.0.M x.0.M))")

  block both_branches_pair_elision:
    var buf = parse(
      "(stmts (call =wasMoved.0.M x.0.M) (if (elif c.0.M (stmts)) (else (stmts))) (call =destroy.0.M x.0.M))")
    runArcopt buf
    assertRender(buf, """
(stmts .
(if
(elif c.0.M
(stmts))
(else
(stmts))).)""")

  block tainting_blocks_partial_elision:
    # Pre-if wasMove is tainted by branch A's use and stays; the
    # in-branch re-wasMove and the post-if destroy are elided.
    var buf = parse(
      "(stmts (call =wasMoved.0.M x.0.M) (if (elif c.0.M (stmts (call use.0.M x.0.M) (call =wasMoved.0.M x.0.M))) (else (stmts))) (call =destroy.0.M x.0.M))")
    runArcopt buf
    assertRender(buf, """
(stmts
(call =wasMoved.0.M x.0.M)
(if
(elif c.0.M
(stmts
(call use.0.M x.0.M).))
(else
(stmts))).)""")

  block diverging_branch_preserves_fallthrough_move:
    # `=wasMoved(x); if c: return; =destroy(x)`. The `if` has no else and the
    # only branch diverges, so it contributes nothing to the join: the move
    # established before the `if` survives to the destroy on the fall-through
    # path, and the pair is elided. (Before divergence handling the returning
    # branch folded in as a conservative fall-through and blocked this.)
    var buf = parse(
      "(stmts (call =wasMoved.0.M x.0.M) (if (elif c.0.M (stmts (ret)))) (call =destroy.0.M x.0.M))")
    runArcopt buf
    assertRender(buf, """
(stmts .
(if
(elif c.0.M
(stmts
(ret)))).)""")

  block diverging_branch_move_does_not_leak_to_join:
    # `if c: =wasMoved(x); return else: <nothing>; =destroy(x)`. The move is on
    # the diverging branch only; the else path reaches the destroy with x NOT
    # moved, so nothing may be elided.
    assertUnchanged(
      "(stmts (if (elif c.0.M (stmts (call =wasMoved.0.M x.0.M) (ret))) (else (stmts))) (call =destroy.0.M x.0.M))")

  block per_proc_reset_and_elision:
    # Two procs each with their own `x.0.M` local; the pair elides inside each,
    # and per-proc reset keeps proc 1's move from leaking into proc 2.
    discard pool.syms.getOrIncl("f.0.M")
    discard pool.syms.getOrIncl("g.0.M")
    var buf = parse(
      "(stmts " &
        "(proc f.0.M . . . (stmts (call =wasMoved.0.M x.0.M) (call =destroy.0.M x.0.M))) " &
        "(proc g.0.M . . . (stmts (call =destroy.0.M x.0.M))))")
    runArcopt buf
    # proc f: pair elided to dots. proc g: lone destroy (no preceding move,
    # and no leak from f) stays.
    assertRender(buf, """
(stmts
(proc f.0.M . . .
(stmts . .))
(proc g.0.M . . .
(stmts
(call =destroy.0.M x.0.M))))""")

  echo "arcopt.nim: all self-tests passed"

#
#
#           NIFC Copy / Constant Propagation
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Two-phase copy propagation for NIFC:
##
## 1. **Walk** — traverse the program with a `Tracker[SymId, Cursor]`
##    keeping each local's current binding (a leaf literal, a `Symbol`,
##    or a nullary `ParLe` constant such as `(true)`). At every value-
##    position use of a tracked symbol, record a **patch** keyed by the
##    use's buffer position; the patch's value is a `Cursor` pointing at
##    the source subtree to splice in.
##
## 2. **Apply** — rebuild the buffer by walking the original token by
##    token; at every position present in the patch table, splice the
##    source subtree via `addSubtree`; otherwise copy the token. This
##    allows multi-token substitutions like `(true)` that a single-token
##    in-place overwrite cannot handle.
##
## ## Control-flow awareness
##
## - `if` / `case` use the tracker's structured `closeBranches` —
##   branched bindings to the same value merge because we define a
##   content-aware `==` for `Cursor`.
## - **Loops** are modelled as a 2-sibling group (no-iteration +
##   body-iteration). A pre-scan of the body's writes and address-takings
##   clears those symbols at the start of the body sibling, so the body
##   walks under the same conservative state every iteration would see.
##   Variables never written in the body keep their pre-loop bindings —
##   *inside* the loop and after it.
## - Forward `(jmp L)` / `(lab L)` route through the tracker's label
##   merge primitives.
##
## ## Local variables vs. function calls
##
## A statement-level `(call …)` cannot mutate a local whose address has
## never been taken. We maintain a `addrTaken` set (monotonic, updated
## whenever an `(addr x)` is seen — including in pre-scans of loops) and
## a call only invalidates symbols in that set. Bindings to ordinary
## (non-addressed) locals survive calls intact.
##
## ## What is substitutable
##
## A binding's RHS is recorded as a `Cursor` and may be substituted
## later iff it is:
## - a single-token leaf (`Symbol`, `IntLit`, `UIntLit`, `FloatLit`,
##   `CharLit`, `StringLit`), or
## - a nullary `ParLe` constant: `(true)`, `(false)`, `(nil)`, `(inf)`,
##   `(neginf)`, `(nan)`.
##
## Other multi-token expressions (e.g. `(neg 5)`, `(sizeof T)`) could be
## supported with the same machinery — left as a follow-up. Calls,
## derefs and similar impure / lvalue forms are deliberately excluded.

import std / [tables, sets, hashes, assertions]
include "../../lib" / nifprelude
import nifstreams, nifcursors
import ".." / nifc_model
import ".." / ".." / models / tags        # DiscardTagId
import trackers, patchsets, nifrender

# Compare cursors by the *token value* they point at, not by buffer
# position. Defined locally so the `Tracker[SymId, Cursor]` generic in
# this module picks it up via mixin lookup.
proc `==`*(a, b: Cursor): bool {.inline.} =
  # `cursorIsNil` (not `hasMore`): these are *stored binding* cursors that may
  # be `default(Cursor)` (p == nil). `hasMore` would dereference such a cursor
  # when virtualParRi is off (no `rem > 0` short-circuit); the nil test only
  # inspects the pointer.
  let av = not cursorIsNil(a)
  let bv = not cursorIsNil(b)
  if not av and not bv: return true
  if not av or not bv: return false
  let ta = a.load
  let tb = b.load
  ta.kind == tb.kind and ta.uoperand == tb.uoperand

const NullaryParLeTags = {TrueC, FalseC, NilC, InfC, NeginfC, NanC}
const LeafLiteralKinds = {IntLit, UIntLit, FloatLit, CharLit, StringLit}

proc isSubstitutable(c: Cursor): bool =
  ## True iff the subtree rooted at `c` may be cloned into a value
  ## position safely.
  case c.kind
  of Symbol: true
  of IntLit, UIntLit, FloatLit, CharLit, StringLit: true
  of ParLe: c.exprKind in NullaryParLeTags
  else: false

type
  PendingCall = object
    ## A `(var :tmp T (call …))` whose fate is still being decided.
    ## - Consumed by its single use → fuse: rewrite that use to the call
    ##   subtree and replace the decl with a dot.
    ## - Survives the whole walk with use-count 0 → replace decl with
    ##   `(discard call)` so the side effect still runs.
    ## - Otherwise (used in a non-substitutable position, or invalidated
    ##   by an intervening event) → left untouched.
    defPos: int            ## position of the `(var :tmp …)` decl in `orig`
    callCur: Cursor        ## cursor at the call expression (for splicing)
    mentionedSyms: HashSet[SymId]
                           ## every `Symbol` referenced in the call subtree.
                           ## Used to invalidate the pending binding when
                           ## something that could touch one of those syms
                           ## happens between the bind and the consumer.

  Context = object
    orig: ptr TokenBuf
    constants: Tracker[SymId, Cursor]
    addrTaken: HashSet[SymId]
    patchset: Patchset
    useCount: Table[SymId, int]      ## Symbol-token uses per sym (pre-scanned)
    substCount: Table[SymId, int]    ## uses we actually substituted away
    delCandidates: Table[SymId, int] ## local-var sym -> its decl position
    pending: Table[SymId, PendingCall]
                                     ## live call-init local-var bindings
                                     ## awaiting their consumer (the fuse) or
                                     ## an invalidating event.
    dotBuf: TokenBuf                 ## a single DotToken: replaces a deleted decl
    synth: seq[TokenBuf]             ## stable storage for `(discard call)` synths

proc createContext(orig: ptr TokenBuf): Context =
  result = Context(orig: orig,
          constants: initTracker[SymId, Cursor](),
          addrTaken: initHashSet[SymId](),
          patchset: initPatchset(orig),
          useCount: initTable[SymId, int](),
          substCount: initTable[SymId, int](),
          delCandidates: initTable[SymId, int](),
          pending: initTable[SymId, PendingCall](),
          dotBuf: createTokenBuf(2),
          synth: @[])
  result.dotBuf.addDotToken()

# ---- subtree inspection ---------------------------------------------------

proc bindingReferences(cur: Cursor; target: SymId): bool =
  ## Does the subtree rooted at `cur` mention `Symbol target`?
  if not cur.hasMore: return false
  case cur.kind
  of Symbol:
    return cur.symId == target
  of ParLe:
    var n = cur
    var found = false
    n.loopInto:
      if not found and bindingReferences(n, target):
        found = true
      skip n
    return found
  else:
    return false

proc collectSyms(cur: Cursor; outSyms: var HashSet[SymId]) =
  ## Add every `Symbol` token in the subtree rooted at `cur` to `outSyms`.
  ## (`SymbolDef` is intentionally excluded — it's a definition, not a read.)
  if not cur.hasMore: return
  case cur.kind
  of Symbol:
    outSyms.incl cur.symId
  of ParLe:
    var n = cur
    n.loopInto:
      collectSyms(n, outSyms)
      skip n
  else: discard

proc firstSymbolIn(c: Cursor): SymId =
  ## The first `Symbol` token in the subtree rooted at `c`, or
  ## `SymId(0)` if there is none.
  if not c.hasMore: return SymId(0)
  case c.kind
  of Symbol:
    return c.symId
  of ParLe:
    var n = c
    var found = SymId(0)
    n.loopInto:
      if found == SymId(0):
        let inner = firstSymbolIn(n)
        if inner != SymId(0): found = inner
      skip n
    return found
  else:
    return SymId(0)

proc preScanWrites(start: Cursor; writes, addrs: var HashSet[SymId]) =
  ## Walk the subtree at `start`. Add every `Symbol` LHS of an `asgn` /
  ## `store` to `writes`. Add the first `Symbol` inside every `(addr …)`
  ## to `addrs`. Recurses into nested subtrees.
  if not start.hasMore: return
  if start.kind != ParLe: return
  if start.stmtKind in {AsgnS, StoreS}:
    let lhs = start.firstSon
    if lhs.kind == Symbol:
      writes.incl lhs.symId
  if start.exprKind == AddrC:
    let s = firstSymbolIn(start.firstSon)
    if s != SymId(0): addrs.incl s
  var n = start
  n.loopInto:
    preScanWrites(n, writes, addrs)
    skip n

# ---- tracker maintenance --------------------------------------------------

proc invalidateReferencing(c: var Context; target: SymId) =
  ## Clear every tracker entry whose binding subtree references `target`.
  var toClear: seq[SymId] = @[]
  for k, cur in c.constants.pairs:
    if bindingReferences(cur, target):
      toClear.add k
  for k in toClear:
    c.constants[k] = default(Cursor)

proc invalidateAddrTaken(c: var Context) =
  ## Drop bindings for every symbol whose address has been taken — a
  ## call or write through a pointer could have modified any of them.
  var toClear: seq[SymId] = @[]
  for s in c.addrTaken:
    if not cursorIsNil(c.constants[s]): toClear.add s
  for s in toClear:
    c.constants[s] = default(Cursor)
  for s in c.addrTaken:
    invalidateReferencing(c, s)

proc invalidatePendingForSym(c: var Context; s: SymId) =
  ## Drop any pending call-binding whose tmp is `s` or whose call mentions
  ## `s`. Called when something writes `s` or takes its address — either
  ## could change what the pending call would now observe / produce.
  var toDel: seq[SymId] = @[]
  for tmp, info in c.pending:
    if tmp == s or s in info.mentionedSyms:
      toDel.add tmp
  for tmp in toDel:
    c.pending.del tmp

proc clearPending(c: var Context) =
  ## Drop all pending bindings — used when control flow leaves straight-line
  ## execution (branch entry, loop, jmp/lab, break/ret). A pending binding
  ## whose consumer is on a different control-flow path would reorder the
  ## call's side effect with respect to the divergence.
  c.pending.clear()

proc markAddrTaken(c: var Context; s: SymId) =
  c.addrTaken.incl s
  c.constants[s] = default(Cursor)
  invalidateReferencing(c, s)
  invalidatePendingForSym(c, s)

proc effectiveBinding(c: Context; rhs: Cursor): Cursor =
  ## What should a new binding to `rhs` actually store? Chains through
  ## an existing binding when `rhs` is a tracked `Symbol`, otherwise
  ## returns `rhs` itself when substitutable, otherwise `default(Cursor)`.
  if rhs.kind == Symbol:
    let prior = c.constants[rhs.symId]
    if not cursorIsNil(prior): return prior
    return rhs
  if isSubstitutable(rhs):
    return rhs
  return default(Cursor)

# ---- branch-state forwarding ----------------------------------------------

proc openBranches(c: var Context) =
  # Entering a branch group means the next code we walk might or might not
  # run; carrying a pending call-binding across it would reorder the call's
  # side effect relative to whichever side of the divergence wins. Drop
  # them on entry.
  c.constants.openBranches()
  clearPending c
proc openBranch(c: var Context) = c.constants.openBranch()
proc openFinalBranch(c: var Context) = c.constants.openFinalBranch()
proc closeBranch(c: var Context) = c.constants.closeBranch()
proc closeBranches(c: var Context) = c.constants.closeBranches()
proc gotoLabel(c: var Context; L: LabelId) =
  c.constants.gotoLabel L
  clearPending c
proc landLabel(c: var Context; L: LabelId) =
  c.constants.landLabel L
  clearPending c
proc clearAll(c: var Context) =
  c.constants.clearAll()
  clearPending c

# ---- patch recording (phase 1 walk) ---------------------------------------

proc countUses(c: Cursor; useCount: var Table[SymId, int]) =
  ## Count every `Symbol` (use) token in the single tree at `c`. `SymbolDef`
  ## (the definition site) is intentionally excluded. Mode-agnostic: walks
  ## via `loopInto`/`skip`, so it works with or without `-d:virtualParRi`.
  case c.kind
  of Symbol:
    useCount.mgetOrPut(c.symId, 0) += 1
  of ParLe:
    var n = c
    n.loopInto:
      countUses(n, useCount)
      skip n
  else: discard

proc recordSubstitution(c: var Context; useCur: Cursor; binding: Cursor) =
  ## Record that the token at `useCur`'s position should be replaced
  ## with the subtree at `binding` during the apply phase.
  let usePos = cursorToPosition(c.orig[], useCur)
  c.patchset.addSubst(usePos, binding)
  if useCur.kind == Symbol:
    c.substCount.mgetOrPut(useCur.symId, 0) += 1

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward

proc trExpr(c: var Context; n: var Cursor) =
  case n.kind
  of Symbol:
    let s = n.symId
    let binding = c.constants[s]
    if not cursorIsNil(binding) and isSubstitutable(binding):
      recordSubstitution(c, n, binding)
    inc n
  of ParLe:
    case n.exprKind
    of AddrC:
      let lvalue = n.firstSon
      let s = firstSymbolIn(lvalue)
      if s != SymId(0):
        markAddrTaken(c, s)
      skip n
    of DerefC:
      # Operand is a pointer expression — recurse to substitute symbols
      # inside it.
      n.into:
        if n.hasMore: trExpr(c, n)
        while n.hasMore: skip n
    of AtC:
      # `(at arr idx)` — arr is an lvalue (skip), idx is a value.
      n.into:
        if n.hasMore: skip n            # arr
        if n.hasMore: trExpr(c, n)      # idx
        while n.hasMore: skip n
    of DotC:
      # `(dot obj field …)` — obj is an lvalue; field/extras are
      # declarations, never substitutable here. Skip the whole thing.
      skip n
    of CallC:
      # A `(call …)` in expression position can only touch locals whose
      # address has been taken — and we maintain the invariant that no
      # pending entry references such a sym (see `trVar`), so it can't
      # invalidate a pending binding. Only the live-binding tracker needs
      # the addrTaken sweep.
      n.into:
        if n.hasMore: skip n            # callee — leave as-is
        while n.hasMore: trExpr(c, n)   # args
      invalidateAddrTaken c
    else:
      n.loopInto:
        trExpr(c, n)
  else:
    inc n

proc trVar(c: var Context; n: var Cursor) =
  let kind = n.stmtKind
  let defPos = cursorToPosition(c.orig[], n)
  let d = takeVarDecl(n)
  # Call-init local with a substitution-safe call expression: register as
  # pending and SKIP walking the call's contents. Reason: the call subtree
  # may later be spliced verbatim (fuse) or wrapped in `(discard …)`
  # (unused), and `addSubtree` does not re-apply patches to the source.
  # Patches recorded inside the call would silently drop, but `substCount`
  # would still have been incremented — and that wrongly trips dead-decl
  # deletion for syms whose only "substituted" use was inside the moved
  # call. Skipping the walk costs intra-call substitutions in the rarer
  # survives-as-is case; the fuse/discard cases stay correct.
  if kind == VarS and d.name.kind == SymbolDef and
     d.value.kind == ParLe and d.value.exprKind == CallC:
    var mentioned = initHashSet[SymId]()
    collectSyms(d.value, mentioned)
    # Invariant: pending entries reference no address-taken sym, so
    # subsequent calls / writes through pointers can't change what the
    # pending call would observe — eliminates O(pending) per-call
    # invalidation passes.
    var safe = true
    for sm in mentioned:
      if sm in c.addrTaken:
        safe = false
        break
    if safe:
      let s = d.name.symId
      c.pending[s] = PendingCall(defPos: defPos, callCur: d.value,
                                 mentionedSyms: mentioned)
      c.constants[s] = default(Cursor)
      return
  if d.value.kind != DotToken:
    var v = d.value
    trExpr(c, v)
  if d.name.kind == SymbolDef:
    let s = d.name.symId
    if d.value.kind != DotToken:
      let binding = effectiveBinding(c, d.value)
      c.constants[s] = binding
      # Dead-binding elimination: a *local* `var` bound to a pure, substitutable
      # value is a deletion candidate. If, after the walk, every use of it was
      # substituted (subst == total occurrences), the decl is dead. Restricted
      # to `VarS` — globals/threadvars/consts may be referenced externally.
      if kind == VarS and not cursorIsNil(binding):
        c.delCandidates[s] = defPos
    else:
      c.constants[s] = default(Cursor)

proc trAsgn(c: var Context; n: var Cursor) =
  n.into:
    let lhsStart = n
    skip n
    let rhsStart = n
    skip n
    while n.hasMore: skip n

  # `(asgn lhs tmp)` where `tmp` is a pending call-init local with use-count
  # 1 fuses to `(asgn lhs (call …))` — same call, no temp. Recording the
  # `lhs → tmp` alias in the tracker for this shape is the original copy-prop
  # bug: `tmp` is short-lived (scoped to the bind site), and propagating it
  # into later `lhs` uses leaked an inner-scope symbol past its scope. With
  # the fuse there is no `tmp` in the output.
  let canFuse = lhsStart.kind == Symbol and rhsStart.kind == Symbol and
                rhsStart.symId in c.pending and
                c.useCount.getOrDefault(rhsStart.symId) == 1
  if canFuse:
    let tmp = rhsStart.symId
    let info = c.pending[tmp]
    # Fuse: rewrite this asgn's RHS Symbol token to the call subtree, and
    # replace the original `(var :tmp …)` decl with a dot.
    c.patchset.addSubst(cursorToPosition(c.orig[], rhsStart), info.callCur)
    c.patchset.addSubst(info.defPos, cursorAt(c.dotBuf, 0))
    c.substCount.mgetOrPut(tmp, 0) += 1
    c.pending.del tmp
    let s = lhsStart.symId
    invalidateReferencing(c, s)
    c.constants[s] = default(Cursor)         # asgn now reads a call result
    invalidatePendingForSym(c, s)            # asgn writes to s
  else:
    var r = rhsStart
    trExpr(c, r)
    if lhsStart.kind == Symbol:
      let s = lhsStart.symId
      invalidateReferencing(c, s)
      c.constants[s] = effectiveBinding(c, rhsStart)
      invalidatePendingForSym(c, s)
    else:
      # Complex LHS (`(at …)`, `(deref …)`, `(dot …)`). Walk it so any
      # value sub-positions (e.g. an array index) get substituted.
      var l = lhsStart
      if lhsStart.kind == ParLe:
        trExpr(c, l)
      # Conservatively assume the complex write may go through a pointer
      # to any address-taken local.
      invalidateAddrTaken c
      # Extract the LHS's root local and drop pending entries that mention
      # it. For `(deref p)` this is `p` — slightly overkill, but harmless:
      # by the pending invariant no entry mentions an addr-taken sym, so
      # the deref write cannot affect any pending call's observed value.
      let root = firstSymbolIn(lhsStart)
      if root != SymId(0):
        invalidatePendingForSym(c, root)

proc trCallStmt(c: var Context; n: var Cursor) =
  n.into:
    if n.hasMore: skip n              # callee
    while n.hasMore: trExpr(c, n)
  invalidateAddrTaken c
  # No pending invalidation: by the pending invariant (no mentioned sym is
  # address-taken), a call cannot touch what any pending call would read.

proc trIf(c: var Context; n: var Cursor) =
  openBranches c
  n.loopInto:
    case n.substructureKind
    of ElifU:
      n.into:
        if n.hasMore: trExpr(c, n)    # condition
        openBranch c
        if n.hasMore: tr(c, n)        # body
        while n.hasMore: skip n
        closeBranch c
    of ElseU:
      n.into:
        openFinalBranch c             # else makes the group exhaustive
        if n.hasMore: tr(c, n)
        while n.hasMore: skip n
        closeBranch c
    else:
      skip n
  closeBranches c

proc trCase(c: var Context; n: var Cursor) =
  n.into:
    if n.hasMore: trExpr(c, n)        # selector
    openBranches c
    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          if n.hasMore: skip n        # ranges
          openBranch c
          if n.hasMore: tr(c, n)
          while n.hasMore: skip n
          closeBranch c
      of ElseU:
        n.into:
          openFinalBranch c
          if n.hasMore: tr(c, n)
          while n.hasMore: skip n
          closeBranch c
      else:
        skip n
    closeBranches c

proc trLoopBody(c: var Context; n: var Cursor) =
  ## Walks `(while cond body)` or `(loop pre cond body after)` children.
  case n.stmtKind
  of WhileS:
    n.into:
      if n.hasMore: trExpr(c, n)      # condition
      if n.hasMore: tr(c, n)          # body
      while n.hasMore: skip n
  of LoopS:
    n.into:
      if n.hasMore: tr(c, n)          # before-cond
      if n.hasMore: trExpr(c, n)      # cond
      if n.hasMore: tr(c, n)          # body
      if n.hasMore: tr(c, n)          # after
      while n.hasMore: skip n
  else:
    skip n

proc trLoop(c: var Context; n: var Cursor) =
  # Pre-scan to find Symbol writes and addr-takings inside the loop.
  var writes = initHashSet[SymId]()
  var newAddrs = initHashSet[SymId]()
  preScanWrites(n, writes, newAddrs)
  for s in newAddrs: markAddrTaken(c, s)

  openBranches c

  # Sibling 1: loop body does not execute. No state changes.
  openBranch c
  closeBranch c

  # Sibling 2: loop body executes (at least once). Clear writes and any
  # address-taken locals (calls/pointer-stores in the body may mutate
  # them) before walking — so substitutions inside the body see the same
  # conservative state every iteration would.
  openBranch c
  for s in writes:
    c.constants[s] = default(Cursor)
    invalidateReferencing(c, s)
  invalidateAddrTaken c
  trLoopBody(c, n)
  closeBranch c

  closeBranches c

proc trJmp(c: var Context; n: var Cursor) =
  var probe = n
  inc probe
  if probe.kind == Symbol:
    gotoLabel(c, LabelId(probe.symId.uint32))
  skip n

proc trLab(c: var Context; n: var Cursor) =
  var probe = n
  inc probe
  if probe.kind == SymbolDef:
    landLabel(c, LabelId(probe.symId.uint32))
  skip n

proc trBreakOrRet(c: var Context; n: var Cursor) =
  skip n
  clearAll c

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of ParLe:
    case n.stmtKind
    of VarS, GvarS, TvarS, ConstS: trVar(c, n)
    of AsgnS, StoreS:              trAsgn(c, n)
    of CallS:                      trCallStmt(c, n)
    of IfS:                        trIf(c, n)
    of CaseS:                      trCase(c, n)
    of WhileS, LoopS:              trLoop(c, n)
    of JmpS:                       trJmp(c, n)
    of LabS:                       trLab(c, n)
    of BreakS, RetS, RaiseS:       trBreakOrRet(c, n)
    of StmtsS, ScopeS:
      n.loopInto:
        tr(c, n)
    else:
      trExpr(c, n)
  of Symbol:
    trExpr(c, n)
  else:
    inc n

# ---- public entry point --------------------------------------------------

proc runCopyPropagation*(buf: var TokenBuf) =
  ## Two-phase in-place copy/constant propagation. After this call,
  ## `buf` has every redundant `Symbol` use replaced by its bound
  ## leaf-literal / `Symbol` / nullary-constant subtree.
  var ctx = createContext(addr buf)
  countUses(beginRead(buf), ctx.useCount)
  var n = beginRead(buf)
  tr(ctx, n)
  # Delete every candidate decl whose uses were *all* propagated away. A decl
  # with a surviving non-substituted occurrence (reassignment, `addr`, a
  # `dot`/`at` base, or a read past an invalidation) keeps `subst < total`.
  for s, defPos in ctx.delCandidates:
    if ctx.substCount.getOrDefault(s) == ctx.useCount.getOrDefault(s):
      ctx.patchset.addSubst(defPos, cursorAt(ctx.dotBuf, 0))
  # Unconsumed pending call-init bindings. Those still in `ctx.pending` at
  # this point survived the walk without being consumed (no single-read
  # fuse fired) and without invalidation. With `use-count == 0` (truly
  # unused) we drop the binding but keep the call as `(discard call)`;
  # otherwise we leave the var decl as-is (the use was non-substitutable —
  # `addr`, an `asgn` LHS, a `dot`/`at` base — so removing the local would
  # break that reference). Pre-size `synth` so growth can't invalidate the
  # cursors the patchset stores.
  var discardCount = 0
  for s, _ in ctx.pending:
    if ctx.useCount.getOrDefault(s) == 0: inc discardCount
  if discardCount > 0:
    ctx.synth = newSeqOfCap[TokenBuf](discardCount)
    for s, info in ctx.pending:
      if ctx.useCount.getOrDefault(s) == 0:
        var buf2 = createTokenBuf(16)
        buf2.addParLe TagId(ord(DiscardTagId)), info.callCur.info
        var cc = info.callCur
        buf2.addSubtree cc
        buf2.addParRi()
        ctx.synth.add buf2
        ctx.patchset.addSubst(info.defPos, cursorAt(ctx.synth[^1], 0))
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    var stream = nifstreams.openFromBuffer(src, "M")
    result = fromStream(stream)

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = render(buf)
    runCopyPropagation buf
    assertRender(buf, before)

  discard pool.syms.getOrIncl("x.0.M")
  discard pool.syms.getOrIncl("y.0.M")
  discard pool.syms.getOrIncl("a.0.M")
  discard pool.syms.getOrIncl("b.0.M")
  discard pool.syms.getOrIncl("c.0.M")
  discard pool.syms.getOrIncl("use.0.M")
  discard pool.syms.getOrIncl("side.0.M")

  block simple_literal_propagation:
    # x's single use is propagated, so its (now dead) decl is deleted.
    var buf = parse(
      "(stmts (var :x.0.M . (i 32) 5) (call use.0.M x.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts .
(call use.0.M 5))""")

  block reassignment_clears:
    assertUnchanged(
      "(stmts (var :x.0.M . (i 32) 5) (asgn x.0.M (call side.0.M)) (call use.0.M x.0.M))")

  block branch_agreement:
    var buf = parse(
      "(stmts (var :x.0.M . (i 32) 5) (if (elif c.0.M (asgn x.0.M 5)) (else (asgn x.0.M 5))) (call use.0.M x.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts
(var :x.0.M .
(i 32)5)
(if
(elif c.0.M
(asgn x.0.M 5))
(else
(asgn x.0.M 5)))
(call use.0.M 5))""")

  block branch_disagreement:
    assertUnchanged(
      "(stmts (var :x.0.M . (i 32) 5) (if (elif c.0.M (asgn x.0.M 6)) (else (asgn x.0.M 7))) (call use.0.M x.0.M))")

  block propagation_into_expression:
    var buf = parse(
      "(stmts (var :x.0.M . (i 32) 5) (call use.0.M (add (i 32) x.0.M x.0.M)))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts .
(call use.0.M
(add
(i 32)5 5)))""")

  block chained_propagation:
    # a → 5 and b → a → 5; both decls become dead and are deleted.
    var buf = parse(
      "(stmts (var :a.0.M . (i 32) 5) (var :b.0.M . (i 32) a.0.M) (call use.0.M b.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts . .
(call use.0.M 5))""")

  block symbol_to_symbol_propagation:
    # a keeps its (impure) call binding; b aliases a and is deleted, its use
    # rewritten to a.
    var buf = parse(
      "(stmts (var :a.0.M . (i 32) (call side.0.M)) (var :b.0.M . (i 32) a.0.M) (call use.0.M b.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts
(var :a.0.M .
(i 32)
(call side.0.M)).
(call use.0.M a.0.M))""")

  block symbol_propagation_invalidated:
    assertUnchanged(
      "(stmts (var :a.0.M . (i 32) (call side.0.M)) (var :b.0.M . (i 32) a.0.M) (asgn a.0.M 9) (call use.0.M b.0.M))")

  block true_literal_propagation:
    var buf = parse(
      "(stmts (var :x.0.M . (bool) (true)) (call use.0.M x.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts .
(call use.0.M
(true)))""")

  block nil_literal_propagation:
    var buf = parse(
      "(stmts (var :x.0.M . (ptr (i 32)) (nil)) (call use.0.M x.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts .
(call use.0.M
(nil)))""")

  block call_does_not_clear_unaddressed:
    # x's address is never taken → call cannot mutate it; binding survives the
    # call, the use is propagated, and the dead decl is deleted.
    var buf = parse(
      "(stmts (var :x.0.M . (i 32) 5) (call side.0.M) (call use.0.M x.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts .
(call side.0.M)
(call use.0.M 5))""")

  block call_clears_addressed:
    assertUnchanged(
      "(stmts (var :x.0.M . (i 32) 5) (call side.0.M (addr x.0.M)) (call use.0.M x.0.M))")

  block loop_preserves_unwritten_binding:
    var buf = parse(
      "(stmts (var :x.0.M . (i 32) 5) (while c.0.M (call use.0.M x.0.M)) (call use.0.M x.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts .
(while c.0.M
(call use.0.M 5))
(call use.0.M 5))""")

  block loop_invalidates_written_binding:
    assertUnchanged(
      "(stmts (var :x.0.M . (i 32) 5) (while c.0.M (asgn x.0.M 9)) (call use.0.M x.0.M))")

  block loop_only_invalidates_written_one:
    # y survives the loop (no write to y) → propagated and its dead decl
    # deleted; x is written inside the loop, so its decl and use both stay.
    var buf = parse(
      "(stmts (var :x.0.M . (i 32) 5) (var :y.0.M . (i 32) 9) (while c.0.M (asgn x.0.M 0)) (call use.0.M y.0.M x.0.M))")
    runCopyPropagation buf
    assertRender(buf, """
(stmts
(var :x.0.M .
(i 32)5).
(while c.0.M
(asgn x.0.M 0))
(call use.0.M 9 x.0.M))""")

  echo "copy_propagation.nim: all self-tests passed"

#
#
#           NIFC Address CSE
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Two-phase common-subexpression elimination for **address-yielding**
## memory chains in NIFC.
##
## The traditional "value CSE" of `(x.field[i].fieldB)` is fragile: every
## intervening call or write through an aliased pointer can change the
## value at that location, forcing us to recompute. Caching the
## **address** instead is robust — the address of `x.field[i]` is a
## function only of `x`'s storage location and `i`'s value, neither of
## which a foreign call can change unless their addresses have escaped.
##
## So we rewrite:
##
## ```
## y = x.field[i].fieldB
## effect()
## z = x.field[i].fieldB + 1
## ```
##
## as:
##
## ```
## var t = addr(x.field[i].fieldB)
## y = t[]
## effect()
## z = t[] + 1
## ```
##
## ## Pattern (matches arcopt / copy_propagation)
##
## - **Phase 1 walk** — records two kinds of patches keyed by buffer
##   position: `pkInsert` (splice a synthesized subtree *before* the
##   token at the position) and `pkSubst` (replace the subtree at the
##   position with a synthesized one).
## - **Phase 2 apply** — rebuilds the buffer; at each patched position
##   emit inserts first, then either the substitution (and skip the
##   original) or fall through to copy the original normally.
##
## ## Candidates
##
## A subtree is a CSE candidate iff it is one of:
## - `(dot obj field …)` — field selection,
## - `(at arr idx)` — array indexing,
## - `(deref ptr)` where `ptr` is itself a non-trivial subtree.
##
## ## Invalidation
##
## A cache entry stores the original expression's buffer position. On
## every cache-mutating event we walk all live entries and clear those
## whose expression mentions the affected `Symbol`:
##
## - `(asgn s …)` where `s` is a `Symbol` LHS: drop entries that mention
##   `s`. Other assignment shapes (`(asgn (at …) …)`, `(asgn (deref …)
##   …)`) modify memory through a pointer, not the symbol's value, so
##   they only invalidate cache entries that mention an addressed local
##   (treated the same as a call below).
## - `(addr s)`: drop entries that mention `s`, then add `s` to
##   `addrTaken` (monotonic — once taken, stays).
## - `(call …)`: drop entries that mention any symbol in `addrTaken`.
##   Bindings whose expression touches *only* unaddressed locals are
##   preserved across the call — the whole point of address CSE.
## - Loops: pre-scan body for writes and `addr`-takings; the body
##   sibling of the 2-sibling pattern enters with the cache cleared.
##
## ## Branch handling
##
## `if` / `case` use the tracker's `closeBranches` (intersection on a
## `(tempSym, exprPos)` pair — so entries cached pre-branch with both
## branches leaving them untouched survive). A first occurrence in one
## branch and a second in another do not merge — this MVP lazily but
## eagerly generates the temp at first occurrence (so the temp's
## storage and the `(deref t)` substitutions are visible in subsequent
## walks).

import std / [tables, sets, hashes, assertions, strutils, formatfloat]
include "../../lib" / nifprelude
import nifstreams, nifcursors
import ".." / nifc_model
from ".." / ".." / hexer / funcsummary import FunctionSummary,
  FunctionSummaryTable, paramDirectEscapes, paramMayWrite
import ".." / ".." / models / tags
import trackers, patchsets_legacy, nifrender

# ---- candidate predicate --------------------------------------------------

proc loadsAPointer(c: Cursor): bool =
  ## Walk an lvalue access chain down to the first pointer indirection and
  ## judge whether *computing its address* requires loading a pointer from
  ## memory. That is the only case in which caching the address avoids real,
  ## repeated work.
  ##
  ## - A chain rooted at a local/global symbol (`myLocal.a.b.arr[4]`) is just a
  ##   constant/scaled offset from a known slot — folded into the addressing
  ##   mode for free; caching it would only add a pointer + an indirection.
  ## - A single deref of a *local pointer* (`(*p).field`) doesn't pay off
  ##   either: the address is `p + offset`, where `p` is already a value, so
  ##   the one load happens either way and the offset is free — caching just
  ##   spends another local.
  ## - But when the dereferenced pointer is itself a memory access — `(*(x.pf))`,
  ##   `(*((*p).next))`, `arr[i]` reached through such a pointer — computing the
  ##   address re-loads that pointer every time, so caching it is a real win.
  var n = c
  while n.kind == ParLe:
    case n.exprKind
    of DerefC, PatC:
      # The pointer being dereferenced. Worth caching only if it is itself a
      # non-trivial access (a load), not a plain local pointer variable.
      return n.firstSon.kind == ParLe
    of DotC, AtC: n = n.firstSon   # walk to the base
    else: return false             # call result, conv, … — leave it alone
  return false                     # Symbol / literal root → trivial offset

proc isCSECandidate(c: Cursor): bool =
  if c.kind != ParLe: return false
  case c.exprKind
  of DotC, AtC, DerefC, PatC: loadsAPointer(c)
  else: false

# ---- context --------------------------------------------------------------

type
  CachedEntry = object
    tempSym: SymId     # the address-temp; default SymId(0) means "no entry"
    exprPos: int       # position in `orig` of the cached expression
                       # (zero is a safe sentinel — position 0 is the outer
                       # `(stmts …)` ParLe, never a CSE candidate)

  Context = object
    orig: ptr TokenBuf
    cache: Tracker[string, CachedEntry]
    addrTaken: HashSet[SymId]
    patchset: Patchset
    synth: seq[TokenBuf]      ## synthesized subtrees the patchset cursors point into
    stmtStack: seq[int]       ## buffer positions of the enclosing hoist anchor
    tempCounter: int
    moduleSuffix: string
    summaries: ptr FunctionSummaryTable

proc createContext(orig: ptr TokenBuf; moduleSuffix: string;
                   summaries: ptr FunctionSummaryTable): Context =
  Context(orig: orig,
          cache: initTracker[string, CachedEntry](),
          addrTaken: initHashSet[SymId](),
          patchset: initPatchset(orig),
          synth: @[],
          stmtStack: @[],
          tempCounter: 0,
          moduleSuffix: moduleSuffix,
          summaries: summaries)

proc currentStmtPos(c: Context): int {.inline.} =
  if c.stmtStack.len > 0: c.stmtStack[^1] else: -1

proc freshTempSym(c: var Context): SymId =
  inc c.tempCounter
  result = pool.syms.getOrIncl("cse.t." & $c.tempCounter & "." & c.moduleSuffix)

# ---- expression hashing ---------------------------------------------------

proc hashExpr(c: Cursor; result: var string) =
  case c.kind
  of Symbol:
    result.add 'S'
    result.addInt c.symId.int
  of IntLit:
    result.add 'I'
    result.addInt pool.integers[c.intId]
  of UIntLit:
    result.add 'U'
    result.add $pool.uintegers[c.uintId]
  of FloatLit:
    result.add 'F'
    result.add $pool.floats[c.floatId]
  of CharLit:
    result.add 'C'
    result.addInt int c.uoperand
  of StringLit:
    result.add '"'
    result.add pool.strings[c.litId]
    result.add '"'
  of Ident:
    result.add 'i'
    result.add pool.strings[c.litId]
  of ParLe:
    result.add '('
    result.addInt int c.uoperand
    var n = c
    n.loopInto:
      result.add ' '
      hashExpr(n, result)
      skip n
    result.add ')'
  else:
    result.add '?'

proc hashExpr(c: Cursor): string =
  result = ""
  hashExpr(c, result)

# ---- subtree inspection ---------------------------------------------------

proc expressionMentions(cur: Cursor; target: SymId): bool =
  if not cur.hasMore: return false
  case cur.kind
  of Symbol:
    return cur.symId == target
  of ParLe:
    var n = cur
    var found = false
    n.loopInto:
      if not found and expressionMentions(n, target):
        found = true
      skip n
    return found
  else:
    return false

proc expressionMentionsAny(cur: Cursor; targets: HashSet[SymId]): bool =
  if not cur.hasMore: return false
  case cur.kind
  of Symbol:
    return cur.symId in targets
  of ParLe:
    var n = cur
    var found = false
    n.loopInto:
      if not found and expressionMentionsAny(n, targets):
        found = true
      skip n
    return found
  else:
    return false

proc firstSymbolIn(c: Cursor): SymId =
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

# ---- cache invalidation ---------------------------------------------------

proc markAddrTaken(c: var Context; s: SymId)
proc clearCache(c: var Context)

proc invalidateMentioning(c: var Context; target: SymId) =
  var toClear: seq[string] = @[]
  for key, entry in c.cache.pairs:
    if entry.tempSym == SymId(0): continue
    let exprCur = cursorAt(c.orig[], entry.exprPos)
    if expressionMentions(exprCur, target):
      toClear.add key
  for key in toClear:
    c.cache[key] = default(CachedEntry)

proc invalidateForCall(c: var Context) =
  if c.addrTaken.len == 0: return
  var toClear: seq[string] = @[]
  for key, entry in c.cache.pairs:
    if entry.tempSym == SymId(0): continue
    let exprCur = cursorAt(c.orig[], entry.exprPos)
    if expressionMentionsAny(exprCur, c.addrTaken):
      toClear.add key
  for key in toClear:
    c.cache[key] = default(CachedEntry)

proc callSummary(c: Context; call: Cursor; summary: var FunctionSummary): bool =
  if c.summaries == nil: return false
  if call.kind != ParLe: return false
  let callee = call.firstSon
  if callee.kind != Symbol: return false
  if not c.summaries[].hasKey(callee.symId): return false
  summary = c.summaries[].getOrQuit(callee.symId)
  result = true

proc invalidateForCall(c: var Context; call: Cursor) =
  ## Every cached entry is a `loadsAPointer` chain: its address is a function
  ## of one or more pointer values held in memory. A callee that writes to
  ## memory can change any of those pointers, and without alias analysis we
  ## cannot prove the write misses a cached chain. So the cache survives a
  ## call ONLY when the callee is provably write-free; otherwise we drop it.
  ## (The earlier `s in c.addrTaken` gate was unsound — a callee writes
  ## through a *pointer argument* regardless of whether the caller took the
  ## address of anything, e.g. `setLen(s)` reallocating `s`'s buffer leaves a
  ## cached `addr(s.data[i])` dangling.)
  var summary = FunctionSummary()
  if not callSummary(c, call, summary):
    clearCache c               # unknown callee: assume it writes memory
    return

  if summary.writesGlobal or summary.callsUnknown:
    clearCache c
    return

  # Mark escaping pointer args as addr-taken (kept for symbol-level reasoning
  # by `invalidateMentioning` elsewhere), and detect any write-through-pointer.
  var args = call.firstSon
  skip args
  var argIndex = 0
  var mayWrite = false
  while args.kind != ParRi:
    let s = firstSymbolIn(args)
    if s != SymId(0) and paramDirectEscapes(summary, argIndex):
      markAddrTaken(c, s)
    if paramMayWrite(summary, argIndex):
      mayWrite = true
    skip args
    inc argIndex

  if mayWrite:
    clearCache c

# ---- synthesis ------------------------------------------------------------

proc addVarDecl(c: var Context; tempSym: SymId; expr: Cursor;
                info: PackedLineInfo): int =
  ## Synthesize `(var :tempSym . . (addr <expr>))` into a new entry in
  ## `c.synth`. Returns the index.
  result = c.synth.len
  var buf = createTokenBuf(16)
  buf.addParLe(TagId(ord(VarTagId)), info)
  buf.addSymDef tempSym, info
  buf.addDotToken()           # pragmas
  buf.addDotToken()           # type — inferred from initializer (allowed by NIFC)
  buf.addParLe(TagId(ord(AddrTagId)), info)
  buf.addSubtree expr
  buf.addParRi()              # close addr
  buf.addParRi()              # close var
  c.synth.add buf

proc addDeref(c: var Context; tempSym: SymId; info: PackedLineInfo): int =
  ## Synthesize `(deref tempSym)` into a new entry in `c.synth`.
  result = c.synth.len
  var buf = createTokenBuf(4)
  buf.addParLe(TagId(ord(DerefTagId)), info)
  buf.addSymUse tempSym, info
  buf.addParRi()
  c.synth.add buf

proc synthCursor(c: var Context; idx: int): Cursor {.inline.} =
  cursorAt(c.synth[idx], 0)

proc addInsertPatch(c: var Context; pos: int; synthIdx: int) =
  c.patchset.addInsert(pos, synthCursor(c, synthIdx))

proc addSubstPatch(c: var Context; pos: int; synthIdx: int) =
  c.patchset.addSubst(pos, synthCursor(c, synthIdx))

# ---- branch-state forwarding ----------------------------------------------

proc openBranches(c: var Context) = c.cache.openBranches()
proc openBranch(c: var Context) = c.cache.openBranch()
proc openFinalBranch(c: var Context) = c.cache.openFinalBranch()
proc closeBranch(c: var Context) = c.cache.closeBranch()
proc closeBranches(c: var Context) = c.cache.closeBranches()
proc gotoLabel(c: var Context; L: LabelId) = c.cache.gotoLabel L
proc landLabel(c: var Context; L: LabelId) = c.cache.landLabel L
proc clearCache(c: var Context) = c.cache.clearAll()

proc markAddrTaken(c: var Context; s: SymId) =
  if s in c.addrTaken: return
  c.addrTaken.incl s
  invalidateMentioning(c, s)

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward

proc handleCandidate(c: var Context; n: Cursor) =
  ## `n` points at an `isCSECandidate` ParLe. Either reuse a cached
  ## temp (just add a subst patch) or — first time we see this hash —
  ## generate one, hoist a var decl before the enclosing statement, and
  ## substitute this occurrence as well.
  let key = hashExpr(n)
  let exprPos = cursorToPosition(c.orig[], n)
  let info = n.info
  var entry = c.cache[key]
  if entry.tempSym == SymId(0):
    # First occurrence: hoist a var decl before the enclosing statement.
    let stmtPos = c.currentStmtPos
    if stmtPos < 0:
      # No hoist anchor — can't introduce a temp; skip.
      return
    # If the hoist anchor is a loop statement, this candidate lives in the
    # loop's *condition* (the body's own statements would be the anchor
    # instead). Hoisting the address before the loop computes it ONCE, but the
    # condition re-evaluates every iteration — so a body that mutates the
    # underlying pointer (e.g. `while c.scope.up != nil: c.scope = c.scope.up`)
    # would keep reading the stale, pre-loop address and never terminate.
    # The hoist model can't express "recompute per iteration", so don't CSE
    # condition candidates at all; the same expression inside the body still
    # gets CSE'd normally (anchored to a body statement, inside the loop).
    let anchor = cursorAt(c.orig[], stmtPos)
    if anchor.stmtKind in {WhileS, LoopS}:
      return
    let tempSym = freshTempSym(c)
    let varIdx = addVarDecl(c, tempSym, n, info)
    addInsertPatch(c, stmtPos, varIdx)
    let derefIdx = addDeref(c, tempSym, info)
    addSubstPatch(c, exprPos, derefIdx)
    entry = CachedEntry(tempSym: tempSym, exprPos: exprPos)
    c.cache[key] = entry
  else:
    # Subsequent occurrence: just substitute.
    let derefIdx = addDeref(c, entry.tempSym, info)
    addSubstPatch(c, exprPos, derefIdx)

proc trExpr(c: var Context; n: var Cursor) =
  case n.kind
  of ParLe:
    case n.exprKind
    of AddrC:
      let s = firstSymbolIn(n.firstSon)
      if s != SymId(0): markAddrTaken(c, s)
      skip n
    of CallC:
      let call = n
      n.into:
        if n.hasMore: skip n               # callee
        while n.hasMore: trExpr(c, n)
      invalidateForCall(c, call)
    of DotC, AtC, DerefC:
      if isCSECandidate(n):
        handleCandidate(c, n)
      # Whether or not we cached, recurse into children so any nested
      # candidate (e.g. the `idx` of an `(at arr (dot s.field i))`)
      # also gets a chance — except for the bare lvalue base, which is
      # a name, not a value.
      n.into:
        if n.hasMore: skip n               # base
        while n.hasMore: trExpr(c, n)
    else:
      n.loopInto:
        trExpr(c, n)
  else:
    inc n

proc trVar(c: var Context; n: var Cursor) =
  let d = takeVarDecl(n)
  if d.value.kind != DotToken:
    var v = d.value
    trExpr(c, v)
  # Var decls don't write to existing symbols; nothing to invalidate.

proc trAsgn(c: var Context; n: var Cursor) =
  n.into:
    let lhsStart = n
    skip n
    let rhsStart = n
    skip n
    while n.hasMore: skip n

  var r = rhsStart
  trExpr(c, r)

  case lhsStart.kind
  of Symbol:
    invalidateMentioning(c, lhsStart.symId)
  of ParLe:
    var l = lhsStart
    trExpr(c, l)
    # A store *through a pointer* — `(asgn (dot p f) …)`, `(asgn (at a i) …)`,
    # `(asgn (deref p) …)` — writes to a memory location we cannot
    # disambiguate without alias analysis. Every cached entry is a
    # `loadsAPointer` chain whose address depends on a pointer value held in
    # memory, so such a store may invalidate any of them (e.g. rewriting
    # `p.next` makes a cached `addr(p.next.field)` stale). The previous
    # `invalidateForCall` here was a no-op whenever no local had its address
    # taken, so a loop that advanced via a through-pointer store kept reusing
    # the stale address and never terminated. Be conservative: drop the cache.
    clearCache c
  else: discard

proc trCallStmt(c: var Context; n: var Cursor) =
  let call = n
  n.into:
    if n.hasMore: skip n
    while n.hasMore: trExpr(c, n)
  invalidateForCall(c, call)

proc trIf(c: var Context; n: var Cursor) =
  openBranches c
  n.loopInto:
    case n.substructureKind
    of ElifU:
      n.into:
        if n.hasMore: trExpr(c, n)
        openBranch c
        if n.hasMore: tr(c, n)
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
    if n.hasMore: trExpr(c, n)
    openBranches c
    while n.hasMore:
      case n.substructureKind
      of OfU:
        n.into:
          if n.hasMore: skip n
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
  case n.stmtKind
  of WhileS:
    n.into:
      if n.hasMore: trExpr(c, n)
      if n.hasMore: tr(c, n)
      while n.hasMore: skip n
  of LoopS:
    n.into:
      if n.hasMore: tr(c, n)
      if n.hasMore: trExpr(c, n)
      if n.hasMore: tr(c, n)
      if n.hasMore: tr(c, n)
      while n.hasMore: skip n
  else:
    skip n

proc trLoop(c: var Context; n: var Cursor) =
  var writes = initHashSet[SymId]()
  var addrs = initHashSet[SymId]()
  preScanWrites(n, writes, addrs)
  for s in addrs: markAddrTaken(c, s)

  openBranches c
  openBranch c
  closeBranch c
  openBranch c
  clearCache c
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
  clearCache c

proc isHoistAnchor(sk: NifcStmt): bool {.inline.} =
  sk notin {NoStmt, StmtsS, ScopeS}

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of ParLe:
    let sk = n.stmtKind
    let pushed = isHoistAnchor(sk)
    if pushed:
      c.stmtStack.add cursorToPosition(c.orig[], n)
    case sk
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
    if pushed:
      discard c.stmtStack.pop()
  of Symbol: inc n
  else: inc n

# ---- public entry --------------------------------------------------------

proc runCSE*(buf: var TokenBuf; moduleSuffix = "M";
             summaries: ptr FunctionSummaryTable = nil) =
  ## Two-phase address-CSE for `buf`. Rewrites repeated address-yielding
  ## memory chains (`x.field[i]`, `arr[idx]`, `(deref complex-ptr)`) to
  ## share a single hoisted `(var :t . . (addr <expr>))` and use
  ## `(deref t)` at every occurrence.
  var ctx = createContext(addr buf, moduleSuffix, summaries)
  var n = beginRead(buf)
  tr(ctx, n)
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    var stream = nifstreams.openFromBuffer(src, "M")
    result = fromStream(stream)

  discard pool.syms.getOrIncl("x.0.M")
  discard pool.syms.getOrIncl("y.0.M")
  discard pool.syms.getOrIncl("z.0.M")
  discard pool.syms.getOrIncl("a.0.M")
  discard pool.syms.getOrIncl("i.0.M")
  discard pool.syms.getOrIncl("fld.0.M")
  discard pool.syms.getOrIncl("side.0.M")
  discard pool.syms.getOrIncl("c.0.M")
  discard pool.syms.getOrIncl("p.0.M")     # a local pointer
  discard pool.syms.getOrIncl("pp.0.M")    # a pointer-to-pointer
  discard pool.syms.getOrIncl("fld2.0.M")

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = render(buf)
    runCSE buf
    assertRender(buf, before)

  # Stack/static-rooted access chains are NOT CSE candidates: their address is
  # a constant/scaled offset from a known slot, so caching it would only add a
  # pointer plus an indirection.
  block stack_rooted_not_cached:
    assertUnchanged(
      "(stmts (asgn y.0.M (dot x.0.M fld.0.M)) (asgn z.0.M (dot x.0.M fld.0.M)))")

  block nested_stack_rooted_not_cached:
    # `x.fld.fld2[4]` — a compile-time offset from the stack slot `x`.
    assertUnchanged(
      "(stmts (asgn y.0.M (at (dot (dot x.0.M fld.0.M) fld2.0.M) 4)) (asgn z.0.M (at (dot (dot x.0.M fld.0.M) fld2.0.M) 4)))")

  block single_deref_of_local_not_cached:
    # `(*p).fld` — one deref of a *local* pointer. The address is `p + offset`
    # (p already a value), so the lone load happens either way; caching only
    # spends another local.
    assertUnchanged(
      "(stmts (asgn y.0.M (dot (deref p.0.M) fld.0.M)) (asgn z.0.M (dot (deref p.0.M) fld.0.M)))")

  # The chains below dereference a pointer that is itself a load (`**pp`), so
  # computing the address re-loads `*pp` each time — that is when address-CSE
  # actually pays off.
  block deref_dot_shared:
    var buf = parse(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(asgn z.0.M
(deref cse.t.1.M)))""")

  block deref_at_shared:
    var buf = parse(
      "(stmts (asgn y.0.M (at (deref (deref pp.0.M)) i.0.M)) (asgn z.0.M (at (deref (deref pp.0.M)) i.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(at
(deref
(deref pp.0.M))i.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(asgn z.0.M
(deref cse.t.1.M)))""")

  block call_does_not_invalidate:
    var buf = parse(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (call side.0.M) (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(call side.0.M)
(asgn z.0.M
(deref cse.t.1.M)))""")

  block addr_then_call_invalidates:
    # i's address was taken; the call could modify i → cache cleared,
    # the second `(**pp)[i]` gets its own temp.
    var buf = parse(
      "(stmts (asgn y.0.M (at (deref (deref pp.0.M)) i.0.M)) (call side.0.M (addr i.0.M)) (asgn z.0.M (at (deref (deref pp.0.M)) i.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(at
(deref
(deref pp.0.M))i.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(call side.0.M
(addr i.0.M))
(var :cse.t.2.M . .
(addr
(at
(deref
(deref pp.0.M))i.0.M)))
(asgn z.0.M
(deref cse.t.2.M)))""")

  block asgn_to_index_invalidates:
    var buf = parse(
      "(stmts (asgn y.0.M (at (deref (deref pp.0.M)) i.0.M)) (asgn i.0.M 5) (asgn z.0.M (at (deref (deref pp.0.M)) i.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(at
(deref
(deref pp.0.M))i.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(asgn i.0.M 5)
(var :cse.t.2.M . .
(addr
(at
(deref
(deref pp.0.M))i.0.M)))
(asgn z.0.M
(deref cse.t.2.M)))""")

  block unrelated_asgn_does_not_invalidate:
    var buf = parse(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (asgn z.0.M 7) (asgn a.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(asgn z.0.M 7)
(asgn a.0.M
(deref cse.t.1.M)))""")

  block single_use_still_introduces_temp:
    # Documented limitation of the eager strategy.
    var buf = parse("(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M)))""")

  block nested_in_arithmetic:
    var buf = parse(
      "(stmts (asgn y.0.M (add (i 32) (dot (deref (deref pp.0.M)) fld.0.M) 1)) (asgn z.0.M (add (i 32) (dot (deref (deref pp.0.M)) fld.0.M) 2)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(add
(i 32)
(deref cse.t.1.M)1))
(asgn z.0.M
(add
(i 32)
(deref cse.t.1.M)2)))""")

  block loop_clears_cache:
    var buf = parse(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (while c.0.M (stmts)) (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(while c.0.M
(stmts))
(var :cse.t.2.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn z.0.M
(deref cse.t.2.M)))""")

  # ---- dominance: pre-if expression dominates uses inside branches ----

  block dominance_into_branch:
    var buf = parse(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (if (elif c.0.M (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(if
(elif c.0.M
(asgn z.0.M
(deref cse.t.1.M)))))""")

  block dominance_into_both_branches:
    var buf = parse(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (if (elif c.0.M (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M))) (else (asgn a.0.M (dot (deref (deref pp.0.M)) fld.0.M)))))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(if
(elif c.0.M
(asgn z.0.M
(deref cse.t.1.M)))
(else
(asgn a.0.M
(deref cse.t.1.M)))))""")

  block dominance_within_branch:
    var buf = parse(
      "(stmts (if (elif c.0.M (stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M))))))")
    runCSE buf
    assertRender(buf, """
(stmts
(if
(elif c.0.M
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(asgn z.0.M
(deref cse.t.1.M))))))""")

  block dominance_then_post_if_use:
    # Pre-if first occurrence, branch use, post-if use → all three
    # share one temp because no branch invalidates the cache.
    var buf = parse(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (if (elif c.0.M (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))) (asgn a.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")
    runCSE buf
    assertRender(buf, """
(stmts
(var :cse.t.1.M . .
(addr
(dot
(deref
(deref pp.0.M))fld.0.M)))
(asgn y.0.M
(deref cse.t.1.M))
(if
(elif c.0.M
(asgn z.0.M
(deref cse.t.1.M))))
(asgn a.0.M
(deref cse.t.1.M)))""")

  echo "cse.nim: all self-tests passed"

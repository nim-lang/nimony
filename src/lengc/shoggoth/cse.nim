#
#
#           Lengc Address CSE
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Two-phase **value** common-subexpression elimination for memory loads in
## Lengc. Runs over **nifcore** cursors and uses [patchsets](patchsets.nim) for
## the structural rewrites (same family as [induction_variables](induction_variables.nim)).
##
## A repeated load `x.field[i]` is computed once into a temp and reused:
##
## ```
## y = x.field[i]            var t = x.field[i]
## z = x.field[i]      →     y = t
##                          z = t
## ```
##
## This eliminates the load itself (stronger than address-CSE), but the cached
## value is stale the moment anything writes the memory it reads — including the
## leaf. That precise "does this call clobber what I read?" question is answered
## by the alias-aware function summaries (`invalidateForCall`).
##
## Candidates: any memory load — `(dot …)`, `(at …)`, `(deref …)`, `(pat …)`.
## An assignment's LHS is a write target, not a value, so it is never cached;
## `addr` operands (e.g. `var`-parameter arguments) are likewise left alone.
##
## Invalidation: a cached entry is dropped when an event can change the memory
## it reads — a write to a mentioned symbol, an `addr` of a mentioned symbol, a
## through-pointer/field store (drops everything), or a call whose alias-aware
## summary says it may write the graph of one of the entry's symbols.

import std / [tables, sets, hashes, assertions, strutils, formatfloat]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/pragmaKind, tag enums
import ".." / ".." / "models" / tags          # *TagId ordinals for synthesis
import trackers, patchsets
import aliasing                               # intra-proc Steensgaard alias classes

# ---- function summaries (alias-aware; pure data, mirrors hexer/funcsummary) -
# These are read from `(smry …)` pragmas in the buffer by the nifcore loader
# below (Stage 2). The wire format is pinned in `doc/tags.md`.

type
  ParamEffect* = object
    cls*: uint32
    reads*, writes*, slotWritten*, escapes*: bool

  FunctionSummary* = object
    writesGlobal*, readsGlobal*, callsUnknown*, raises*: bool
    resultCls*: uint32
    resultEscapes*: bool
    params*: seq[ParamEffect]

  FunctionSummaryTable* = Table[SymId, FunctionSummary]
    ## Collected once from the whole module; cse runs per body. The module and
    ## all per-body buffers share one pool, so `SymId` keys stay consistent.

proc paramMayWrite(s: FunctionSummary; idx: int): bool {.inline.} =
  if s.callsUnknown: return true
  if idx < 0 or idx >= s.params.len: return true
  result = s.params[idx].writes or s.params[idx].slotWritten

# ---- nifcore helpers ------------------------------------------------------

proc child0(c: Cursor): Cursor {.inline.} =
  ## Cursor at the first child of the TagLit `c`.
  result = c
  inc result

# ---- candidate predicate --------------------------------------------------
# A candidate is any memory load: `(dot …)`, `(at …)`, `(deref …)`, `(pat …)`.
# Value CSE caches the loaded value, so even stack-rooted loads are worth it
# (the load is eliminated, not just address arithmetic). The check is just the
# tag membership, done inline in `trExpr`.

# ---- context --------------------------------------------------------------

type
  CachedEntry = object
    hasFirst: bool       # a first occurrence has been recorded
    firstExprPos: int    # position in `orig` of the first occurrence
    anchorStack: seq[int] # enclosing hoist-anchor statements at the first
                          # occurrence (innermost last); the decl is hoisted at
                          # the deepest anchor common to all occurrences

  PendingDecl = object   # a materialized temp whose decl is hoisted at the end,
    tempName: string     # so its position can move up as further occurrences in
    exprPos: int         # shallower scopes appear
    addrMode: bool
    commonStack: seq[int] # deepest hoist-anchor common to all occurrences
    firstStack: seq[int] # the first (earliest) occurrence's anchor stack
    minOuter: int        # earliest outermost anchor across occurrences (used
                         # when they share no anchor → hoist at the body level)
    hoistPos: int

  Context = object
    orig: ptr TokenBuf
    cache: Tracker[string, CachedEntry]
    materialized: Table[int, string]   # first-occurrence pos → temp name (not
                                       # branch-scoped: sibling branches reusing
                                       # one dominating first share the temp)
    pending: Table[int, PendingDecl]   # first-occurrence pos → deferred decl
    writeTargets: HashSet[string]      # hashExpr of every lvalue that is an
                                       # assignment target → those are address-
                                       # CSE'd (`(deref t)`), not value-CSE'd
    addrTaken: HashSet[SymId]
    patchset: Patchset
    synth: seq[TokenBuf]
    stmtStack: seq[int]
    tempCounter: int
    moduleSuffix: string
    summaries: ptr FunctionSummaryTable
    aa: Aliasing                ## intra-proc alias partition of this body

proc createContext(orig: ptr TokenBuf; moduleSuffix: string;
                   summaries: ptr FunctionSummaryTable): Context =
  Context(orig: orig,
          cache: initTracker[string, CachedEntry](),
          materialized: initTable[int, string](),
          pending: initTable[int, PendingDecl](),
          writeTargets: initHashSet[string](),
          addrTaken: initHashSet[SymId](),
          patchset: initPatchset(orig),
          synth: @[],
          stmtStack: @[],
          tempCounter: 0,
          moduleSuffix: moduleSuffix,
          summaries: summaries)

proc currentStmtPos(c: Context): int {.inline.} =
  if c.stmtStack.len > 0: c.stmtStack[^1] else: -1

proc freshTempName(c: var Context): string =
  inc c.tempCounter
  result = "`cse." & $c.tempCounter

# ---- expression hashing ---------------------------------------------------

proc hashExpr(c: Cursor; result: var string) =
  case c.kind
  of Symbol, SymbolDef:
    result.add 'S'
    result.add symName(c)
  of IntLit:
    result.add 'I'
    result.add $intVal(c)
  of UIntLit:
    result.add 'U'
    result.add $uintVal(c)
  of FloatLit:
    result.add 'F'
    result.add $floatVal(c)
  of CharLit:
    result.add 'C'
    result.addInt int charLit(c)
  of StrLit:
    result.add '"'
    result.add strVal(c)
    result.add '"'
  of Ident:
    result.add 'i'
    result.add strVal(c)
  of TagLit:
    result.add '('
    result.addInt int cursorTagId(c)
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
    return symId(cur) == target
  of TagLit:
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
    return symId(cur) in targets
  of TagLit:
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
    return symId(c)
  of TagLit:
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
  if start.kind != TagLit: return
  if start.stmtKind in {AsgnS, StoreS}:
    let lhs = child0(start)
    if lhs.kind == Symbol:
      writes.incl symId(lhs)
  if start.exprKind == AddrC:
    let s = firstSymbolIn(child0(start))
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
    if not entry.hasFirst: continue
    let exprCur = cursorAt(c.orig[], entry.firstExprPos)
    if expressionMentions(exprCur, target):
      toClear.add key
  for key in toClear:
    c.cache[key] = default(CachedEntry)

proc expressionTouchesClass(c: var Context; cur: Cursor;
                            reps: HashSet[SymId]): bool =
  ## True if `cur` reads a symbol whose alias-class representative is in `reps`.
  if not cur.hasMore: return false
  case cur.kind
  of Symbol:
    return c.aa.find(symId(cur)) in reps
  of TagLit:
    var n = cur
    var found = false
    n.loopInto:
      if not found and expressionTouchesClass(c, n, reps):
        found = true
      skip n
    return found
  else:
    return false

proc clobberClasses(c: var Context; reps: HashSet[SymId]; exempt = "") =
  ## Drop every cached entry that reads memory in one of the alias classes,
  ## except the entry keyed `exempt` (the address temp of the exact location a
  ## store writes — a leaf write does not move its own address).
  if reps.len == 0: return
  var toClear: seq[string] = @[]
  for key, entry in c.cache.pairs:
    if not entry.hasFirst: continue
    if key == exempt: continue
    if expressionTouchesClass(c, cursorAt(c.orig[], entry.firstExprPos), reps):
      toClear.add key
  for key in toClear:
    c.cache[key] = default(CachedEntry)

proc invalidateForStore(c: var Context; lhs: Cursor) =
  ## A store through a pointer/field writes the location `lhs`. Drop only the
  ## cached loads whose chain may alias it — i.e. read memory in the same alias
  ## class as `lhs`'s base — instead of clearing the whole cache. The *address*
  ## temp of `lhs` itself survives: writing a location does not move its address.
  let s = firstSymbolIn(lhs)
  if s == SymId(0):
    clearCache c                 # indeterminate target → conservative
    return
  var reps = initHashSet[SymId]()
  reps.incl c.aa.find(s)
  clobberClasses(c, reps, exempt = hashExpr(lhs))

proc callSummary(c: Context; call: Cursor; summary: var FunctionSummary): bool =
  if c.summaries == nil: return false
  if call.kind != TagLit: return false
  let callee = child0(call)
  if callee.kind != Symbol: return false
  let key = symId(callee)
  if not c.summaries[].hasKey(key): return false
  summary = c.summaries[].getOrDefault(key)
  result = true

proc invalidateForCall(c: var Context; call: Cursor) =
  ## A cached entry is a `loadsAPointer` address chain whose value depends on
  ## pointers held in memory reachable from the chain's symbols. A callee can
  ## change one of those pointers only by writing through memory it can reach:
  ## a global, or the reachable graph of one of its pointer arguments.
  ##
  ## With an alias-aware summary we no longer clear the whole cache on any
  ## write. From the summary's partition we compute the set of *actual*
  ## arguments whose graph the callee may write — a written parameter plus every
  ## parameter the callee placed in the same partition class, since it may have
  ## aliased them internally — and drop only the cache entries that mention one
  ## of those actuals (or a symbol whose address has escaped, which the callee
  ## could reach through an alias). Entries touching only unrelated locals
  ## survive the call, which is the whole point of address CSE.
  ##
  ## Soundness rests on the same aliasing model the rest of this pass uses:
  ## aliasing between distinct locals only arises through escaped addresses
  ## (tracked in `addrTaken`) or through-pointer stores (which clear the whole
  ## cache in `trAsgn`). A write reachable from a global always sets
  ## `writesGlobal` in the summary, handled below.
  var summary = FunctionSummary()
  if not callSummary(c, call, summary):
    clearCache c                 # unknown callee: assume it writes any memory
    return
  if summary.writesGlobal or summary.callsUnknown:
    clearCache c                 # may write through a global → any memory
    return

  # Pass 1: per-actual root symbol, the partition classes the callee may write,
  # and mark escaping pointer args as addr-taken. Iterate with `into` so the
  # scope is bounded to the call's own children (nifcore has no ParRi).
  var argSyms: seq[SymId] = @[]
  var writtenClasses = initHashSet[uint32]()
  var argIndex = 0
  var ca = call
  ca.into:
    if ca.hasMore: skip ca         # callee
    while ca.hasMore:
      let s = firstSymbolIn(ca)
      argSyms.add s
      if s != SymId(0) and argIndex < summary.params.len and
         summary.params[argIndex].escapes:
        markAddrTaken(c, s)
      if paramMayWrite(summary, argIndex):
        let cls = if argIndex < summary.params.len: summary.params[argIndex].cls
                  else: uint32(argIndex)  # varargs actual: its own synthetic class
        writtenClasses.incl cls
      skip ca
      inc argIndex

  if writtenClasses.len == 0:
    return                       # provably read-only call: the cache survives

  # Pass 2: clobbered = the *alias classes* (intra-proc) of the written actuals
  # — those whose parameter shares a written summary class — plus the classes of
  # any escaped symbol. Using the class (not just the actual symbol) closes the
  # gap where a cached load is rooted at a local that aliases the actual.
  var reps = initHashSet[SymId]()
  for s in c.addrTaken: reps.incl c.aa.find(s)
  for i in 0 ..< argSyms.len:
    if argSyms[i] == SymId(0): continue
    let cls = if i < summary.params.len: summary.params[i].cls else: uint32(i)
    if cls in writtenClasses:
      reps.incl c.aa.find(argSyms[i])
  clobberClasses(c, reps)

# ---- synthesis ------------------------------------------------------------

proc synthBuf(c: var Context; cap: int): TokenBuf =
  createTokenBuf(cap, c.orig[].pool, c.orig[].tags)

proc addValueVarDecl(c: var Context; tempName: string; expr: Cursor;
                     info: NifLineInfo): int =
  ## Synthesize `(var :tempName . . <expr>)` — caches the value of `expr`.
  result = c.synth.len
  var buf = synthBuf(c, 16)
  buf.openTag TagId(ord(VarTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymDef tempName
  buf.addDotToken()           # pragmas
  buf.addDotToken()           # type — inferred from initializer
  buf.addSubtree expr
  buf.closeTag()              # var
  c.synth.add ensureMove(buf)

proc addSymUseSynth(c: var Context; tempName: string): int =
  ## Synthesize a bare symbol use `tempName` (replaces a value-CSE occurrence).
  result = c.synth.len
  var buf = synthBuf(c, 2)
  buf.addSymUse tempName
  c.synth.add ensureMove(buf)

proc addAddrVarDecl(c: var Context; tempName: string; expr: Cursor;
                    info: NifLineInfo): int =
  ## Synthesize `(var :tempName . . (addr <expr>))` — caches the ADDRESS of an
  ## lvalue that is also a write target, so reads and writes can share it.
  result = c.synth.len
  var buf = synthBuf(c, 16)
  buf.openTag TagId(ord(VarTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymDef tempName
  buf.addDotToken()           # pragmas
  buf.addDotToken()           # type — inferred from initializer
  buf.openTag TagId(ord(AddrTagId))
  buf.addSubtree expr
  buf.closeTag()              # addr
  buf.closeTag()              # var
  c.synth.add ensureMove(buf)

proc addDerefSynth(c: var Context; tempName: string): int =
  ## Synthesize `(deref tempName)` — a valid lvalue for both reads and writes.
  result = c.synth.len
  var buf = synthBuf(c, 4)
  buf.openTag TagId(ord(DerefTagId))
  buf.addSymUse tempName
  buf.closeTag()
  c.synth.add ensureMove(buf)

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

proc commonPrefix(a, b: seq[int]): seq[int] =
  result = @[]
  var i = 0
  while i < a.len and i < b.len and a[i] == b[i]:
    result.add a[i]
    inc i

proc outerAnchor(stack: seq[int]): int {.inline.} =
  if stack.len > 0: stack[0] else: high(int)

proc hoistPosOf(c: var Context; common, firstStack: seq[int]; minOuter: int): int =
  ## Where to insert the decl. If the deepest common anchor is a *branching*
  ## construct (the occurrences are in different branches), hoist before it —
  ## the shared deps are computed before the branch. If it is a *sequential*
  ## block (a `scope`) the occurrences are sequential within it, so hoist before
  ## the earliest one's child-statement (after any local dep declared in the
  ## block), NOT before the block. With no common anchor, hoist at the body level.
  if common.len == 0:
    return (if minOuter < high(int): minOuter else: -1)
  let deepest = common[^1]
  let a = cursorAt(c.orig[], deepest)
  if a.stmtKind in {IfS, CaseS, WhileS, LoopS, IteS, ItecS}:
    return deepest                       # before the branching construct
  if firstStack.len > common.len:
    return firstStack[common.len]        # before the earliest child in the block
  result = deepest

proc substUse(c: var Context; exprPos: int; tempName: string; addrMode: bool) =
  addSubstPatch(c, exprPos,
    (if addrMode: addDerefSynth(c, tempName) else: addSymUseSynth(c, tempName)))

proc handleCandidate(c: var Context; n: Cursor): bool =
  ## Lazy CSE. The *first* occurrence is only recorded — no temp yet — so
  ## single-use expressions are left untouched. On the *second* occurrence the
  ## temp is materialized; the cache (control flow) decides *whether* reuse is
  ## valid, while the decl is hoisted at the deepest hoist-anchor common to all
  ## occurrences (the emitted scope that encloses them) — placed at the end so it
  ## can move up as further occurrences appear. An lvalue that is also an
  ## assignment target (in `writeTargets`) is cached by ADDRESS (`var t = addr L`;
  ## every read/write becomes `(deref t)`); everything else by value.
  let key = hashExpr(n)
  let exprPos = cursorToPosition(c.orig[], n)
  let entry = c.cache[key]
  if not entry.hasFirst:
    let stmtPos = c.currentStmtPos
    if stmtPos < 0: return false           # no hoist anchor
    let anchor = cursorAt(c.orig[], stmtPos)
    if anchor.stmtKind in {WhileS, LoopS}:
      # Loop-condition candidate: hoisting before the loop would compute a stale
      # pre-loop value every iteration. Don't CSE here.
      return false
    c.cache[key] = CachedEntry(hasFirst: true, firstExprPos: exprPos,
                               anchorStack: c.stmtStack)
    return false                           # recorded only — not yet a temp
  # Second-or-later occurrence.
  let addrMode = key in c.writeTargets
  let fp = entry.firstExprPos
  var tempName: string
  if c.materialized.hasKey(fp):
    tempName = c.materialized.getOrDefault(fp)
    var pd = c.pending.getOrDefault(fp)    # widen the hoist to enclose this use
    pd.commonStack = commonPrefix(pd.commonStack, c.stmtStack)
    pd.minOuter = min(pd.minOuter, outerAnchor(c.stmtStack))
    pd.hoistPos = hoistPosOf(c, pd.commonStack, pd.firstStack, pd.minOuter)
    c.pending[fp] = pd
  else:
    tempName = freshTempName(c)
    c.materialized[fp] = tempName
    let common = commonPrefix(entry.anchorStack, c.stmtStack)
    let mo = min(outerAnchor(entry.anchorStack), outerAnchor(c.stmtStack))
    c.pending[fp] = PendingDecl(tempName: tempName, exprPos: fp, addrMode: addrMode,
                                commonStack: common, firstStack: entry.anchorStack,
                                minOuter: mo,
                                hoistPos: hoistPosOf(c, common, entry.anchorStack, mo))
    substUse(c, fp, tempName, addrMode)     # rewrite the first occurrence too
  substUse(c, exprPos, tempName, addrMode)
  result = true

proc flushPending(c: var Context) =
  ## Emit each materialized temp's decl at its final (shallowest) hoist position.
  for fp, pd in c.pending:
    if pd.hoistPos < 0: continue
    let firstCur = cursorAt(c.orig[], pd.exprPos)
    let varIdx = if pd.addrMode: addAddrVarDecl(c, pd.tempName, firstCur, rawLineInfo(firstCur))
                 else: addValueVarDecl(c, pd.tempName, firstCur, rawLineInfo(firstCur))
    c.patchset.addInsert(pd.hoistPos, synthCursor(c, varIdx))

proc trExpr(c: var Context; n: var Cursor) =
  case n.kind
  of TagLit:
    case n.exprKind
    of AddrC:
      let s = firstSymbolIn(child0(n))
      if s != SymId(0): markAddrTaken(c, s)
      skip n
    of CallC:
      let call = n
      n.into:
        if n.hasMore: skip n               # callee
        while n.hasMore: trExpr(c, n)
      invalidateForCall(c, call)
    of DotC, AtC, DerefC, PatC:
      # A memory load. A materialized occurrence (2nd+) is replaced by a temp so
      # we stop; otherwise recurse into the index/value children but NOT the base
      # — the base is a location/aggregate, not a value worth caching (caching
      # `(deref b)` would copy the whole object).
      if handleCandidate(c, n):
        skip n
      else:
        n.into:
          if n.hasMore: skip n             # base
          while n.hasMore: trExpr(c, n)    # indices / value children
    else:
      n.loopInto:
        trExpr(c, n)
  else:
    inc n

proc trVar(c: var Context; n: var Cursor) =
  n.into:
    if n.hasMore: skip n                    # name
    if n.hasMore: skip n                    # pragmas
    if n.hasMore: skip n                    # type
    while n.hasMore: trExpr(c, n)           # initializer
  # Var decls don't write existing symbols; nothing to invalidate.

proc trAsgn(c: var Context; n: var Cursor) =
  var lhsStart: Cursor
  var rhsStart: Cursor
  n.into:
    lhsStart = n
    skip n
    rhsStart = n
    skip n
    while n.hasMore: skip n

  var r = rhsStart
  trExpr(c, r)

  case lhsStart.kind
  of Symbol:
    invalidateMentioning(c, symId(lhsStart))
  of TagLit:
    # A store through a pointer/field. The LHS is a write target: if it (also)
    # recurs it is address-CSE'd — `handleCandidate` rewrites it to `(deref t)`,
    # still a valid assignment target. Then invalidate the aliasing class; the
    # address temp of this exact location survives (its address didn't move).
    discard handleCandidate(c, lhsStart)
    invalidateForStore(c, lhsStart)
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
        openFinalBranch c
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
  let probe = child0(n)
  if probe.kind == Symbol:
    gotoLabel(c, LabelId(uint32(symId(probe))))
  skip n

proc trLab(c: var Context; n: var Cursor) =
  let probe = child0(n)
  if probe.kind == SymbolDef:
    landLabel(c, LabelId(uint32(symId(probe))))
  skip n

proc trBreakOrRet(c: var Context; n: var Cursor) =
  skip n
  clearCache c

proc isHoistAnchor(sk: LengStmt): bool {.inline.} =
  # `(scope …)` IS an anchor: it becomes a C `{…}` block, so it must appear on
  # the anchor stack or a temp could be hoisted inside it while a sibling use is
  # outside. `(stmts …)` is transparent (no block of its own).
  sk notin {NoStmt, StmtsS}

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of TagLit:
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
  else:
    inc n

# ---- summary loader: read `(smry …)` pragmas from the buffer ---------------
# A nifcore reader for the wire format produced by hexer/funcsummary (which
# runs on the older nifcursors API). Keying by this buffer's own nifcore SymIds
# keeps the table consistent with the code cse is optimizing. NOTE: never
# `return` out of an `into`/`loopInto` body — that skips cursor restoration.

proc readParamSummary(n: var Cursor; s: var FunctionSummary) =
  n.into:
    if n.hasMore and n.kind == IntLit:
      let idx = int(intVal(n))
      inc n
      if idx >= 0:
        while s.params.len <= idx: s.params.add ParamEffect()
        var cls = uint32(idx)
        if n.hasMore and n.kind == IntLit:
          cls = uint32(intVal(n))
          inc n
        s.params[idx].cls = cls
        while n.hasMore:
          if n.kind == Ident:
            case strVal(n)
            of "reads": s.params[idx].reads = true
            of "writes": s.params[idx].writes = true
            of "slot": s.params[idx].slotWritten = true
            of "escapes": s.params[idx].escapes = true
            else: discard
            inc n
          else:
            skip n
    while n.hasMore: skip n         # drain so `into` sees rem == 0

proc readSummary(n: var Cursor; outSummary: var FunctionSummary) =
  outSummary = FunctionSummary()
  var sawResult = false
  n.into:
    while n.hasMore:
      if n.kind == Ident:
        case strVal(n)
        of "writeGlobal": outSummary.writesGlobal = true; inc n
        of "readGlobal": outSummary.readsGlobal = true; inc n
        of "callsUnknown": outSummary.callsUnknown = true; inc n
        of "raises": outSummary.raises = true; inc n
        of "resultEscapes": outSummary.resultEscapes = true; inc n
        of "result":
          inc n
          if n.hasMore and n.kind == IntLit:
            outSummary.resultCls = uint32(intVal(n))
            sawResult = true
            inc n
        else: inc n
      elif n.kind == TagLit and n.substructureKind == ParamU:
        readParamSummary(n, outSummary)
      else:
        skip n
  if not sawResult:
    outSummary.resultCls = uint32(outSummary.params.len)

proc readSummaryPragma(pragmas: Cursor; outSummary: var FunctionSummary): bool =
  if pragmas.kind != TagLit: return false
  var found = false
  var p = pragmas
  p.loopInto:
    if not found and p.kind == TagLit and p.pragmaKind == SmryP:
      var q = p
      readSummary(q, outSummary)
      found = true
    skip p
  result = found

proc collectFunctionSummaries*(buf: var TokenBuf): FunctionSummaryTable =
  ## Build the module-level summary table from each proc's `(smry …)` pragma.
  ## Call this once on the whole module; pass the result to per-body `runCSE`.
  ## (Leng has no nested procs, so a proc body never carries summaries itself.)
  result = initTable[SymId, FunctionSummary]()
  var n = beginRead(buf)
  if n.kind == TagLit and n.stmtKind == StmtsS:
    n.loopInto:
      if n.kind == TagLit and n.stmtKind == ProcS:
        var p = n
        let d = takeProcDecl(p)
        var summary = FunctionSummary()
        if d.name.kind == SymbolDef and readSummaryPragma(d.pragmas, summary):
          result[symId(d.name)] = summary
      skip n

# ---- write-target pre-pass -----------------------------------------------

proc collectWriteTargets(c: var Context; n: var Cursor) =
  ## Record the hash of every memory lvalue that is an assignment target, so the
  ## main walk knows to address-CSE (not value-CSE) such expressions.
  if not n.hasMore: return
  if n.kind != TagLit:
    inc n
    return
  case n.stmtKind
  of AsgnS:                        # (asgn dest src)
    let lhs = child0(n)
    if lhs.kind == TagLit and lhs.exprKind in {DotC, AtC, DerefC, PatC}:
      c.writeTargets.incl hashExpr(lhs)
    skip n
  of StoreS:                       # (store src dest) — dest is the 2nd child
    var lhs = child0(n)
    skip lhs
    if lhs.kind == TagLit and lhs.exprKind in {DotC, AtC, DerefC, PatC}:
      c.writeTargets.incl hashExpr(lhs)
    skip n
  else:
    n.loopInto:
      collectWriteTargets(c, n)

# ---- public entry --------------------------------------------------------

proc runCSE*(buf: var TokenBuf; moduleSuffix = "M";
             summaries: ptr FunctionSummaryTable = nil) =
  ## Two-phase CSE for a single proc `buf` (a Leng body has no nested procs).
  ## `summaries` is the module-level table from `collectFunctionSummaries` run
  ## once on the whole module; nil ⇒ every call conservatively clears.
  var ctx = createContext(addr buf, moduleSuffix, summaries)
  ctx.aa = computeAliasing(buf)   # alias pre-pass: drives precise invalidation
  block:
    var wn = beginRead(buf)
    collectWriteTargets(ctx, wn)  # decide address- vs value-CSE per expression
  var n = beginRead(buf)
  tr(ctx, n)
  flushPending(ctx)               # emit deferred temp decls at their final positions
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  template chk(input, expected: string) =
    var buf = parse(input)
    runCSE buf
    let got = toString(buf)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runCSE buf
    doAssert toString(buf) == before, "expected unchanged:\n  " & input

  block no_loads_unchanged:
    assertUnchanged("(stmts (asgn y.0.M 5) (asgn z.0.M 7))")

  block stack_field_load_shared:
    # Value CSE caches even stack-rooted loads (the load is eliminated).
    chk(
      "(stmts (asgn y.0.M (dot x.0.M fld.0.M)) (asgn z.0.M (dot x.0.M fld.0.M)))",
      "(stmts (var :`cse.1 . . (dot x.0.M fld.0.M)) " &
      "(asgn y.0.M `cse.1) (asgn z.0.M `cse.1))")

  block deref_dot_shared:
    chk(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))",
      "(stmts (var :`cse.1 . . (dot (deref (deref pp.0.M)) fld.0.M)) " &
      "(asgn y.0.M `cse.1) (asgn z.0.M `cse.1))")

  block call_unknown_prevents_cse:
    # Unknown callee (no summary) clears the cache between the two occurrences,
    # so neither is reused → lazy CSE leaves the body untouched.
    assertUnchanged(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (call side.0.M) (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")

  block index_write_prevents_cse:
    # `i = 5` reassigns the index the load mentions → no reuse → unchanged.
    assertUnchanged(
      "(stmts (asgn y.0.M (at (deref (deref pp.0.M)) i.0.M)) (asgn i.0.M 5) (asgn z.0.M (at (deref (deref pp.0.M)) i.0.M)))")

  block unrelated_asgn_does_not_invalidate:
    chk(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (asgn z.0.M 7) (asgn a.0.M (dot (deref (deref pp.0.M)) fld.0.M)))",
      "(stmts (var :`cse.1 . . (dot (deref (deref pp.0.M)) fld.0.M)) " &
      "(asgn y.0.M `cse.1) (asgn z.0.M 7) (asgn a.0.M `cse.1))")

  block store_disjoint_survives:
    # A store through `qq` does not kill a cached load through `pp`: alias
    # analysis proves the two graphs disjoint. (The old clear-all rule lost it.)
    chk(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) " &
      "(asgn (dot (deref (deref qq.0.M)) g.0.M) v.0.M) " &
      "(asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))",
      "(stmts (var :`cse.1 . . (dot (deref (deref pp.0.M)) fld.0.M)) " &
      "(asgn y.0.M `cse.1) (asgn (dot (deref (deref qq.0.M)) g.0.M) v.0.M) " &
      "(asgn z.0.M `cse.1))")

  block store_aliasing_prevents_cse:
    # Same shape as `store_disjoint_survives` but the store is through `pp` (same
    # alias class as the load) → it kills the cached load → no reuse → unchanged.
    assertUnchanged(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) " &
      "(asgn (dot (deref pp.0.M) g.0.M) v.0.M) " &
      "(asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")

  block lvalue_write_target_address_cse:
    # `a.arr[i]` is read, written, read. Value CSE can't fold a write target, but
    # its address is stable across the write, so it is address-CSE'd: one `addr`
    # temp, `(deref t)` for both reads and the write. (Like `mat()` in alloc.nim.)
    chk(
      "(stmts " &
      "(asgn y.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)) " &
      "(asgn (at (dot (deref a.0.M) arr.0.M) i.0.M) b.0.M) " &
      "(asgn z.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)))",
      "(stmts " &
      "(var :`cse.1 . . (addr (at (dot (deref a.0.M) arr.0.M) i.0.M))) " &
      "(asgn y.0.M (deref `cse.1)) " &
      "(asgn (deref `cse.1) b.0.M) " &
      "(asgn z.0.M (deref `cse.1)))")

  block dominance_into_both_branches:
    chk(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (if (elif c.0.M (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M))) (else (asgn a.0.M (dot (deref (deref pp.0.M)) fld.0.M)))))",
      "(stmts (var :`cse.1 . . (dot (deref (deref pp.0.M)) fld.0.M)) " &
      "(asgn y.0.M `cse.1) " &
      "(if (elif c.0.M (asgn z.0.M `cse.1)) (else (asgn a.0.M `cse.1))))")

  block loop_prevents_cse_across:
    # A loop body may run any number of times, so the cache is cleared across it
    # → the pre- and post-loop loads don't share → unchanged.
    assertUnchanged(
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) (while c.0.M (stmts)) (asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")

  # ---- alias-aware call invalidation via partition summaries ----
  # Module-level: `f` writes only its parameter 0's graph (class 0). cse runs on
  # a body buffer (no procs) with the module summary table. A cached chain
  # rooted at a *different* symbol survives the call; one rooted at the written
  # actual does not. The old "any write clears everything" rule killed both.

  let modBuf = "(stmts (proc :f.0.M . . (pragmas (smry (param 0 0 writes))) .))"

  template chkWithSummaries(modSrc, bodyIn, bodyExpected: string) =
    var mb = parse(modSrc)
    var summaries = collectFunctionSummaries(mb)
    # The module and body buffers share one pool in the real pipeline, so the
    # `SymId` keys line up; mirror that here via `sharedPool`.
    var body = parseFromBuffer(bodyIn, "M", 100,
                               sharedPool = mb.pool, sharedTags = createLengTagPool())
    runCSE(body, "M", addr summaries)
    let got = toString(body)
    let want = canon(bodyExpected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertUnchangedWithSummaries(modSrc, bodyIn: string) =
    var mb = parse(modSrc)
    var summaries = collectFunctionSummaries(mb)
    var body = parseFromBuffer(bodyIn, "M", 100,
                               sharedPool = mb.pool, sharedTags = createLengTagPool())
    let before = toString(body)
    runCSE(body, "M", addr summaries)
    doAssert toString(body) == before, "expected unchanged:\n  " & bodyIn

  block summary_disjoint_survives:
    chkWithSummaries(modBuf,
      "(stmts (asgn y.0.M (dot (deref (deref qq.0.M)) fld.0.M)) " &
      "(call f.0.M pp.0.M) " &
      "(asgn z.0.M (dot (deref (deref qq.0.M)) fld.0.M)))",
      "(stmts (var :`cse.1 . . (dot (deref (deref qq.0.M)) fld.0.M)) " &
      "(asgn y.0.M `cse.1) (call f.0.M pp.0.M) " &
      "(asgn z.0.M `cse.1))")

  block summary_written_prevents_cse:
    # Same shape as `summary_disjoint_survives` but the load is rooted at the
    # written actual `pp` → the call kills it → no reuse → unchanged.
    assertUnchangedWithSummaries(modBuf,
      "(stmts (asgn y.0.M (dot (deref (deref pp.0.M)) fld.0.M)) " &
      "(call f.0.M pp.0.M) " &
      "(asgn z.0.M (dot (deref (deref pp.0.M)) fld.0.M)))")

  echo "cse.nim: all self-tests passed"

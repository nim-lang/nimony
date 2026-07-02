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
import ".." / nifmodules                      # MainModule (type context, threaded through)
import ".." / typenav                         # getType — to skip value-CSE of aggregates

const AddressCSE = false
  ## Address-CSE caches `&location` (as a held pointer temp) so repeated read/write
  ## of one lvalue reuses the address. DISABLED: on the arkham targets a memory
  ## address is folded into the load/store addressing mode (`or [base+idx*8], m`)
  ## for free, so caching it saves no work and instead ties up a pointer register
  ## across the whole live range — measured as the dominant register-pressure source
  ## in the allocator hot path (9 held pointers in rawAlloc). Value-CSE of genuine
  ## loads stays on (a load is a real memory access worth factoring); the write-target
  ## aggregate-skip at `handleCandidate` still holds with an empty `writeTargets`.

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
    firstExprPos: int    # position in `orig` of the first occurrence's LVALUE `L`
                         # (for an `(addr L)` occurrence this is `L`, the child) —
                         # it is what the decl caches (`L` or `addr L`)
    firstUsePos: int     # position of the NODE to rewrite for the first occurrence:
                         # `L` for a load/write, the whole `(addr L)` node for an
                         # address-of. Usually == firstExprPos (differs only for
                         # an address-of first occurrence).
    firstIsAddrOf: bool  # the first occurrence was `(addr L)` (rewrite → bare temp),
                         # not a load/write of `L` (rewrite → `(deref temp)`)
    firstStmtPos: int    # position of the statement directly enclosing the first
                         # occurrence — the (sound) decl-insertion point
    anchorStack: seq[int] # the first occurrence's enclosing hoist anchors
                          # (scope/if/case/loop = C `{}` blocks); a later
                          # occurrence is only reused when this is a prefix of its
                          # own anchor stack, i.e. it lies inside the same block as
                          # the decl (otherwise the temp would be out of scope)

  PendingDecl = object   # a materialized temp; its decl is emitted at flush time
    tempName: string
    exprPos: int         # the first occurrence's LVALUE position (what the decl caches)
    addrMode: bool       # the temp holds `addr L` (a pointer); loads/writes of `L`
                         # rewrite to `(deref t)`, address-of occurrences to bare `t`
    hoistPos: int        # = first occurrence's `firstStmtPos` (dominates every
                         # cache-flow-matched occurrence, and follows all stores
                         # that precede the first use in its block)
    usePositions: seq[tuple[pos: int; deref: bool]]
                           # occurrence positions to rewrite to the temp, each with
                           # its rewrite form: `deref` ⇒ `(deref t)` (an lvalue load/
                           # write under address-CSE), else the bare temp `t` (a value-
                           # CSE use, or an address-of occurrence whose value IS the
                           # cached address). Applied at flush (NOT eagerly) so a temp
                           # whose hoist position turns out unsound can be dropped
                           # without leaving dangling uses

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
    curStmt: int                ## position of the innermost block's CURRENT child
                                ## statement — the decl-insertion point that
                                ## dominates the first occurrence and sits AFTER
                                ## every store/decl preceding it in that block
    tempCounter: int
    moduleSuffix: string
    summaries: ptr FunctionSummaryTable
    aa: Aliasing                ## intra-proc alias partition of this body
    m: ptr MainModule           ## module type context; nil ⇒ no type-based skips
    localDefPos: Table[SymId, int]  ## local symbol → position of its `(var …)` decl,
                                    ## so a cached load is never hoisted above the
                                    ## declaration of a local it reads

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
          curStmt: -1,
          tempCounter: 0,
          moduleSuffix: moduleSuffix,
          summaries: summaries,
          localDefPos: initTable[SymId, int]())

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

proc preScanWrites(start: Cursor; writes, addrs: var HashSet[SymId]) =
  if not start.hasMore: return
  if start.kind != TagLit: return
  if start.stmtKind in {AsgnS, StoreS}:
    let lhs = child0(start)
    if lhs.kind == Symbol:
      writes.incl symId(lhs)
  if start.exprKind == AddrC:
    let s = rootOf(child0(start))
    if s != SymId(0): addrs.incl s
  var n = start
  n.loopInto:
    preScanWrites(n, writes, addrs)
    skip n

# ---- cache invalidation ---------------------------------------------------

proc markAddrTaken(c: var Context; s: SymId; exempt = "")
proc clearCache(c: var Context)

proc invalidateMentioning(c: var Context; target: SymId; exempt = "") =
  ## Drop every cached entry whose expression mentions `target` (its value/address
  ## may have changed). `exempt` keeps one key alive: taking `&L` marks `L`'s root
  ## addr-taken but does NOT change `&L` itself, so `L`'s own address entry survives
  ## (mirrors `invalidateForStore`'s exempt of the written location's address temp).
  var toClear: seq[string] = @[]
  for key, entry in c.cache.pairs:
    if not entry.hasFirst: continue
    if key == exempt: continue
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
  let s = rootOf(lhs)
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
      let s = rootOf(ca)
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

proc emitTempType(c: var Context; buf: var TokenBuf; valueExpr: Cursor; ptrWrap: bool) =
  ## Spell out the temp's declared type from the type context, instead of leaving
  ## the slot empty (`.`) for the C backend to re-infer. Leng's `getNominalType`-
  ## based var-type inference (`codegen.genVarDecl`) fails — yielding `(err)` in a
  ## type position — when the initializer is a load through a field of an
  ## inter-module-inlined *foreign* object, whose type the backend cannot navigate
  ## in this module's context. The optimizer's own type context CAN resolve it, so
  ## we bake the result in here. `ptrWrap` wraps it as `(ptr T)` for the
  ## address-cache form (`var t = addr L`). Falls back to `.` when unresolvable
  ## (the candidate gate already rejects such loads, so this is belt-and-braces).
  if c.m != nil:
    let t = getNominalType(c.m[], valueExpr)
    if not (t.kind == TagLit and t.typeKind == NoType):
      if ptrWrap:
        buf.openTag TagId(ord(PtrTagId))
        buf.addSubtree t
        buf.closeTag()
      else:
        buf.addSubtree t
      return
  buf.addDotToken()

proc addValueVarDecl(c: var Context; tempName: string; expr: Cursor;
                     info: NifLineInfo): int =
  ## Synthesize `(var :tempName . <type> <expr>)` — caches the value of `expr`.
  result = c.synth.len
  var buf = synthBuf(c, 16)
  buf.openTag TagId(ord(VarTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymDef tempName
  buf.addDotToken()           # pragmas
  emitTempType(c, buf, expr, ptrWrap = false)   # declared type
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
  emitTempType(c, buf, expr, ptrWrap = true)    # declared type: (ptr typeof expr)
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

proc markAddrTaken(c: var Context; s: SymId; exempt = "") =
  if s in c.addrTaken: return
  c.addrTaken.incl s
  invalidateMentioning(c, s, exempt)

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward

proc substUse(c: var Context; exprPos: int; tempName: string; deref: bool) =
  ## Rewrite the occurrence at `exprPos` to the temp: `(deref t)` when `deref`
  ## (an lvalue load/write sharing an address temp), else the bare temp `t` (a
  ## value-CSE use, or an address-of occurrence whose value is the cached address).
  addSubstPatch(c, exprPos,
    (if deref: addDerefSynth(c, tempName) else: addSymUseSynth(c, tempName)))

type
  LoadTypeClass = enum
    ltcScalar      ## a value type we can materialize a temp for
    ltcAggregate   ## object/union/array — caching copies the whole thing
    ltcUnknown     ## type navigation failed (`(err)` sentinel)

proc classifyLoad(c: var Context; n: Cursor): LoadTypeClass =
  ## Classify the value loaded by `n` for value-CSE eligibility. Needs the type
  ## context; without it (`m == nil`, e.g. the self-tests) we assume scalar.
  ##
  ## `ltcAggregate`: object/union/array — caching copies the whole aggregate into
  ## a temp (a pessimization), and the temp would need its aggregate type spelled.
  ##
  ## `ltcUnknown`: `getType` returned its `(err)` sentinel (a non-type tag) — the
  ## value's type cannot be navigated, so a hoisted `(var :t . . load)` would force
  ## the C backend to *infer* a type it equally cannot resolve, emitting `(err)` in
  ## a type position. This happens after inter-module inlining splices a foreign
  ## body whose loads reference types not in this module's context. We must not
  ## value-CSE such a load.
  if c.m == nil: return ltcScalar
  let t = getType(c.m[], n)             # navigates nominal → object/array body
  if t.typeKind in {ObjectT, UnionT, ArrayT}: ltcAggregate
  elif t.kind == TagLit and t.typeKind == NoType: ltcUnknown
  else: ltcScalar

proc latestLocalDef(c: Context; start: Cursor): int =
  ## The largest declaration position among the locals read anywhere in the
  ## expression subtree `start` (-1 if it reads none we track). A temp caching
  ## this expression must be hoisted *after* this position; otherwise its decl
  ## would reference a not-yet-declared local. Field selectors and other symbols
  ## that are not local-var decls are simply absent from `localDefPos`.
  result = -1
  let startPos = cursorToPosition(c.orig[], start)
  var e = start
  skip e
  let endPos = cursorToPosition(c.orig[], e)
  var i = startPos
  while i < endPos:
    let tok = cursorAt(c.orig[], i)
    if tok.kind == Symbol:
      let p = c.localDefPos.getOrDefault(tok.symId, -1)
      if p > result: result = p
    inc i

proc handleCandidate(c: var Context; n: Cursor; isAddrOf = false): bool =
  ## Lazy CSE. The *first* occurrence is only recorded — no temp yet — so
  ## single-use expressions are left untouched. On the *second* occurrence the
  ## temp is materialized; the cache (control flow) decides *whether* reuse is
  ## valid, while the decl is hoisted at the deepest hoist-anchor common to all
  ## occurrences (the emitted scope that encloses them) — placed at the end so it
  ## can move up as further occurrences appear. An lvalue that is also an
  ## assignment target OR whose address is taken (in `writeTargets`) is cached by
  ## ADDRESS (`var t = addr L`; every load/write becomes `(deref t)` and every
  ## `(addr L)` becomes bare `t`); everything else by value.
  ##
  ## `isAddrOf`: `n` is an `(addr L)` node; the caller has verified `L` (its child)
  ## is an address-CSE'd memory lvalue. We key on `L` (so it unifies with plain
  ## loads/writes of `L`) but rewrite the whole `(addr L)` node to the bare temp.
  let lvalueCur = if isAddrOf: child0(n) else: n
  let key = hashExpr(lvalueCur)
  # Don't *value*-CSE an aggregate load: it copies the whole object/array. An
  # lvalue that is also a write target / addr-of'd is address-CSE'd (caches a cheap
  # pointer), so it stays eligible — only plain value loads are skipped here. A load
  # whose type cannot be navigated is never CSE'd (in either mode): the materialized
  # temp's inferred type would be `(err)` and break the C backend.
  let cls = classifyLoad(c, lvalueCur)
  if cls == ltcUnknown: return false
  if key notin c.writeTargets and cls == ltcAggregate:
    return false
  let addrMode = key in c.writeTargets
  when not AddressCSE:
    # `addrMode`/`isAddrOf` means we'd cache an ADDRESS (`&L`) as a held pointer temp.
    # Don't: on the arkham targets the address folds into the load/store addressing mode
    # for free, so caching it saves nothing and ties up a register across the live range.
    # Leave the lvalue inline (no temp) — NOT value-CSE'd (unsound across its own writes).
    if addrMode or isAddrOf: return false
  let usePos = cursorToPosition(c.orig[], n)          # node to rewrite
  let lvaluePos = cursorToPosition(c.orig[], lvalueCur)  # `L`, what the decl caches
  let entry = c.cache[key]
  if not entry.hasFirst:
    if c.curStmt < 0: return false         # no enclosing statement → nowhere to hoist
    let stmtPos = c.currentStmtPos
    if stmtPos >= 0 and cursorAt(c.orig[], stmtPos).stmtKind in {WhileS, LoopS}:
      # Loop-condition candidate: hoisting before the loop would compute a stale
      # pre-loop value every iteration. Don't CSE here.
      return false
    c.cache[key] = CachedEntry(hasFirst: true, firstExprPos: lvaluePos,
                               firstUsePos: usePos, firstIsAddrOf: isAddrOf,
                               firstStmtPos: c.curStmt, anchorStack: c.stmtStack)
    return false                           # recorded only — not yet a temp
  # Second-or-later occurrence. The cache is flow-sensitive and intersects at
  # branch joins, so this occurrence is dominated by the first; the first's
  # enclosing statement is therefore a valid, dominating decl site — no widening.
  # But the decl lives in the first occurrence's block, so this use must lie
  # inside that same block (its anchor stack a prefix of ours) or the temp would
  # be out of scope. If not, skip — a later occurrence in the right block may match.
  block:
    let a = entry.anchorStack
    if a.len > c.stmtStack.len: return false
    for i in 0 ..< a.len:
      if a[i] != c.stmtStack[i]: return false
  # A use's rewrite form: an address-of occurrence yields the bare address temp;
  # any other occurrence under address-CSE is an lvalue → `(deref t)`; value-CSE
  # is always bare.
  let derefThis = addrMode and not isAddrOf
  let derefFirst = addrMode and not entry.firstIsAddrOf
  let fp = entry.firstExprPos
  var tempName: string
  if c.materialized.hasKey(fp):
    tempName = c.materialized.getOrDefault(fp)
    var pd = c.pending.getOrDefault(fp)
    pd.usePositions.add (usePos, derefThis)
    c.pending[fp] = pd
  else:
    tempName = freshTempName(c)
    c.materialized[fp] = tempName
    c.pending[fp] = PendingDecl(tempName: tempName, exprPos: fp, addrMode: addrMode,
                                hoistPos: entry.firstStmtPos,
                                usePositions: @[(entry.firstUsePos, derefFirst),
                                                (usePos, derefThis)])
  result = true

proc flushPending(c: var Context) =
  ## Emit each materialized temp's decl at its final (shallowest) hoist position,
  ## then rewrite its occurrences. A temp is DROPPED (decl + all rewrites) when its
  ## hoist position would precede the declaration of a local its expression reads:
  ## the cached expression's free locals must all be in scope at the decl site.
  ## Because the rewrites are applied here (not eagerly), dropping leaves the
  ## original expressions untouched and correct.
  for fp, pd in c.pending:
    if pd.hoistPos < 0: continue
    let firstCur = cursorAt(c.orig[], pd.exprPos)
    if pd.hoistPos <= latestLocalDef(c, firstCur):
      continue                              # unsound hoist — keep the originals
    let varIdx = if pd.addrMode: addAddrVarDecl(c, pd.tempName, firstCur, rawLineInfo(firstCur))
                 else: addValueVarDecl(c, pd.tempName, firstCur, rawLineInfo(firstCur))
    c.patchset.addInsert(pd.hoistPos, synthCursor(c, varIdx))
    for u in pd.usePositions:
      substUse(c, u.pos, pd.tempName, u.deref)

proc trExpr(c: var Context; n: var Cursor) =
  case n.kind
  of TagLit:
    case n.exprKind
    of AddrC:
      let inner = child0(n)
      # `(addr L)` where `L` is an address-CSE'd memory lvalue: unify with loads/
      # writes of `L`. Cache `addr L` once; this whole node rewrites to the bare
      # temp (its value IS the address). `markAddrTaken` still fires for the value-
      # entry soundness of OTHER cached loads through the root, but EXEMPTS `L`'s
      # own address entry — taking `&L` does not change `&L`.
      if inner.kind == TagLit and inner.exprKind in {DotC, AtC, DerefC, PatC} and
         hashExpr(inner) in c.writeTargets:
        discard handleCandidate(c, n, isAddrOf = true)
        let s = rootOf(inner)
        if s != SymId(0): markAddrTaken(c, s, exempt = hashExpr(inner))
        skip n
      else:
        let s = rootOf(inner)
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
  let declPos = cursorToPosition(c.orig[], n)
  var nameStart = default(Cursor)
  var typeStart = default(Cursor)
  n.into:
    if n.hasMore:
      nameStart = n
      skip n                                # name
    if n.hasMore: skip n                    # pragmas
    if n.hasMore:
      typeStart = n
      skip n                                # type
    while n.hasMore: trExpr(c, n)           # initializer
  # Register the local's declared type so a later `getType` on a load through it
  # resolves (mirrors the alias pass / C backend). `computeAliasing` discards
  # scope-nested locals when it closes their scope, so we re-register here as we
  # descend; body symIds are unique, so a flat registration needs no scoping.
  # Empty type slots (optimizer temps) are left unregistered — `getType` then
  # falls through to its error type, which is simply not an aggregate.
  if nameStart.kind == SymbolDef:
    # Remember where this local is declared so `handleCandidate` never hoists a
    # cached load above the declaration of a local it reads (which would emit a
    # use of an out-of-scope symbol).
    c.localDefPos[nameStart.symId] = declPos
    if c.m != nil and not cursorIsNil(typeStart) and typeStart.kind != DotToken:
      c.m[].registerLocal(nameStart.symId, typeStart)
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
  # An anchor is a statement that forms a nested lexical scope — a C `{…}` block:
  # a temp hoisted at the first occurrence's statement must not be reused by an
  # occurrence OUTSIDE that block, so the block must appear on the anchor stack.
  # `(scope)`/`(if)`/`(case)`/`(while)`/`(loop)`/`(try)`/`(onerr)` qualify.
  # `(stmts)` is transparent, and LEAF statements (asgn/store/call/var/ret/…) form
  # no block of their own — anchoring them would give two sibling statements
  # DIFFERENT anchor stacks and defeat all cross-statement CSE (which is the whole
  # point of hoisting a shared load/address to a dominating decl). Unknown/other
  # statements stay anchored (the safe, CSE-suppressing default).
  sk notin {NoStmt, StmtsS,
            AsgnS, StoreS, CallS, VarS, GvarS, TvarS, ConstS, DiscardS,
            RetS, BreakS, RaiseS, JmpS, LabS, EmitS, KeepovfS, MflagS, VflagS}

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
        let savedStmt = c.curStmt
        c.curStmt = cursorToPosition(c.orig[], n)   # this block's current child
        tr(c, n)
        c.curStmt = savedStmt
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
  ## Record the hash of every memory lvalue that should be ADDRESS-CSE'd rather
  ## than value-CSE'd: an assignment target (its value can't be value-cached, but
  ## its address is stable across the write) OR an lvalue whose address is taken
  ## (`(addr L)`) — so a cached `addr L` unifies the `(addr L)` node with plain
  ## loads/writes of `L` (which become `(deref t)`). Recurses into ALL children,
  ## including assignment RHS, since `(addr L)` most often sits there.
  if not n.hasMore: return
  if n.kind != TagLit:
    inc n
    return
  # `(addr L)` of a memory lvalue → `L` is address-CSE'd.
  if n.exprKind == AddrC:
    let inner = child0(n)
    if inner.kind == TagLit and inner.exprKind in {DotC, AtC, DerefC, PatC}:
      c.writeTargets.incl hashExpr(inner)
  case n.stmtKind
  of AsgnS:                        # (asgn dest src)
    let lhs = child0(n)
    if lhs.kind == TagLit and lhs.exprKind in {DotC, AtC, DerefC, PatC}:
      c.writeTargets.incl hashExpr(lhs)
    n.loopInto:
      collectWriteTargets(c, n)    # recurse (find `(addr L)` in the RHS / indices)
  of StoreS:                       # (store src dest) — dest is the 2nd child
    var lhs = child0(n)
    skip lhs
    if lhs.kind == TagLit and lhs.exprKind in {DotC, AtC, DerefC, PatC}:
      c.writeTargets.incl hashExpr(lhs)
    n.loopInto:
      collectWriteTargets(c, n)
  else:
    n.loopInto:
      collectWriteTargets(c, n)

# ---- public entry --------------------------------------------------------

proc runCSE*(buf: var TokenBuf; moduleSuffix = "M";
             summaries: ptr FunctionSummaryTable = nil;
             m: ptr MainModule = nil) =
  ## Two-phase CSE for a single proc `buf` (a Leng body has no nested procs).
  ## `summaries` is the module-level table from `collectFunctionSummaries` run
  ## once on the whole module; nil ⇒ every call conservatively clears. `m` is the
  ## module type context (proc params already registered in the current scope) —
  ## nil falls back to the coarse, type-agnostic alias partition.
  var ctx = createContext(addr buf, moduleSuffix, summaries)
  ctx.m = m                          # type context: skip value-CSE of aggregates
  ctx.aa = computeAliasing(buf, m)   # alias pre-pass: drives precise invalidation
  block:
    var wn = beginRead(buf)
    collectWriteTargets(ctx, wn)  # identify write-target / addr-of'd lvalues
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

  when AddressCSE:
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
  else:
    block lvalue_write_target_left_inline:
      # Address-CSE disabled: the read/write/read of one lvalue stays inline (arkham
      # folds the address into each load/store's addressing mode) — no temp.
      assertUnchanged(
        "(stmts " &
        "(asgn y.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)) " &
        "(asgn (at (dot (deref a.0.M) arr.0.M) i.0.M) b.0.M) " &
        "(asgn z.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)))")

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

  when AddressCSE:   # these validate address-CSE, which is disabled on the arkham path
    block addr_and_load_shared:
      # `a.arr[i]` is loaded, then its ADDRESS is taken (into a pointer local). The
      # address arithmetic is shared: cache `addr(a.arr[i])` once; the load becomes
      # `(deref t)`, the `(addr …)` becomes bare `t`. (The rawDealloc
      # `freeSmallChunks[s div 16]` case — inlined `listAdd` takes the address.)
      chk(
        "(stmts " &
        "(asgn y.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)) " &
        "(asgn p.0.M (addr (at (dot (deref a.0.M) arr.0.M) i.0.M))))",
        "(stmts " &
        "(var :`cse.1 . . (addr (at (dot (deref a.0.M) arr.0.M) i.0.M))) " &
        "(asgn y.0.M (deref `cse.1)) " &
        "(asgn p.0.M `cse.1))")

    block addr_first_then_load:
      # Address taken first, then loaded — same unification, order-independent.
      chk(
        "(stmts " &
        "(asgn p.0.M (addr (at (dot (deref a.0.M) arr.0.M) i.0.M))) " &
        "(asgn y.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)))",
        "(stmts " &
        "(var :`cse.1 . . (addr (at (dot (deref a.0.M) arr.0.M) i.0.M))) " &
        "(asgn p.0.M `cse.1) " &
        "(asgn y.0.M (deref `cse.1)))")

    block two_addrs_shared:
      # Two `(addr L)` occurrences share the address temp.
      chk(
        "(stmts " &
        "(asgn u.0.M (addr (at (dot (deref p.0.M) arr.0.M) i.0.M))) " &
        "(asgn v.0.M (addr (at (dot (deref p.0.M) arr.0.M) i.0.M))))",
        "(stmts " &
        "(var :`cse.1 . . (addr (at (dot (deref p.0.M) arr.0.M) i.0.M))) " &
        "(asgn u.0.M `cse.1) " &
        "(asgn v.0.M `cse.1))")

  when not AddressCSE:
    block addr_load_not_shared_when_disabled:
      # With address-CSE off, a load + a subsequent `(addr L)` of the same lvalue are
      # left inline (arkham folds the address into the load/store) — no shared temp.
      assertUnchanged(
        "(stmts " &
        "(asgn y.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)) " &
        "(asgn p.0.M (addr (at (dot (deref a.0.M) arr.0.M) i.0.M))))")

  block index_write_blocks_addr_load:
    # The index `i` is reassigned between the load and the addr → the address
    # changed → no reuse → unchanged.
    assertUnchanged(
      "(stmts " &
      "(asgn y.0.M (at (dot (deref a.0.M) arr.0.M) i.0.M)) " &
      "(asgn i.0.M 5) " &
      "(asgn p.0.M (addr (at (dot (deref a.0.M) arr.0.M) i.0.M))))")

  echo "cse.nim: all self-tests passed"

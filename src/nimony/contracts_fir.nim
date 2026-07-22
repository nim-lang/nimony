#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

##[
Contract analysis over the **Final IR** (`doc/final_ir.md`).

Tries to prove or disprove `.requires` and `.ensures` annotations and to
verify initialization and not-nil properties.

Where the older `contracts_njvl.nim` eliminated jumps and tracked "did we
already leave" with materialized control-flow flags (`mflag`/`jtrue`) and an
`Implications` lattice, this analysis runs directly on the structured Final IR:

- `(ite cond then else)` for branching
- `(loop body)` — infinite loop; the body ends in `(continue .)` and every
  forward exit is a `(jmp loopExit)`
- `(lab L)` / `(jmp L)` — the structured multi-exit
- `(try body (except ...)* (fin ...)?)`, `(ret ...)`, `(raise ...)`
- `(store value dest)` for assignments

"Did we already leave" is now positional: the `Tracker` (`njvl/tracker.nim`)
carries fall-through reachability and the per-target exit summaries, and a
`(lab)` multi-join resolves them in one forward pass. Per the chosen design the
state is *hybrid*: `inferle` facts stay imperative (`save`/`restore` at branch
points, snapshotted per-exit for the multi-join), while the **Tracker** owns
init-tracking and fall-through.

In order to not be too annoying in the case of a contract violation, the
compiler emits a warning (that can be suppressed or turned into an error).
]##

import std / [assertions, tables, hashes, sets, strutils, syncio]

include ".." / lib / nifprelude
include ".." / lib / compat2

import ".." / models / tags
import ".." / lib / symparser
import ".." / njvl / [njvl_model, finalir]
import flowtracker
import ".." / hexer / passes
import nimony_model, programs, decls, typenav, sembasics, reporters,
  renderer, typeprops, inferle, xints, builtintypes

type
  BorrowableCheck = enum
    IsBorrowable       ## simple path: symbols, dots, array access
    IsBorrowableFromConst
    IsBorrowableFromGlobal
    HasAddr            ## path contains explicit `addr` — unsafe escape hatch
    NotBorrowable      ## deref in middle of path or function call

  BorrowInfo = object
    borrower: SymId   ## variable holding the borrow; upon `(kill borrower)` the borrow ends
    mode: BorrowableCheck
    path: seq[SymId]  ## root :: field1 :: field2 :: ...
    info: PackedLineInfo

  NjvlContext = object
    flow: FlowState                    # the journaled analysis state: the
                                       # definite-assignment init-set and the
                                       # `inferle` facts, mutated in place.
    typeCache: TypeCache
    tr: FlowTracker                    # control flow: fall-through liveness +
                                       # per-exit state accumulation (journaled).
    errors: TokenBuf
    procCanRaise: bool
    moduleSuffix: string
    nestedProcs: int
    loopExitLabels: HashSet[SymId]     # `(lab)`s emitted right after a `(loop)`.
                                       # Their post-join state keeps the pre-loop
                                       # facts (break-site facts are dropped; only
                                       # break-site inits are joined). See
                                       # `bindLoopExit`.
    inlineVars: Table[SymId, Cursor] # var -> to its init expression
    resultSym: SymId                   # symId of the `result` local for the current proc, or NoSymId
    activeBorrows: seq[BorrowInfo]
    verbose: bool                      # --verbose: dump final IR on init/contract
                                       # failures for easier debugging
    currentProcStart: Cursor           # cursor at the start of the proc whose
                                       # body we are currently analysing (used
                                       # for the --verbose dump)

# `c.facts` reads/writes the fact set inside the journaled `FlowState`; the bulk
# of the pass mutates facts through this alias, so it stays spelled `c.facts`.
template facts(c: NjvlContext): untyped = c.flow.facts

proc markInit(c: var NjvlContext; symId: SymId) {.inline.} =
  c.flow.inits.incl symId

proc isInitialized(c: NjvlContext; symId: SymId): bool {.inline.} =
  symId in c.flow.inits

proc dumpCurrentProc(c: var NjvlContext; info: PackedLineInfo; msg: string) =
  ## Dump the NJ IR of the proc currently under analysis to stderr. Used
  ## by `--verbose` so the user can see the lowered form that caused
  ## a contract/init failure. Gated on `c.verbose` — callers still invoke
  ## it unconditionally; this proc is the single decision point.
  if not c.verbose: return
  if cursorIsNil(c.currentProcStart): return
  stderr.writeLine "--- NJ IR (--verbose) for: " & msg
  stderr.writeLine "--- at " & infoToStr(info) & ":"
  stderr.writeLine toString(c.currentProcStart, false)
  stderr.writeLine "--- end NJ IR dump ---"

proc buildErr(c: var NjvlContext; info: PackedLineInfo; msg: string) =
  when defined(debug):
    writeStackTrace()
    echo infoToStr(info) & " Error: " & msg
    quit msg
  dumpCurrentProc(c, info, msg)
  var hintedMsg = msg
  if not c.verbose:
    hintedMsg.add " [pass --verbose for the NJ IR]"
  c.errors.buildTree ErrT, info:
    c.errors.addDotToken()
    c.errors.addStrLit(hintedMsg, info)

proc contractViolation(c: var NjvlContext; orig: Cursor; fact: LeXplusC; report: bool) =
  if report:
    echo "known facts in this context: "
    for i in 0 ..< c.facts.len:
      echo $c.facts[i]
    echo "canonical fact: ", $fact
  error "contract violation: ", orig

# Forward declarations
proc traverseStmt(c: var NjvlContext; n: var Cursor)
proc traverseExpr(c: var NjvlContext; pc: var Cursor)
proc analyseCall(c: var NjvlContext; n: var Cursor)

proc extractSymId(n: Cursor): SymId {.inline.} =
  var n = n
  if n.exprKind in {HaddrX, HderefX}: inc n

  if n.isSymbol:
    result = n.symId
  elif n.isTagLit and n.tagEnum == VTagId:
    result = n.childCursor.symId
  else:
    result = NoSymId

proc extractSymIdForStore(n: Cursor): SymId =
  # idea both (etupat result.0 +0) and (etupat result.0 +1) create
  # a full store to `result.0`.
  var n = n
  if n.njvlKind == EtupatV:
    inc n
  result = extractSymId(n)

proc skipSymbol(r: var Cursor): SymId {.inline.} =
  ## Consume a bare Symbol or (v sym version) node and return its SymId.
  ## Returns NoSymId (without advancing) if r is neither.
  var n = r
  while n.exprKind in {HconvX, ConvX, BaseobjX}:
    inc n
    skip n # type
  result = extractSymId(n)
  if result != NoSymId:
    skip r

# --- Borrow checking ---

proc extractBorrowPath(c: var NjvlContext; n: Cursor; result: var BorrowInfo; followInlineVars=true) =
  ## Extract a path (root :: field1 :: field2 :: ...) from an expression,
  ## expanding inline variables.
  if n.isTagLit:
    let ek = n.exprKind
    if ek in {DotX, DdotX}:
      if ek == DdotX and result.mode != HasAddr:
        result.mode = NotBorrowable
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
      skip r # skip object subtree
      if r.isSymbol:
        result.path.add r.symId
    elif ek == AddrX:
      result.mode = HasAddr
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek == DerefX:
      if result.mode != HasAddr:
        result.mode = NotBorrowable
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in {HaddrX, HderefX}:
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in {TupatX, ArratX, AtX, PatX}:
      # Array/tuple access: recurse into container, don't distinguish indices
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in ConvKinds:
      var r = n
      inc r
      skip r # type
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek == BaseobjX:
      var r = n
      inc r
      skip r # type
      skip r # intlit
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in CallKinds:
      # we borrow from the first argument of the call:
      var r = n
      inc r
      skip r # fn
      extractBorrowPath(c, r, result, followInlineVars)
    elif ek in {AconstrX, SetconstrX, TupconstrX, OconstrX, NilX, TrueX, FalseX}:
      result.mode = IsBorrowableFromConst
    elif n.njvlKind == EtupatV:
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif n.njvlKind == VV:
      extractBorrowPath(c, n.childCursor, result, followInlineVars)
  elif (n.isIntLit or n.isUIntLit or n.isCharLit or n.isFloatLit or n.isStringLit):
    result.mode = IsBorrowableFromConst
  elif n.isSymbol:
    let s = n.symId
    if (followInlineVars or getType(c.typeCache, n).typeKind in {MutT, OutT, LentT}) and s in c.inlineVars:
      extractBorrowPath(c, c.inlineVars.getOrQuit(s), result, followInlineVars)
    else:
      if result.mode != HasAddr:
        result.mode = IsBorrowable
        let res = tryLoadSym(s)
        if res.status == LacksNothing:
          let local = asLocal(res.decl)
          if local.kind in {GvarY, TvarY}:
            result.mode = IsBorrowableFromGlobal
      result.path.add s

proc extractPath(c: var NjvlContext; n: Cursor; followInlineVars=true): BorrowInfo =
  result = BorrowInfo(path: @[], mode: NotBorrowable, info: n.info)
  extractBorrowPath(c, n, result, followInlineVars)

proc `$`(b: BorrowInfo): string =
  result = "BorrowInfo(mode: " & $b.mode & ", path: "
  for i in 0 ..< b.path.len:
    result.add " :: " & pool.syms[b.path[i]]
  result.add ")"

proc pathsOverlap(a, b: BorrowInfo): bool =
  ## Two paths overlap if one is a prefix of the other (or they are equal).
  ## Disjoint siblings (e.g. a.b vs a.c) do not overlap.
  if a.path.len == 0 or b.path.len == 0: return false
  let minLen = min(a.path.len, b.path.len)
  for i in 0 ..< minLen:
    if a.path[i] != b.path[i]:
      return false
  result = true

proc checkBorrowConflict(c: var NjvlContext; mutPath: BorrowInfo; info: PackedLineInfo) =
  for b in c.activeBorrows:
    if pathsOverlap(mutPath, b):
      buildErr c, info, "'" & pool.syms[mutPath.path[0]] & "' is borrowed and cannot be mutated"
      return

proc endBorrow(c: var NjvlContext; sym: SymId) =
  var i = 0
  while i < c.activeBorrows.len:
    if c.activeBorrows[i].borrower == sym:
      # order of active borrows is irrelevant, so swap-delete is fine
      c.activeBorrows.del(i)
    else:
      inc i

template getVarId(c: var NjvlContext; symId: SymId): VarId = VarId(symId)

# --- Range (`range[lo..hi]`) checking ---
#
# Following Araq's design: a value flowing into a `range[lo..hi]` slot carries
# the proof obligation `lo <= value <= hi`. We ask the inferle engine to
# discharge it from the facts known on this path; whatever cannot be proven is
# rejected at compile time. No runtime check is ever emitted (zero runtime cost,
# no new dynamic failure modes). A `range`-typed location, once bound, is itself
# a fact (`lo <= x <= hi`), which is what makes proper subtyping such as
# `range[2..5]` -> `range[0..10]` provable.

proc staticRangeBounds(typ: Cursor; lo, hi: var xint): bool =
  ## Extract the statically-known integer bounds of a `range[lo..hi]` type,
  ## resolving a named range type (a `Symbol`) to its definition. Returns false
  ## for non-range or non-static ranges (which the caller leaves untouched).
  var t = typ
  var guard = 0
  while t.isSymbol and guard < 8:
    let s = tryLoadSym(t.symId)
    if s.status != LacksNothing or s.decl.symKind != TypeY: return false
    t = asTypeDecl(s.decl).body
    inc guard
  if t.typeKind != RangetypeT: return false
  var r = t
  inc r        # skip rangetype tag
  skip r       # skip base type
  case r.kind
  of IntLit: lo = createXint(pool.integers[r.intId])
  of UIntLit: lo = createXint(pool.uintegers[r.uintId])
  else: return false
  inc r
  case r.kind
  of IntLit: hi = createXint(pool.integers[r.intId])
  of UIntLit: hi = createXint(pool.uintegers[r.uintId])
  else: return false
  result = lo <= hi

proc checkRangeAssign(c: var NjvlContext; targetType, value: Cursor) =
  ## Emit and discharge the `lo <= value <= hi` obligation for a value bound to a
  ## `range[lo..hi]`-typed target. Value conversions are handled at the
  ## conversion site (see the `ConvX`/`HconvX` case in `traverseExpr`), so we
  ## skip them here to avoid double-reporting.
  if value.exprKind in {ConvX, HconvX, CastX, BaseobjX}: return
  var lo = zero()
  var hi = zero()
  if not staticRangeBounds(targetType, lo, hi): return

  # 1. The value's own *declared type* may already be a `range` that fits: a
  #    subset `range[aLo..aHi]` with `lo <= aLo` and `aHi <= hi` is provably in
  #    range. This is the type acting as its own proof (proper subtyping such as
  #    `range[2..5]` -> `range[0..10]`), and it is robust across control-flow
  #    joins where flow-derived facts would be intersected away.
  var aLo = zero()
  var aHi = zero()
  if staticRangeBounds(getType(c.typeCache, value), aLo, aHi):
    if lo <= aLo and aHi <= hi: return

  # 2. Otherwise, discharge `lo <= value <= hi` from the facts known on this
  #    path (e.g. a preceding `if a >= 0 ... a <= 10` guard, or a `range`-typed
  #    parameter whose bounds were seeded on entry).
  var v = VarId(0)
  var off = zero()
  var isLit = false
  var r = value
  let sym = skipSymbol(r)
  if sym != NoSymId:
    v = getVarId(c, sym)
  else:
    case value.kind
    of IntLit: off = createXint(pool.integers[value.intId]); isLit = true
    of UIntLit: off = createXint(pool.uintegers[value.uintId]); isLit = true
    else:
      # A value we cannot model cannot be proven in range, so we reject it.
      buildErr c, value.info, "cannot prove value is in range " & $lo & ".." & $hi
      return

  # lo <= v + off   <=>   0 <= v + (off - lo)
  let lower = query(VarId(0), v, off - lo)
  # v + off <= hi   <=>   v <= 0 + (hi - off)
  let upper = query(v, VarId(0), hi - off)
  if not (implies(c.facts, lower) and implies(c.facts, upper)):
    if isLit:
      buildErr c, value.info, "value out of range: " & $off & " notin " & $lo & ".." & $hi
    elif sym != NoSymId:
      buildErr c, value.info, "cannot prove '" & pool.syms[sym] &
        "' is in range " & $lo & ".." & $hi
    else:
      buildErr c, value.info, "cannot prove value is in range " & $lo & ".." & $hi

proc seedRangeFacts(c: var NjvlContext; sym: SymId; typ: Cursor) =
  ## Record that a `range[lo..hi]`-typed parameter holds a value within its
  ## bounds on entry, so obligations that pass it on to an equal-or-wider range
  ## are provable from facts even when the value's static type is erased (e.g.
  ## after arithmetic). Range-to-range narrowing itself is proven structurally
  ## in `checkRangeAssign` and does not depend on this.
  var lo = zero()
  var hi = zero()
  if staticRangeBounds(typ, lo, hi):
    let v = getVarId(c, sym)
    c.facts.add query(VarId(0), v, -lo)  # lo <= v
    c.facts.add query(v, VarId(0), hi)   # v <= hi

# --- Fact extraction from conditions ---

proc rightHandSide(c: var NjvlContext; pc: var Cursor; fact: var LeXplusC): bool =
  result = false
  if pc.exprKind in {AddX, SubX}:
    pc.into:
      skip pc # type
      let symId2 = skipSymbol(pc)
      if symId2 != NoSymId:
        fact.b = getVarId(c, symId2)
        if pc.isIntLit:
          fact.c = fact.c + createXint(pool.integers[pc.intId])
          result = true
          inc pc
        elif pc.kind == UIntLit:
          fact.c = fact.c + createXint(pool.uintegers[pc.uintId])
          result = true
          inc pc
        else:
          traverseExpr c, pc
      else:
        traverseExpr c, pc
        traverseExpr c, pc
  elif (let symId2 = skipSymbol(pc); symId2 != NoSymId):
    fact.b = getVarId(c, symId2)
    result = true
  elif pc.isIntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pool.integers[pc.intId])
    result = true
    inc pc
  elif pc.kind == UIntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pool.uintegers[pc.uintId])
    result = true
    inc pc
  elif pc.exprKind == NilX:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(0'i32)
    result = true
    skip pc
  else:
    traverseExpr c, pc

proc translateCond(c: var NjvlContext; pc: var Cursor; wasEquality: var bool): LeXplusC =
  var r = pc
  result = LeXplusC(a: InvalidVarId, b: VarId(0), c: createXint(0'i32))

  var negations = 0
  var notScopes: seq[Cursor] = @[]
  while r.exprKind == NotX:
    inc negations
    notScopes.add r
    r = sub(r)

  template unwindNegations() =
    while negations > 0:
      negateFact(result)
      dec negations
      let h = notScopes.pop(); r = h; skip r

  let xk = r.exprKind
  var cmpStart = default(Cursor)
  if xk in {LeX, LtX}:
    cmpStart = r; r = sub(r)
    skip r # skip type
  elif xk == EqX:
    wasEquality = negations == 0  # negated equality is inequality, not equality
    cmpStart = r; r = sub(r)
    skip r # skip type
  elif xk == InstanceofX:
    # `(instanceof x T)` truthy: x is not nil (and is at least T at runtime).
    # We don't model the type-narrowing here, just the not-nil consequence.
    var probe = r
    inc probe
    let sa = extractSymId(probe)
    if sa != NoSymId:
      result = isNotNil(getVarId(c, sa))
    else:
      traverseExpr c, pc
      return result
    skip r
    unwindNegations()
    pc = r
    return result
  else:
    # Check for a bare symbol/hderef/haddr (truthy ref check: `if x:` means `x != nil`)
    let sa = extractSymId(r)
    if sa != NoSymId:
      result = isNotNil(getVarId(c, sa))
      skip r
      unwindNegations()
      pc = r
    else:
      traverseExpr c, pc
    return result

  if r.isIntLit:
    result.a = VarId(0)
    result.c = -createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    result.a = VarId(0)
    result.c = -createXint(pool.uintegers[r.uintId])
    inc r
  elif (let sa = skipSymbol(r); sa != NoSymId):
    result.a = getVarId(c, sa)
  elif r.exprKind == NilX:
    result.a = VarId(0)
    skip r
  else:
    traverseExpr c, pc
    return result
  if r.exprKind == NilX:
    wasEquality = false
  if not rightHandSide(c, r, result):
    result.a = InvalidVarId
  # a < b  --> a <= b - 1:
  if xk == LtX:
    result.c = result.c - createXint(1'i32)
  if xk in {LeX, LtX, EqX}: r = cmpStart; skip r

  unwindNegations()

  pc = r

proc analyseCondition(c: var NjvlContext; pc: var Cursor): int =
  ## Returns number of facts added
  var wasEquality = false
  let fact = translateCond(c, pc, wasEquality)
  if fact.isValid:
    c.facts.add fact
    if wasEquality:
      c.facts.add fact.geXplusC
      result = 2
    else:
      result = 1
  else:
    result = 0

# --- Not-nil checking ---

proc markedAs(t: Cursor; mark: NimonyOther): bool =
  # Look through value-passing wrappers like `sink`/`mut`/`lent`/`out`:
  # they don't change a value's nilability, only how it's passed. Without
  # this, a `sink (ref T notnil)` parameter looked nilable, and NJ asked
  # for a non-nil proof on a value the type system already guarantees.
  var t = t
  while t.typeKind in {SinkT, MutT, LentT, OutT}:
    inc t
  result = false
  case t.typeKind
  of PtrT, RefT:
    var e = t.childCursor
    skip e # base type
    if e.hasMore and e.substructureKind == mark:
      result = true
  of CstringT, PointerT:
    let e = t.childCursor
    # no base type
    if e.hasMore and e.substructureKind == mark:
      result = true
  of ProctypeT:
    # New layout: `(proctype <NilTag> (params) RetType <Pragmas>)`. The
    # nilability marker is at slot 0.
    let e = t.childCursor
    if e.substructureKind == mark:
      result = true
  else:
    discard

proc analysableRoot(c: var NjvlContext; n: Cursor): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, TupatX, ArratX, HderefX:
      inc n
    of ConvKinds:
      inc n
      skip n # type part
    of BaseobjX:
      inc n
      skip n # type part
      skip n # skip intlit
    else:
      break
  let s = extractSymId(n)
  if s != NoSymId:
    result = s
    let x = getLocalInfo(c.typeCache, result)
    if x.kind == GvarY:
      # assume sharing of global variables between threads
      result = NoSymId
  else:
    result = NoSymId

proc isNonNilExpr(c: var NjvlContext; n: Cursor): bool =
  ## Check if an expression is trivially non-nil without needing dataflow analysis.
  case n.exprKind
  of AddrX, HaddrX:
    # `(haddr …)` is the synthesised hidden-address form (e.g. emitted
    # for `var V` return lowering); semantically identical to `addr`.
    result = true
  of ConvKinds:
    # e.g. cstring("abc") — a conversion from a non-nil value is non-nil
    var inner = n
    inc inner
    skip inner # skip type part
    result = isNonNilExpr(c, inner)
  of BaseobjX:
    # A base-object upcast (e.g. a derived `ref Dog` widened to `ref Animal`)
    # of a non-nil value is itself non-nil. The operand's static type still
    # carries the `notnil` marker even though the widened result type drops it,
    # so consult the operand's type as well as recursing structurally.
    var inner = n
    inc inner
    skip inner # skip type part
    skip inner # skip inheritance-depth intlit
    result = markedAs(getType(c.typeCache, inner), NotnilU) or isNonNilExpr(c, inner)
  of SufX:
    # suffixed literal, e.g. (suf "abc" "R") — still a literal value
    result = true
  else:
    if n.isStringLit:
      result = true
    else:
      let s = extractSymId(n)
      if s != NoSymId:
        let sk = fetchSymKind(c.typeCache, s)
        result = isRoutine(sk)
      else:
        result = false

proc wantNotNil(c: var NjvlContext; n: Cursor) =
  case n.exprKind
  of NilX:
    buildErr(c, n.info, "expected non-nil value")
  of AddrX, HaddrX:
    discard "fine, addresses (incl. hidden-addr from var-return lowering) are not nil"
  else:
    let t = getType(c.typeCache, n)
    if markedAs(t, NotnilU):
      discard "fine, per type we know it is not nil"
    elif isNonNilExpr(c, n):
      discard "fine, expression is trivially not nil"
    elif t.typeKind in RoutineTypes and not markedAs(t, NilU):
      discard "fine, proc values are not nil unless explicitly marked nil"
    else:
      let r = analysableRoot(c, n)
      if r == NoSymId:
        # account for the fact that NJ already introduced tuples for the error handling:
        var n = n
        if n.exprKind == TupconstrX:
          inc n
          skip n # skip type
          if n.isSymbol and pool.syms[n.symId] == ("Success.0." & SystemModuleSuffix):
            inc n
        if n.exprKind == NewobjX and c.procCanRaise:
          discard "fine, nil value is mapped to OOM by the compiler"
        else:
          buildErr c, n.info, "cannot analyze expression is not nil: " & asNimCode(n)
      else:
        let fact = inferle.isNotNil(VarId r)
        if implies(c.facts, fact):
          discard "fine, did prove access correct"
        else:
          buildErr c, n.info, "cannot prove expression is not nil: " & asNimCode(n)

proc checkNilMatch(c: var NjvlContext; n: Cursor; expected: Cursor) =
  if markedAs(expected, NotnilU):
    wantNotNil c, n

proc wantNotNilDeref(c: var NjvlContext; n: Cursor) =
  let e = getType(c.typeCache, n)
  if markedAs(e, NilU):
    wantNotNil c, n

# --- .requires checking ---

type
  ProofRes = enum
    Unprovable, Disproven, Proven

proc `and`(a, b: ProofRes): ProofRes =
  if a == Proven and b == Proven:
    Proven
  elif a == Disproven or b == Disproven:
    Disproven
  else:
    Unprovable

proc `or`(a, b: ProofRes): ProofRes =
  if a == Proven or b == Proven:
    Proven
  elif a == Disproven and b == Disproven:
    Disproven
  else:
    Unprovable

proc `not`(a: ProofRes): ProofRes =
  if a == Unprovable:
    Unprovable
  elif a == Proven:
    Disproven
  else:
    Proven

proc argAt(call: Cursor; pos: int): Cursor =
  result = call
  inc result
  for i in 0 ..< pos: skip result

proc mapSymbol(c: var NjvlContext; paramMap: Table[SymId, int]; call: Cursor; symId: SymId): VarId =
  result = VarId(0)
  let pos = paramMap.getOrDefault(symId)
  if pos > 0:
    let arg = call.argAt(pos)
    let sid = extractSymId(arg)
    if sid != NoSymId:
      result = getVarId(c, sid)

proc compileCmp(c: var NjvlContext; paramMap: Table[SymId, int]; req, call: Cursor): LeXplusC =
  var r = req
  var a = InvalidVarId
  var b = InvalidVarId
  var cnst = createXint(0'i32)
  let sid = extractSymId(r)
  if sid != NoSymId:
    a = mapSymbol(c, paramMap, call, sid)
    inc r
  let rid = extractSymId(r)
  if rid != NoSymId:
    b = mapSymbol(c, paramMap, call, rid)
    inc r
  elif r.isIntLit:
    b = VarId(0)
    cnst = createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    b = VarId(0)
    cnst = createXint(pool.uintegers[r.uintId])
    inc r
  elif (let op = r.exprKind; op in {AddX, SubX}):
    r = sub(r) # peek only, never left
    skip r # type
    let cid = extractSymId(r)
    if cid != NoSymId:
      b = mapSymbol(c, paramMap, call, cid)
      inc r
      if r.isIntLit:
        cnst = createXint(pool.integers[r.intId])
      elif r.kind == UIntLit:
        cnst = createXint(pool.uintegers[r.uintId])
      else:
        error "expected integer literal but got: ", r
    else:
      error "expected symbol but got: ", r
  result = query(a, b, cnst)

proc checkReq(c: var NjvlContext; paramMap: Table[SymId, int]; req, call: Cursor): ProofRes =
  case req.exprKind
  of AndX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    result = a and b
  of OrX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    result = a or b
  of NotX:
    var r = req
    inc r
    result = not checkReq(c, paramMap, r, call)
  of EqX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    let cm2 = cm.geXplusC
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm) and implies(c.facts, cm2):
      result = Proven
    else:
      result = Disproven
  of LeX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm):
      result = Proven
    else:
      result = Disproven
  of LtX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm.ltXplusC):
      result = Proven
    else:
      result = Disproven
  of ExprX:
    var r = req
    while r.exprKind == ExprX:
      r = sub(r) # throwaway copy; bounds the walk under vpr
      while r.hasMore and not isLastSon(r): skip r
    result = checkReq(c, paramMap, r, call)
  else:
    result = Unprovable

# --- Expression analysis ---

proc analyseOconstr(c: var NjvlContext; n: var Cursor) =
  n.into:
    let objType = n
    skip n # type
    while n.hasMore:
      assert n.substructureKind == KvU
      n.into:
        assert n.isSymbol
        let expected = lookupField(c.typeCache, objType, n.symId)
        assert not cursorIsNil(expected), "could not lookup type for " & pool.syms[n.symId]
        skip n # field name
        checkNilMatch c, n, expected
        skip n # value
        if n.hasMore:
          # optional inheritance
          skip n

proc analyseArrayConstr(c: var NjvlContext; n: var Cursor) =
  n.into:
    let expected = n.childCursor # element type of the array
    skip n # type
    while n.hasMore:
      checkNilMatch c, n, expected
      skip n

proc analyseTupConstr(c: var NjvlContext; n: var Cursor) =
  n.into:
    var expected = n.childCursor # type of the first field
    skip n # type
    while n.hasMore:
      assert expected.hasMore
      let fieldType = getTupleFieldType(expected)
      var val = n
      if val.substructureKind == KvU:
        inc val # skip kv tag
        skip val # skip field name
      checkNilMatch c, val, fieldType
      skip n
      skip expected # type of the next field

proc traverseExpr(c: var NjvlContext; pc: var Cursor) =
  case pc.kind
  of Symbol:
    let symId = pc.symId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {VarY, LetY, CursorY, PatternvarY, ResultY}:
      if c.tr.live and not isInitialized(c, symId):
        buildErr(c, pc.info, "cannot prove that " & pool.syms[symId] & " has been initialized")
        # don't report the same symbol twice from later references
        markInit(c, symId)
    inc pc
  of SymbolDef:
    # SymbolDef can appear inside type expressions embedded in expressions
    # (e.g., `proc(x: int)` within `seq[proc(x: int)]` in `@[]`). The NJVL
    # converter passes them through; simply skip them here.
    inc pc
  of UnknownToken, EofToken, ParLe, ParRi, ExtendedSuffix, LineInfoLit, DotToken, Ident, StrLit, CharLit, IntLit, UIntLit, FloatLit:
    inc pc
  of TagLit:
    case pc.exprKind
    of CallKinds:
      analyseCall c, pc
    of DotX:
      pc.into:
        traverseExpr c, pc # object
        skip pc # field name
        if pc.hasMore: skip pc # inheritance depth
        if pc.hasMore: skip pc # optional access-token string lit
    of DdotX:
      pc.into:
        wantNotNilDeref c, pc
        traverseExpr c, pc # object
        skip pc # field name
        if pc.hasMore: skip pc # inheritance depth
        if pc.hasMore: skip pc # optional access-token string lit
    of DerefX:
      pc.into:
        wantNotNilDeref c, pc
        traverseExpr c, pc
    of OconstrX, NewobjX:
      analyseOconstr c, pc
    of AconstrX:
      analyseArrayConstr c, pc
    of TupconstrX:
      analyseTupConstr c, pc
    of CastX, ConvX, HconvX:
      let isCast = pc.exprKind == CastX
      pc.into:
        let convType = pc
        skip pc # skips type
        # A checked conversion to a `range[lo..hi]` carries the same obligation
        # as an assignment. `cast` is an unchecked escape hatch and is exempt.
        if not isCast:
          checkRangeAssign c, convType, pc
        traverseExpr c, pc
    of NilX:
      # `(nil)` / `(nil <Type>)` / `(nil <Type> <arg>)` — nil literal,
      # possibly carrying its formal type subtree (which for itertype /
      # closure-proctype contains raw param SymbolDefs that the generic
      # expression walk would mis-classify). Nothing in here can hold
      # free variables we'd want to track, so skip the whole subtree.
      skip pc
    else:
      pc.loopInto:
        traverseExpr c, pc
  else:
    inc pc  # ParRi/close (classic) or stray suffix (nifcore)

proc borrowCheckForCall(c: var NjvlContext; args: Cursor) =
  var mutPaths: seq[BorrowInfo] = @[]
  var immPaths: seq[BorrowInfo] = @[]
  var n = args
  while n.hasMore:
    let isMut = n.exprKind == HaddrX
    # Validate borrowable path for haddr arguments (call-scoped borrows)
    var inner = n
    if isMut:
      inner = n.childCursor
      let m = extractPath(c, inner)
      if m.mode == NotBorrowable:
        buildErr c, n.info, "cannot borrow from '" & asNimCode(inner) &
          "': path is not borrowable; use 'addr' to override or a temporary move"
      else:
        mutPaths.add m
    else:
      let m = extractPath(c, n, followInlineVars = false)
      if m.mode in {IsBorrowable, IsBorrowableFromGlobal}:
        immPaths.add m

    skip n
  # Check aliasing: a mutable argument must not overlap with any other argument:
  for i in 0 ..< mutPaths.len:
    for j in 0 ..< immPaths.len:
      if pathsOverlap(mutPaths[i], immPaths[j]):
        when false:
          echo "mutPaths[i]: ", mutPaths[i]
          echo "immPaths[j]: ", immPaths[j]
        buildErr c, mutPaths[i].info, "mutable argument aliases with immutable parameter"
        break
  # Mutable argument must not overlap with any other mutable argument:
  for i in 0 ..< mutPaths.len:
    for j in 0 ..< mutPaths.len:
      if i != j and pathsOverlap(mutPaths[i], mutPaths[j]):
        when false:
          echo "mutPaths[i]: ", mutPaths[i]
          echo "mutPaths[j]: ", mutPaths[j]
        buildErr c, mutPaths[i].info, "mutable argument aliases with mutable parameter"
        break

proc analyseCallArgs(c: var NjvlContext; n: var Cursor) =
  let callCursor = n
  let tt = getType(c.typeCache, n)
  let calleeKind = tt.stmtKind
  var fnType = skipProcTypeToParams(tt)
  var fnPragmas = fnType
  skip fnPragmas # params
  skip fnPragmas # return type
  let effect = whichEffect(calleeKind, fnPragmas)
  traverseExpr c, n # the `fn` itself
  assert fnType.isParamsTag
  let paramsStart = fnType
  fnType = sub(fnType)
  var paramMap = initTable[SymId, int]()
  # Collect argument paths for aliasing check
  let args = n
  var needsBorrowCheck = false
  while n.hasMore:
    if not fnType.hasMore:
      # All formal params consumed but args remain (e.g. varargs that were
      # consumed without a matching VarargsT param, or similar edge cases).
      # Traverse remaining args for their side effects.
      while n.hasMore:
        traverseExpr c, n
      break
    let previousFormalParam = fnType
    let param = takeLocal(fnType, SkipFinalParRi)
    paramMap[param.name.symId] = paramMap.len+1
    let pk = param.typ.typeKind
    # Save arg info before traverseExpr advances n
    let isMut = n.exprKind == HaddrX
    # Validate borrowable path for haddr arguments (call-scoped borrows)
    if isMut:
      var inner = n
      inc inner # skip haddr tag
      needsBorrowCheck = true
    if pk == OutT:
      let s = extractSymId(n)
      if s != NoSymId:
        markInit(c, s)
    elif pk == VarargsT:
      fnType = previousFormalParam
    checkNilMatch c, n, param.typ
    traverseExpr c, n
  if needsBorrowCheck:
    borrowCheckForCall c, args
  while fnType.hasMore: skip fnType
  fnType = paramsStart; skip fnType
  skip fnType # skip return type
  # now we have the pragmas:
  let req = extractPragma(fnType, RequiresP)
  if not cursorIsNil(req):
    let res = checkReq(c, paramMap, req, callCursor)
    when isMainModule:
      if res != Proven:
        error "contract violation: ", req

proc analyseCall(c: var NjvlContext; n: var Cursor) =
  # A `{.noreturn.}` callee (e.g. `quit`, an out-of-range raiser) does not fall
  # through. Mark the path dead after it, so a sibling branch that assigns
  # `result` is correctly seen as the only way out (matches nj.nim, which emits
  # a leave after noreturn calls). The init-set on this dead path contributes to
  # no exit, exactly as a `raise`/`return` would.
  var isNoReturn = false
  n.into: # call instruction
    block:
      var pragmas = skipProcTypeToParams(getType(c.typeCache, n))
      if pragmas.isParamsTag:
        skip pragmas # params
        skip pragmas # return type
        isNoReturn = hasPragma(pragmas, NoreturnP)
    analyseCallArgs(c, n)
  if isNoReturn:
    c.tr.live = false

# --- Assignment fact tracking ---

proc addAsgnFact(c: var NjvlContext; fact: LeXplusC) =
  if fact.isValid:
    c.facts.add fact
    c.facts.add fact.geXplusC

proc cannotBeNil(c: var NjvlContext; n: Cursor): bool {.inline.} =
  let t = getType(c.typeCache, n)
  result = markedAs(t, NotnilU) or isNonNilExpr(c, n)

# --- NJVL-specific traversal ---

proc traverseStore(c: var NjvlContext; n: var Cursor) =
  ## Handle (store value dest) - note reversed order from asgn
  let storeStart = n # skip store tag
  n = sub(n)

  # First analyze the value (source)
  let valueStart = n
  traverseExpr c, n

  # Check borrow conflicts for the destination
  let destMutPath = extractPath(c, n)
  if destMutPath.mode in {IsBorrowable, IsBorrowableFromGlobal}:
    checkBorrowConflict(c, destMutPath, n.info)

  # Now handle the destination (Symbol or NJVL versioned variable (v symId version))
  let destSymId = extractSymIdForStore(n)
  if destSymId != NoSymId:
    let symId = destSymId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {LetY, GletY, TletY}:
      if isInitialized(c, symId):
        c.buildErr n.info, "invalid reassignment to `let` variable"

    var fact = query(getVarId(c, symId), InvalidVarId, createXint(0'i32))
    markInit(c, symId)

    # Check for not-nil type match
    let expected = getType(c.typeCache, n)
    checkNilMatch c, valueStart, expected
    checkRangeAssign c, expected, valueStart

    # Try to extract facts from the value
    var valueForFact = valueStart
    if rightHandSide(c, valueForFact, fact):
      if fact.a == fact.b:
        variableChangedByDiff(c.facts, fact.a, fact.c)
      else:
        invalidateFactsAbout(c.facts, fact.a)
        addAsgnFact c, fact
    else:
      invalidateFactsAbout(c.facts, fact.a)

    # Check if the rhs is known to be not nil
    if (valueStart.exprKind == NewobjX and c.procCanRaise) or cannotBeNil(c, valueStart):
      c.facts.add isNotNil(fact.a)
    else:
      # Also check: the destination type might have notnil (e.g. proctype)
      if markedAs(expected, NotnilU):
        # The nil-match check already passed, so the value IS non-nil
        c.facts.add isNotNil(fact.a)

    # The (re)assigned location again holds an in-range value; the fact
    # bookkeeping above may have invalidated its range facts, so restore them.
    seedRangeFacts c, symId, expected

    skip n
  else:
    checkRangeAssign c, getType(c.typeCache, n), valueStart
    traverseExpr c, n

  n = storeStart; skip n

# --- Exit-summary plumbing (drives the journaled FlowTracker over c.flow) ---
#
# Every leave/bind/branch operation now goes through the `FlowTracker`, which
# journals the whole `FlowState` (init-set + facts) in place: a leave snapshots
# the state at its exit key, a bind joins the accumulated exit state back into
# fall-through, and an `ite`/`case`/`try` checkpoints once and rolls back rather
# than copying the state per branch.

proc leaveToLabel(c: var NjvlContext; label: SymId) = gotoLabel(c.tr, c.flow, label)
proc leaveToReturn(c: var NjvlContext) = gotoReturn(c.tr, c.flow)
proc leaveToRaise(c: var NjvlContext) = gotoRaise(c.tr, c.flow)
proc leaveToContinue(c: var NjvlContext) = gotoContinue(c.tr, c.flow)

proc traverseIte(c: var NjvlContext; n: var Cursor) =
  ## `(ite cond then else)`. Each arm is analyzed under the condition's polarity;
  ## the tracker merges the fall-through state (inits + facts) by liveness — a
  ## branch that always leaves drops out, unifying guard-clause and if-else
  ## style. The state is journaled, so a branch costs O(writes), not a copy.
  let iteStart = n # skip ite/itec tag
  n = sub(n)

  let savedBorrowsLen = c.activeBorrows.len
  # Split BEFORE the condition facts so they belong to the then-branch's delta
  # (the then-branch runs under `assume(cond)`); `commitThen` rolls them back.
  var b = splitBranch(c.tr, c.flow)
  let condFacts = analyseCondition(c, n)

  # Single-fact conditions can be negated for the else-branch's `assume(¬c)`.
  var condFactsList: seq[LeXplusC] = @[]
  if condFacts == 1:
    condFactsList.add c.facts[c.facts.len - 1]

  # then-branch (under assume(c)):
  traverseStmt c, n
  # `commitThen` captures the then-branch (its init delta, facts, and exits) and
  # rolls `c.flow` back to the split baseline — which drops the condition facts,
  # since the split was taken before them.
  commitThen(c.tr, c.flow, b)
  c.activeBorrows.setLen(savedBorrowsLen)

  # else-branch (under assume(¬c)):
  for f in condFactsList:
    var negated = f
    negateFact(negated)
    c.facts.add negated
  if n.isDotToken:
    inc n
  else:
    traverseStmt c, n
  # `mergeBranches` joins the then-branch (in `b`) with the current else-branch,
  # merging both the init-set and the facts; a leaving arm drops out.
  mergeBranches(c.tr, c.flow, b)
  c.activeBorrows.setLen(savedBorrowsLen)

  n = iteStart; skip n

proc traverseLoop(c: var NjvlContext; n: var Cursor) =
  ## `(loop body)` — infinite; the body ends in `(continue .)` and exits
  ## forward via `(jmp loopExit)`. The while-condition is the leading guard
  ## `(ite (not cond) (jmp loopExit) .)` *inside* the body, so it needs no
  ## special handling here. Iteration-gained facts/inits flow only to the
  ## break sites (captured) and to the back-edge (discarded); the loop never
  ## falls through. The `(lab loopExit)` that follows installs the merged
  ## break state via `bindLoopExit`.
  n.into: # loop tag
    let cp = c.flow.checkpoint()
    let savedBorrows = c.activeBorrows.len
    traverseStmt c, n        # the body `(stmts ...)`; ends by leaving
    dropContinue(c.tr)       # the loop header consumes the back-edge
    # The loop never falls through; reset the working state to the pre-loop base.
    # A following `(lab loopExit)` keeps these pre-loop facts (break-site facts are
    # iteration-specific and dropped; break-site inits are joined — see
    # `bindLoopExit`), which is sound: a loop proves nothing new about the facts of
    # its mutated vars afterwards.
    c.flow.rollbackTo cp
    # Borrows taken *inside* the body are loop-local (a `var p = addr coll[i]`
    # cannot outlive the iteration), so drop them — otherwise a later mutation of
    # the borrowed container after the loop is wrongly seen as still-borrowed.
    c.activeBorrows.setLen(savedBorrows)
  # The trailing `(lab loopExit)` (emitted iff a `break`/guard targeted it) is
  # *this* loop's exit. Record it so `traverseLabel` uses `bindLoopExit`.
  if n.isTagLit and n.njvlKind == LabV:
    var peek = n
    inc peek
    c.loopExitLabels.incl peek.symId

proc traverseLabel(c: var NjvlContext; n: var Cursor) =
  ## `(lab L)` — the multi-join. Every forward `jmp L` has already been seen.
  var label = NoSymId
  n.into:
    label = n.symId
    inc n # symdef
  if c.loopExitLabels.contains(label):
    bindLoopExit(c.tr, c.flow, label)
  else:
    bindLabel(c.tr, c.flow, label)

proc traverseJmp(c: var NjvlContext; n: var Cursor) =
  ## `(jmp L)` — a forward structural transfer (loop-`break` included).
  var label = NoSymId
  n.into:
    label = n.symId
    inc n # symuse
  leaveToLabel(c, label)

proc traverseRet(c: var NjvlContext; n: var Cursor) =
  ## `(ret .X)` — primitive return, bound by the proc root. A `return value`
  ## with a non-`result` operand *provides* the result directly (the NJVL path
  ## rewrote this to `result = value`), so it initializes `result` on this exit.
  n.into:
    if n.isDotToken:
      inc n
    else:
      let providesResult = c.resultSym != NoSymId and
        not (n.isSymbol and n.symId == c.resultSym)
      traverseExpr c, n
      if providesResult:
        markInit(c, c.resultSym)
  leaveToReturn(c)

proc traverseRaise(c: var NjvlContext; n: var Cursor) =
  ## `(raise .X)` — primitive raise, bound by the nearest enclosing `except`.
  n.into:
    if n.isDotToken:
      inc n # bare re-raise
    else:
      traverseExpr c, n
  leaveToRaise(c)

proc addCaseFacts(c: var NjvlContext; selSym: SymId; ranges: Cursor) =
  ## Inside an `of` branch the selector is known to lie in `ranges`. When the
  ## branch lists exactly one value/range and the selector is a plain variable,
  ## add the corresponding bound facts (`sel == v`, or `lo <= sel <= hi`).
  if selSym == NoSymId or ranges.substructureKind != RangesU: return
  var r = ranges
  r = sub(r) # into 'ranges'; peek only, never left
  var cnt = 0
  var first = r
  while r.hasMore:
    inc cnt
    skip r
  if cnt != 1: return # a disjunction of values yields no single bound fact
  let a = getVarId(c, selSym)
  r = first
  if r.substructureKind == RangeU:
    inc r
    if r.isIntLit:
      var lo = query(a, VarId(0), createXint(pool.integers[r.intId]))
      c.facts.add lo.geXplusC # sel >= lo
    skip r
    if r.isIntLit:
      c.facts.add query(a, VarId(0), createXint(pool.integers[r.intId])) # sel <= hi
  elif r.isIntLit:
    var f = query(a, VarId(0), createXint(pool.integers[r.intId]))
    c.facts.add f            # sel <= v
    c.facts.add f.geXplusC   # sel >= v

proc traverseCase(c: var NjvlContext; n: var Cursor) =
  ## `(case selector (of (ranges...) body)+ (else body)?)`. An N-way merge:
  ## every branch starts from the pre-case state, plus the bound facts implied
  ## by its `ranges`; the post-case fall-through is the intersection of the
  ## init-sets (and a fact-join) over the arms that fall through.
  let caseStart = n # skip 'case'
  n = sub(n)
  let selCursor = n
  let selSym = extractSymId(selCursor)
  traverseExpr c, n # selector (init-checked)

  # Collect (ranges, body) per branch, walking past the whole case.
  var branches: seq[tuple[ranges, body: Cursor]] = @[]
  while n.substructureKind == OfU:
    n.into: # 'of'
      let ranges = n
      skip n         # ranges
      branches.add (ranges, n)
      skip n         # body
  if n.substructureKind == ElseU:
    n.into:
      branches.add (default(Cursor), n)
      skip n
  n = caseStart; skip n # close 'case'

  let cp = c.flow.checkpoint()
  let savedBorrows = c.activeBorrows.len
  let baseLive = c.tr.live

  var merged = default(FlowSnap)   # join of the fall-through arms (see joinSnap)
  var haveMerged = false

  for br in branches:
    # Each branch resumes from the pre-case state (the selector chose this arm).
    c.flow.rollbackTo cp
    c.tr.live = baseLive
    c.activeBorrows.setLen(savedBorrows)
    if not cursorIsNil(br.ranges):
      addCaseFacts(c, selSym, br.ranges)
    var bc = br.body
    traverseStmt c, bc
    if c.tr.live:
      merged = if haveMerged: joinSnap(merged, snapshot(c.flow)) else: snapshot(c.flow)
      haveMerged = true

  # A case with no `else` is exhaustive (sem guarantees this), so the selector
  # always matches some branch — there is no implicit fall-through to add.
  if haveMerged:
    c.tr.live = true
    setTo(c.flow, cp, merged)
  else:
    c.tr.live = false
    c.flow.rollbackTo cp
  c.activeBorrows.setLen(savedBorrows)

proc traverseTry(c: var NjvlContext; n: var Cursor) =
  ## `(try body (except ...)* (fin ...)?)`. Conservative: an `except` handler
  ## may run after *any* point of the body, so it can only assume the pre-try
  ## state; a `fin` is analyzed on the merged fall-through (its inits are not
  ## propagated onto exit paths — sound, since that only withholds knowledge).
  let tryStart = n # skip 'try'
  n = sub(n)
  let cp = c.flow.checkpoint()
  let savedBorrows = c.activeBorrows.len
  let baseLive = c.tr.live

  traverseStmt c, n # try body

  var merged = default(FlowSnap)   # join of the fall-through of body + handlers
  var haveMerged = false
  if c.tr.live:
    merged = snapshot(c.flow); haveMerged = true

  if n.substructureKind == ExceptU:
    # The excepts catch the body's raises.
    discard takeRaise(c.tr)

  while n.substructureKind == ExceptU:
    n.into: # 'except'
      var boundExc = NoSymId
      while n.hasMore and n.stmtKind notin {StmtsS, ScopeS}:
        if isLocal(n.symKind):
          let local = asLocal(n)
          c.typeCache.registerLocal(local.name.symId, n.symKind, local.typ)
          boundExc = local.name.symId
        skip n
      # handler entry = pre-try state (a raise may interrupt the body anywhere):
      c.flow.rollbackTo cp
      c.tr.live = baseLive
      c.activeBorrows.setLen(savedBorrows)
      # The bound exception value is initialized *in the handler* — mark it after
      # resetting to the pre-try state, which would otherwise discard the init.
      if boundExc != NoSymId:
        markInit(c, boundExc)
      if n.stmtKind in {StmtsS, ScopeS}:
        traverseStmt c, n
      if c.tr.live:
        merged = if haveMerged: joinSnap(merged, snapshot(c.flow)) else: snapshot(c.flow)
        haveMerged = true

  if haveMerged:
    c.tr.live = true
    setTo(c.flow, cp, merged)
  else:
    c.tr.live = false
    c.flow.rollbackTo cp
  c.activeBorrows.setLen(savedBorrows)

  if n.substructureKind == FinU:
    n.into:
      traverseStmt c, n # finally body, on the merged fall-through
  n = tryStart; skip n # close 'try'

proc traverseLocal(c: var NjvlContext; n: var Cursor) =
  let kind = n.symKind
  let localStart = n
  n = sub(n)
  let name = n.symId
  skip n # name
  skip n # export marker
  let skipInitCheck = hasPragma(n, NoinitP)
  let isInline = hasPragma(n, InlineP)
  skip n # pragmas
  c.typeCache.registerLocal(name, kind, n)
  let localType = n
  skip n # type
  if not n.isDotToken or skipInitCheck:
    markInit(c, name)
  if kind == ResultY:
    c.resultSym = name
  if isInline:
    c.inlineVars[name] = n
  # Detect borrow: (haddr X) as init expression starts a borrow.
  # Validate that the path is borrowable (no deref in the middle, no calls).
  # Explicit `addr` in the path is an escape hatch ("unchecked").
  if n.isTagLit and n.exprKind == HaddrX:
    var inner = n
    inc inner # skip haddr tag
    var path = extractPath(c, inner)
    if path.mode in {IsBorrowable, IsBorrowableFromGlobal}:
      path.borrower = name
      c.activeBorrows.add path
    elif path.mode == NotBorrowable:
      buildErr c, n.info, "cannot borrow from '" & asNimCode(inner) &
        "': path is not borrowable; use 'addr' to override or a temporary move"
  if not n.isDotToken and localType.typeKind in {PtrT, RefT, CstringT, PointerT, ProctypeT}:
    checkNilMatch c, n, localType
  if not n.isDotToken:
    checkRangeAssign c, localType, n
  traverseExpr c, n
  n = localStart; skip n
  # The local now holds a value proven to be within its range (if any), so
  # record that for downstream obligations that reference this symbol.
  seedRangeFacts c, name, localType

proc traverseAssume(c: var NjvlContext; n: var Cursor) =
  n.into:
    var wasEquality = false
    let fact = translateCond(c, n, wasEquality)
    if not fact.isValid:
      error "invalid assume: ", n
    else:
      c.facts.add fact
      if wasEquality:
        c.facts.add fact.geXplusC

proc traverseAssert(c: var NjvlContext; n: var Cursor) =
  let orig = n
  n.into:
    var report = false
    var shouldError = false
    if n.pragmaKind == ReportP:
      report = true
      skip n
    if n.pragmaKind == ErrorP:
      shouldError = true
      skip n

    var wasEquality = false
    let fact = translateCond(c, n, wasEquality)
    if not fact.isValid:
      error "invalid assert: ", orig
    elif implies(c.facts, fact):
      if shouldError:
        contractViolation(c, orig, fact, report)
      elif wasEquality:
        if implies(c.facts, fact.geXplusC):
          if report: echo "OK ", $fact
        else:
          if shouldError:
            if report: echo "OK (could indeed not prove) ", $fact
          else:
            contractViolation(c, orig, fact, report)
      else:
        if report: echo "OK ", $fact
    else:
      if shouldError:
        if report: echo "OK (could indeed not prove) ", $fact
      else:
        contractViolation(c, orig, fact, report)

proc traverseProc(c: var NjvlContext; n: var Cursor) =
  let decl = n
  # Fresh, journaling flow state (init-set + facts) for this proc; the enclosing
  # proc's state (with its live checkpoints) is restored on the way out.
  let oldFlow = move c.flow
  c.flow = initFlowState()
  c.procCanRaise = false
  let oldTr = move c.tr
  c.tr = initFlowTracker()
  # Seed with the enclosing init-set ONLY for genuinely nested procs (closures),
  # so a captured outer local stays initialized inside the closure body. A
  # top-level proc must NOT inherit the whole module-level init-set: those syms
  # are globals/consts that are never init-checked. `nestedProcs >= 2` means
  # "inside another proc's body".
  if c.nestedProcs >= 2:
    inheritInits(c.flow, oldFlow)
  let oldResultSym = c.resultSym
  let oldInlineVars = move c.inlineVars
  let oldBorrows = move c.activeBorrows
  let oldProcStart = c.currentProcStart
  c.currentProcStart = decl
  c.resultSym = NoSymId
  let procStart = n
  n = sub(n)
  let symId = n.symId
  var isGeneric = false
  var isExternProc = false
  var outParams: seq[SymId] = @[]
  for i in 0 ..< BodyPos:
    if i == ProcPragmasPos:
      c.procCanRaise = hasPragma(n, RaisesP)
      isExternProc = hasPragma(n, ImportcP) or hasPragma(n, ImportcppP)
    elif i == TypevarsPos:
      isGeneric = n.substructureKind == TypevarsU
    elif i == ParamsPos:
      if n.isTagLit:
        var p = n
        p = sub(p) # peek only, never left
        while p.hasMore:
          let r = takeLocal(p, SkipFinalParRi)
          c.typeCache.registerLocal(r.name.symId, ParamY, r.typ)
          if r.typ.typeKind == OutT and not hasPragma(r.pragmas, NoinitP):
            outParams.add r.name.symId
          # A `range[lo..hi]`-typed parameter is known to be within bounds.
          seedRangeFacts c, r.name.symId, r.typ
      c.typeCache.registerLocal(symId, ProcY, decl)
    skip n

  # Analyze body. Generic procs are only checked once instantiated. Extern
  # (importc/importcpp) procs satisfy their contract at the C level and have no
  # meaningful Nim body — and the lowered body of an extern func with a doc /
  # `runnableExamples` body still ends in an implicit `(ret result)` that reads
  # the never-initialized `result`, so we must skip the *traversal*, not merely
  # the final init check.
  if not isGeneric and not isExternProc:
    traverseStmt c, n
    # Join every `return` into the natural fall-through: the result init-set at
    # proc exit is the intersection over all exit paths. The init-check below
    # then reads `c.flow.inits` — `result`/out-params must be init on every path
    # that leaves the proc.
    bindReturn(c.tr, c.flow)
    let info = decl.info
    # Only when control can actually leave the proc *normally* (fall-through or a
    # `return`) must `result`/out-params be initialized. A proc whose every path
    # raises or otherwise never returns (`c.tr.live == false` here) has no normal
    # exit, so the init obligation is vacuous — e.g. `proc f: string = raise X`.
    if c.tr.live:
      if c.resultSym != NoSymId and not isInitialized(c, c.resultSym):
        buildErr c, info, "cannot prove that " & pool.syms[c.resultSym] & " has been initialized"
      for sym in outParams:
        if not isInitialized(c, sym):
          buildErr c, info, "cannot prove that " & pool.syms[sym] & " has been initialized"
  else:
    skip n
  n = procStart; skip n
  c.tr = ensureMove oldTr
  c.flow = ensureMove oldFlow
  c.resultSym = oldResultSym
  c.inlineVars = ensureMove oldInlineVars
  c.activeBorrows = ensureMove oldBorrows
  c.currentProcStart = oldProcStart

proc traverseStmt(c: var NjvlContext; n: var Cursor) =
  case n.njvlKind
  of IteV, ItecV:
    traverseIte c, n
  of LoopV:
    traverseLoop c, n
  of StoreV:
    traverseStore c, n
  of AssumeV:
    traverseAssume c, n
  of AssertV:
    traverseAssert c, n
  of LabV:
    traverseLabel c, n
  of JmpV:
    traverseJmp c, n
  of MflagV, VflagV:
    # A control-flow flag declaration may still arrive from xelim; the bool
    # storage is harmless. Register it so its later use is not flagged.
    var s = NoSymId
    n.into:
      s = n.symId
      skip n # symdef
    markInit(c, s)
  of JtrueV:
    # Final IR has no `jtrue`; if one survives from xelim, it is inert here.
    skip n
  of KillV:
    # Variable going out of scope - end any active borrows
    n.into:
      while n.hasMore:
        let s = extractSymId(n)
        if s != NoSymId:
          endBorrow(c, s)
        skip n
  of UnknownV:
    # Unknown instruction - variable's contents become unknown after a call.
    # Check borrow conflicts: passing a borrowed path to a var param is a mutation.
    n.into:
      let unknownPath = extractPath(c, n)
      if unknownPath.mode in {IsBorrowable, IsBorrowableFromGlobal}:
        checkBorrowConflict(c, unknownPath, n.info)
      # The location's contents are now unknown: every fact we knew about it is
      # stale. Dropping them is what makes e.g. `move(a)` correctly forget the
      # `a != nil` proof — `a` was passed by `haddr` and reset to a moved-from
      # (nil) state, so a later `a.x` must be re-proven, not silently accepted.
      # Facts are keyed per root variable (see `analysableRoot`), so we invalidate
      # by the path's root symbol.
      if unknownPath.path.len > 0:
        invalidateFactsAbout(c.facts, getVarId(c, unknownPath.path[0]))
      skip n # the unknown location
  of ContinueV:
    # The loop back-edge.
    skip n
    leaveToContinue(c)
  of VV:
    # Versioned variable reference - should not appear as statement
    skip n
  of EtupatV:
    traverseExpr c, n
  of NoVTag:
    case n.stmtKind
    of StmtsS, ScopeS, BlockS:
      n.into:
        while n.hasMore:
          traverseStmt c, n
    of CaseS:
      traverseCase c, n
    of TryS:
      traverseTry c, n
    of RetS:
      traverseRet c, n
    of RaiseS:
      traverseRaise c, n
    of LocalDecls:
      traverseLocal c, n
    of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS:
      # Nested routine - analyze and advance past it
      c.typeCache.openScope()
      inc c.nestedProcs
      traverseProc c, n
      dec c.nestedProcs
      c.typeCache.closeScope()
    of TemplateS, TypeS, CommentS, PragmasS:
      skip n
    of CallKindsS:
      analyseCall c, n
    of DiscardS, YldS:
      n.into:
        traverseExpr c, n
    of EmitS, InclS, ExclS:
      skip n
    of PragmaxS:
      n.into:
        skip n # pragmas
        while n.hasMore:
          traverseStmt c, n
    of NoStmt:
      if n.exprKind in CallKinds:
        analyseCall c, n
      elif n.exprKind == PragmaxX:
        n.into:
          skip n # pragmas
          while n.hasMore:
            traverseStmt c, n
      elif n.exprKind in {DestroyX, CopyX, WasmovedX, SinkhX, TraceX}:
        n.into:
          traverseExpr c, n
          while n.hasMore:
            traverseExpr c, n
      else:
        traverseExpr c, n
    else:
      # Unknown statement - skip it wholesale
      skip n

proc traverseToplevel(c: var NjvlContext; n: var Cursor) =
  case n.stmtKind
  of StmtsS:
    n.into:
      while n.hasMore:
        traverseToplevel c, n
  of PragmaxS:
    n.into:
      skip n # pragmas
      # A pragma block (e.g. `{.cast(uncheckedAccess).}:`) carries a whole body,
      # not a single statement — traverse every child before closing, as the
      # non-toplevel `traverseStmt` already does.
      while n.hasMore:
        traverseToplevel c, n
  of ProcS, FuncS, IteratorS, ConverterS, MethodS:
    inc c.nestedProcs
    traverseProc c, n
    dec c.nestedProcs
  of MacroS, TemplateS, TypeS, CommentS, PragmasS,
     ImportasS, ExportexceptS, BindS, MixinS, UsingS,
     ExportS,
     IncludeS, ImportS, FromimportS, ImportexceptS:
    skip n
  else:
    # Toplevel statements - analyze them
    traverseStmt c, n

proc lowerToFinalIr(input: var TokenBuf; moduleSuffix: string): TokenBuf =
  ## Run the Final-IR lowering (`finalir.nim`, which itself runs xelim first).
  var n = beginRead(input)
  var buf = createTokenBuf(input.len)
  buf.addSubtree n
  var pass = initPass(move buf, moduleSuffix, "xelim_finalir", 0)
  toFinalIr(pass)
  result = ensureMove pass.dest

proc analyzeContractsFinalIr*(input: var TokenBuf; moduleSuffix: string; verbose = false): TokenBuf =
  ## Main entry point: lowers `input` to the Final IR and analyzes contracts.
  ## When `verbose` is true, every contract/init failure dumps the enclosing
  ## proc's IR to stderr to aid debugging.
  var finalBuf = lowerToFinalIr(input, moduleSuffix)

  var c = NjvlContext(
    typeCache: createTypeCache(),
    moduleSuffix: moduleSuffix,
    tr: initFlowTracker(),
    flow: initFlowState(),
    loopExitLabels: initHashSet[SymId](),
    verbose: verbose
  )
  c.typeCache.openScope()

  var fin = beginRead(finalBuf)
  traverseToplevel c, fin

  c.typeCache.closeScope()
  result = ensureMove c.errors

when isMainModule:
  import std / [syncio, os]
  proc main(infile: string) =
    var input = parseFromFile(infile)
    discard analyzeContractsFinalIr(input, "main")

  main(paramStr(1))

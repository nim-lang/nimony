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

"Did we already leave" is now positional: the `Tracker` (`finalir/tracker.nim`)
carries fall-through reachability and the per-target exit summaries, and a
`(lab)` multi-join resolves them in one forward pass. Per the chosen design the
state is *hybrid*: `inferle` facts stay imperative (`save`/`restore` at branch
points, snapshotted per-exit for the multi-join), while the **Tracker** owns
init-tracking and fall-through.

A `.requires` is discharged at the **call site**: the contract is read with each
parameter standing for the argument written there, and handed to the same
`inferle` fact base every other obligation uses. Three answers are possible and
only the middle one is silent — proven, undecided (the run-time guard `hexer`
emits into the callee stays), and violated, which is an error. See
`checkRequires` for why undecided is not an error by default and for the two
module pragmas that move that line.

Compile `nimsem` with `-d:contractStats` to get one
`CONTRACT <verdict> <line> <contract>` line per call site on stderr; that is how
the prover's coverage is measured. Adding `-d:contractLeaves` breaks each site
down per conjunct.
]##

import std / [assertions, tables, hashes, sets, strutils, syncio]

include ".." / lib / nifprelude
include ".." / lib / compat2

import ".." / models / tags
import ".." / lib / symparser
import ".." / finalir / [finalir_model, finalir]
import flowtracker
import ".." / hexer / passes
import nimony_model, programs, decls, typenav, sembasics, reporters,
  renderer, typeprops, inferle, xints, builtintypes, features, expreval

type
  BorrowableCheck = enum
    IsBorrowable       ## simple path: symbols, dots, array access
    IsBorrowableFromConst
    IsBorrowableFromGlobal
    HasAddr            ## path contains explicit `addr` — unsafe escape hatch
    NotBorrowable      ## deref in middle of path or function call

  IvKind = enum
    ivUnknown      ## an arbitrary write: nothing known about it survives a loop
    ivIncreasing   ## every write in the loop moves it up (or leaves it alone)
    ivDecreasing   ## every write in the loop moves it down
    ivNonNegative  ## not monotone, but every write keeps it `>= 0`: `y = y shr 1`
                   ## and `y = y and mask` are the halving/masking steps a
                   ## bit-walking loop is made of, and `0 <= y` is exactly the
                   ## invariant such a loop needs and no more

  StepInfo = object
    ## A routine whose whole body is `x = x ± k` for its first, `var`,
    ## parameter — `inc` and `dec` are the ones that matter.
    param: SymId
    isSub: bool
    stepSym: SymId   ## the step is this second parameter …
    stepLit: xint    ## … or, when `stepSym` is `NoSymId`, this literal

  RangeBounds = object
    lo, hi: xint

  AccessorInfo = object
    ## A *transparent accessor*: a one-parameter routine whose whole body is
    ## `result = <expr>`. `value` is that expression, written in terms of
    ## `param`, and is what a call to the routine is keyed as.
    param: SymId
    value: Cursor

  BorrowInfo = object
    borrower: SymId   ## variable holding the borrow; upon `(kill borrower)` the borrow ends
    mode: BorrowableCheck
    path: seq[SymId]  ## root :: field1 :: field2 :: ...
    info: NifLineInfo

  CallContext = object
    ## What the calls of one statement established about the locations they
    ## wrote. The lowering puts an `(unknown x)` after that statement for every
    ## argument passed by address, and those statements read this rather than
    ## forgetting everything about `x`.
    stepped: VarId
      ## `inc i` moved `i` by an amount the call stated exactly: its facts were
      ## shifted, not lost.
    ensured: seq[SymId]
      ## Roots whose facts a callee's `.ensures` re-established after the call.
    views: seq[SymId]
      ## `var openArray` arguments: their elements are unknown now, never their
      ## length (`isVarOpenArray`).

  FirContext = object
    flow: FlowState                    # the journaled analysis state: the
                                       # definite-assignment init-set and the
                                       # `inferle` facts, mutated in place.
    typeCache: TypeCache
    tr: FlowTracker                    # control flow: fall-through liveness +
                                       # per-exit state accumulation (journaled).
    errors: TokenBuf
    procCanRaise: bool
    inHook: bool                       # inside a `=destroy`/`=wasMoved`/… body:
                                       # hooks operate on raw, possibly
                                       # moved-from memory, so a `notnil` field
                                       # may legitimately be set to `nil` there.
    features: set[Feature]
    moduleSuffix: string
    nestedProcs: int
    loopExitLabels: HashSet[SymId]     # `(lab)`s emitted right after a `(loop)`.
                                       # Their post-join state keeps the pre-loop
                                       # facts (break-site facts are dropped; only
                                       # break-site inits are joined). See
                                       # `bindLoopExit`.
    inlineVars: Table[SymId, Cursor] # var -> to its init expression
    derivedIds: Table[string, VarId]   # canonical location key -> derived VarId
                                       # (see "Derived locations" below)
    derivedKeys: seq[string]           # parallel to `derivedRoots`: the location key
    derivedRoots: seq[SymId]           # derived VarId -> the variable it hangs
                                       # off, which is what a write invalidates
    accessors: Table[SymId, AccessorInfo] # routines that are just `result = path`
    notAccessors: HashSet[SymId]       # negative cache for the lookup above
    constDepth: int
    moduleConsts: Table[SymId, Cursor] # `const`s declared in this module: their
                                       # values, which `tryEvalOrdinal` cannot
                                       # load before the module is written
    moduleTypes: Table[SymId, Cursor]  # `type`s declared in this module, which
                                       # `tryLoadSym` cannot load yet either
    moduleRoutines: Table[SymId, Cursor] # ... and the routines
    pointeeFns: Table[SymId, bool]     # memo for `returnsPointee`
    moduleFuncs: HashSet[SymId]        # `func`s declared in this module, generic
                                       # instances included (see `collectAccessors`)
    steps: Table[SymId, StepInfo]      # routines that are just `x = x ± k`
    notSteps: HashSet[SymId]           # negative cache for the lookup above
    prescanning: bool
      ## True while `scanLoopWrites` walks a loop body ahead of its traversal.
      ## The body's own locals are not registered with the type cache yet, so
      ## nothing may ask for a type during it.
    substVar: Table[SymId, VarId]
      ## While an `.ensures` is being read, the `result` it names stands for a
      ## `VarId` rather than for an expression: either the location the call was
      ## bound to, or the anonymous slot `checkRangeAssign` judges a call's
      ## value in.
    declaredRange: Table[VarId, RangeBounds]
      ## The bounds a location's `range` TYPE states. Unlike a flow fact this
      ## can never go stale — every write to the location owes the range in
      ## turn — so it answers questions the flow facts have been widened away
      ## from, which is what a `Natural` field advanced inside a loop needs.
    oldSlots: Table[string, VarId]
      ## `old(e)` in the `.ensures` of the call being analysed: the snapshot of
      ## location `e` taken before the call's mutations, keyed by `e`'s
      ## location key. Rebuilt at every call.
    ownEnsures: Cursor
      ## The `.ensures` of the routine being analysed, proven at every exit
      ## (`verifyEnsures`); nil when it has none.
    ownOldSlots: Table[string, VarId]
      ## `old(e)` of `ownEnsures`: the snapshots taken on entry.
    resultSym: SymId                   # symId of the `result` local for the current proc, or NoSymId
    activeBorrows: seq[BorrowInfo]
    verbose: bool                      # --verbose: dump final IR on init/contract
                                       # failures for easier debugging
    bits: int                          # target `int` width for compile-time folding
    currentProcStart: Cursor           # cursor at the start of the proc whose
                                       # body we are currently analysing (used
                                       # for the --verbose dump)

# `c.facts` reads/writes the fact set inside the journaled `FlowState`; the bulk
# of the pass mutates facts through this alias, so it stays spelled `c.facts`.
template facts(c: FirContext): untyped = c.flow.facts

proc markInit(c: var FirContext; symId: SymId) {.inline.} =
  c.flow.inits.incl symId

proc isInitialized(c: FirContext; symId: SymId): bool {.inline.} =
  symId in c.flow.inits

proc dumpCurrentProc(c: var FirContext; info: NifLineInfo; msg: string) =
  ## Dump the Final IR of the proc currently under analysis to stderr. Used
  ## by `--verbose` so the user can see the lowered form that caused
  ## a contract/init failure. Gated on `c.verbose` — callers still invoke
  ## it unconditionally; this proc is the single decision point.
  if not c.verbose: return
  if cursorIsNil(c.currentProcStart): return
  stderr.writeLine "--- Final IR (--verbose) for: " & msg
  stderr.writeLine "--- at " & infoToStr(info) & ":"
  stderr.writeLine toString(c.currentProcStart, false)
  stderr.writeLine "--- end Final IR dump ---"

proc buildErr(c: var FirContext; rawInfo: NifLineInfo; msg: string) =
  # This pass runs on the FINAL IR, where the node an error is pinned to is
  # often one the compiler synthesized -- the epilogue's `(ret result)` is what
  # an uninitialized `result` is reported at -- and those carry no line info.
  # Printed, such an error is a bare `???`; worse, `reporters` deduplicates by
  # line info, so the second and every later one in a module is swallowed and
  # the user fixes them one recompile at a time. Fall back to the enclosing
  # proc's declaration, which is where the reader has to look anyway.
  let info = if rawInfo.isValid or cursorIsNil(c.currentProcStart): rawInfo
             else: c.currentProcStart.info
  when defined(debug):
    writeStackTrace()
    echo infoToStr(info) & " Error: " & msg
    quit msg
  dumpCurrentProc(c, info, msg)
  var hintedMsg = msg
  if not c.verbose:
    hintedMsg.add " [pass --verbose for the Final IR]"
  c.errors.buildTree ErrT, info:
    c.errors.addDotToken()
    c.errors.addStrLit(hintedMsg, info)

# Forward declarations
proc traverseStmt(c: var FirContext; n: var Cursor; call: var CallContext)
proc traverseExpr(c: var FirContext; pc: var Cursor; call: var CallContext)
proc analyseCall(c: var FirContext; n: var Cursor; call: var CallContext)

proc freshCall(): CallContext {.inline.} = CallContext(stepped: InvalidVarId)

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
  if n.finalIrKind == EtupatV:
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

proc establishesBorrow(c: var FirContext; n: Cursor): bool =
  ## True if `n` is a call to a routine marked `.establishesBorrow.`, i.e. one
  ## whose result keeps aliasing its first argument after the call returns.
  ##
  ## Nothing in the callee's body can tell us this: a view constructor such as
  ## `toOpenArray` stores a raw pointer into the result, and the raw pointer is
  ## exactly where the path we could follow ends. So the annotation on the
  ## declaration is what carries the borrow across the call boundary.
  if not n.isTagLit or n.exprKind notin CallKinds: return false
  var fn = n
  inc fn # the callee
  var fnType = skipProcTypeToParams(getType(c.typeCache, fn))
  if not fnType.isParamsTag: return false
  skip fnType # params
  skip fnType # return type
  result = hasPragma(fnType, EstablishesBorrowP)

proc extractBorrowPath(c: var FirContext; n: Cursor; result: var BorrowInfo; followInlineVars=true) =
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
    elif n.finalIrKind == EtupatV:
      var r = n
      inc r
      extractBorrowPath(c, r, result, followInlineVars)
    elif n.finalIrKind == VV:
      extractBorrowPath(c, n.childCursor, result, followInlineVars)
  elif (n.isIntLit or n.isUIntLit or n.isCharLit or n.isFloatLit or n.isStringLit):
    result.mode = IsBorrowableFromConst
  elif n.isSymbol:
    let s = n.symId
    # A `.establishesBorrow.` call hoisted into an inline temp must be followed
    # even when `followInlineVars` is off: `f(toOpenArray(s), s)` reaches us as
    # `f(tmp, (haddr s))` and the alias is invisible unless we look through
    # `tmp`. The type of such a temp is the view, not `var`/`out`/`lent`, so the
    # modifier test below does not catch it.
    if s in c.inlineVars and (followInlineVars or
                              getType(c.typeCache, n).typeKind in {MutT, OutT, LentT} or
                              establishesBorrow(c, c.inlineVars.getOrQuit(s))):
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

proc extractPath(c: var FirContext; n: Cursor; followInlineVars=true): BorrowInfo =
  result = BorrowInfo(path: @[], mode: NotBorrowable, info: n.info)
  extractBorrowPath(c, n, result, followInlineVars)

proc `$`(b: BorrowInfo): string =
  result = "BorrowInfo(mode: " & $b.mode & ", path: "
  for i in 0 ..< b.path.len:
    result.add " :: " & pool.symString(b.path[i])
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

proc checkBorrowConflict(c: var FirContext; mutPath: BorrowInfo; info: NifLineInfo) =
  for b in c.activeBorrows:
    if pathsOverlap(mutPath, b):
      buildErr c, info, "'" & asNimCode(mutPath.path[0]) & "' is borrowed and cannot be mutated"
      return

proc localInfoOf(c: var FirContext; s: SymId): LocalInfo =
  ## `getLocalInfo` only knows the locals of the proc under analysis; a global
  ## carries a module suffix and has to be loaded from disk.
  result = getLocalInfo(c.typeCache, s)
  if result.kind == NoSym:
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      let l = asLocal(res.decl)
      result = LocalInfo(kind: l.kind, typ: l.typ, val: l.val)

proc diesWithProc(c: var FirContext; s: SymId): bool =
  ## Locals are gone once the proc returns; params, globals and consts are not.
  result = localInfoOf(c, s).kind in {VarY, LetY, CursorY}

proc outlivesProc(c: var FirContext; s: SymId): bool =
  ## Storing here makes the value observable after the proc has returned:
  ## `result` flows out through the return, a global simply stays, and a
  ## `var`/`out` param writes through to a location the caller owns.
  if s == c.resultSym: return true
  let x = localInfoOf(c, s)
  result = x.kind in {GvarY, TvarY, GletY, TletY} or
           (x.kind == ParamY and x.typ.typeKind in {MutT, OutT})

proc borrowCarriedBy(c: var FirContext; value: Cursor): BorrowInfo =
  ## The borrow `value` carries, if any: one already held by the symbol
  ## (`let v = toOpenArray(a)` … `g = v`), one the expression establishes on the
  ## spot (`g = toOpenArray(a)`), or one held by the inline temp the value was
  ## hoisted into (`return toOpenArray(a)` becomes `(let `x …) (ret `x)`).
  result = BorrowInfo(path: @[], mode: NotBorrowable, info: value.info)
  let s = extractSymId(value)
  if s != NoSymId:
    for b in c.activeBorrows:
      if b.borrower == s:
        return b
    if s in c.inlineVars:
      return borrowCarriedBy(c, c.inlineVars.getOrQuit(s))
  if establishesBorrow(c, value):
    result = extractPath(c, value)

proc canCarryBorrow(c: var FirContext; n: Cursor): bool =
  ## Only a reference-like value can carry a borrow past the expression that
  ## produced it. `for x in s: return x` reads through an iteration borrow but
  ## hands back a copy, and a case-of temp holding a plain field value is no
  ## different — in both the borrow ends at the load, so the type is what
  ## decides, not the path.
  let t = getType(c.typeCache, n)
  result = t.typeKind in {MutT, LentT, OutT} or isViewType(t)

proc checkBorrowOutlivesProc(c: var FirContext; value: Cursor) =
  ## A borrow must not outlive what it borrows from. `g = toOpenArray(a)` for a
  ## local `a` leaves `g` pointing into a dead frame, which no amount of
  ## mutation checking downstream can catch — by then the owner is gone.
  if not canCarryBorrow(c, value): return
  let borrowed = borrowCarriedBy(c, value)
  if borrowed.mode == IsBorrowable and borrowed.path.len > 0 and
     diesWithProc(c, borrowed.path[0]):
    buildErr c, value.info, "borrow of '" & asNimCode(borrowed.path[0]) &
      "' escapes the proc; it does not live long enough"

proc checkEscapingBorrow(c: var FirContext; value: Cursor; destRoot: SymId) =
  if destRoot != NoSymId and outlivesProc(c, destRoot):
    checkBorrowOutlivesProc(c, value)

proc endBorrow(c: var FirContext; sym: SymId) =
  var i = 0
  while i < c.activeBorrows.len:
    if c.activeBorrows[i].borrower == sym:
      # order of active borrows is irrelevant, so swap-delete is fine
      c.activeBorrows.del(i)
    else:
      inc i

template getVarId(c: var FirContext; symId: SymId): VarId = VarId(symId)

proc assumeEnsures(c: var FirContext; call: Cursor; resultVar: VarId)
proc operandRange(c: var FirContext; n: Cursor; lo, hi: var xint)

proc analyseIfDerived(c: var FirContext; n: Cursor; call: var CallContext) =
  ## A guard operand that turns out to be a *derived* location (`s.len`, and so
  ## a call) is consumed by the fact machinery rather than by `traverseExpr`, so
  ## it would otherwise escape analysis entirely — including its own contract.
  ## A bare symbol is deliberately not traversed: that is what the code this
  ## replaced did, and traversing one would re-open init questions the guard has
  ## nothing to do with.
  if extractSymId(n) == NoSymId:
    var probe = n
    traverseExpr c, probe, call

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
  of IntLit: lo = createXint(r.intVal)
  of UIntLit: lo = createXint(r.uintVal)
  else: return false
  inc r
  case r.kind
  of IntLit: hi = createXint(r.intVal)
  of UIntLit: hi = createXint(r.uintVal)
  else: return false
  result = lo <= hi

proc integerTypeBounds(bits: int; typ: Cursor; lo, hi: var xint): bool =
  ## The bounds a builtin integer type's *width* states: `(u 8)` holds `0..255`,
  ## `(i 16)` holds `-32768..32767`, `char` is 8 bits. A value of such a type
  ## cannot be anything else, so `byte(x shr a)` indexes a 256-element table
  ## without a single fact about `x`.
  result = false
  var t = typ
  var guard = 0
  var resolved = true
  while t.isSymbol and guard < 8 and resolved:
    let s = tryLoadSym(t.symId)
    if s.status == LacksNothing and s.decl.symKind == TypeY:
      t = asTypeDecl(s.decl).body
    else:
      resolved = false
    inc guard
  let k = t.typeKind
  if resolved and k in {IntT, UIntT, CharT}:
    var width = 8
    if k != CharT:
      var w = t
      inc w
      width = if w.kind == IntLit and w.intVal > 0: int(w.intVal) else: bits
    if width >= 1 and width <= 64:
      if k == IntT:
        hi = (createXint(1'i64) shl (width - 1)) - createXint(1'i64)
        lo = -hi - createXint(1'i64)
      else:
        lo = zero()
        hi = if width == 64: createXint(high(uint64)) else: (createXint(1'i64) shl width) - createXint(1'i64)
      result = not lo.isNaN and not hi.isNaN

proc valueBounds(c: var FirContext; n: Cursor; lo, hi: var xint; depth = 0): bool =
  ## The interval `n` lies in by its *shape* alone — no flow facts: a constant,
  ## `a and k` (`0..k` for a constant `k >= 0`, whatever `a` is), `a shr k` of a
  ## non-negative bounded `a`, and a conversion whose target type holds the
  ## operand's interval. What bit-twiddling code computes an index from —
  ## `HexChars[(n and 0xF0) shr 4]`, `bits[u shr IntShift]` — is exactly this,
  ## and none of it is expressible as `a <= b + c`.
  result = false
  if depth <= 8:
    let folded = tryEvalOrdinal(c.bits, n)
    if not folded.isNaN:
      lo = folded
      hi = folded
      result = true
    else:
      case n.exprKind
      of BitandX:
        var d = n
        d = sub(d)
        skip d # the type operand
        var aLo = zero()
        var aHi = zero()
        let leftBounded = valueBounds(c, d, aLo, aHi, depth+1)
        skip d
        var bLo = zero()
        var bHi = zero()
        if valueBounds(c, d, bLo, bHi, depth+1) and bLo == bHi and zero() <= bLo:
          lo = zero()
          hi = if leftBounded and zero() <= aLo: min(aHi, bHi) else: bHi
          result = true
      of ShrX, AshrX:
        var d = n
        d = sub(d)
        skip d # the type operand
        var aLo = zero()
        var aHi = zero()
        if valueBounds(c, d, aLo, aHi, depth+1) and zero() <= aLo:
          skip d
          var err = false
          let k = asSigned((tryEvalOrdinal(c.bits, d)), err)
          if not err and k >= 0 and k < 64:
            lo = aLo shr int(k)
            hi = aHi shr int(k)
            result = not lo.isNaN and not hi.isNaN
      of HconvX, ConvX:
        var d = n
        d = sub(d)
        let target = d
        skip d
        var tLo = zero()
        var tHi = zero()
        if integerTypeBounds(c.bits, target, tLo, tHi):
          if valueBounds(c, d, lo, hi, depth+1) and tLo <= lo and hi <= tHi:
            result = true
          else:
            # Whatever the operand, the converted value is one of the target
            # type's: `byte(x shr a)` is `0..255`.
            lo = tLo
            hi = tHi
            result = true
      of AddX, SubX:
        let isSub = n.exprKind == SubX
        var d = n
        d = sub(d)
        skip d # the type operand
        var aLo = zero()
        var aHi = zero()
        if valueBounds(c, d, aLo, aHi, depth+1):
          skip d
          var bLo = zero()
          var bHi = zero()
          if valueBounds(c, d, bLo, bHi, depth+1):
            lo = if isSub: aLo - bHi else: aLo + bLo
            hi = if isSub: aHi - bLo else: aHi + bHi
            result = not lo.isNaN and not hi.isNaN
      of SufX:
        # `15'u8` is `(suf 15u "u8")`
        var d = n
        d = sub(d)
        result = valueBounds(c, d, lo, hi, depth+1)
      of MulX:
        var d = n
        d = sub(d)
        skip d # the type operand
        var aLo = zero()
        var aHi = zero()
        if valueBounds(c, d, aLo, aHi, depth+1):
          skip d
          var bLo = zero()
          var bHi = zero()
          if valueBounds(c, d, bLo, bHi, depth+1):
            # the extremes of a product of intervals are among its corners
            let p1 = aLo * bLo
            let p2 = aLo * bHi
            let p3 = aHi * bLo
            let p4 = aHi * bHi
            lo = min(min(p1, p2), min(p3, p4))
            hi = max(max(p1, p2), max(p3, p4))
            result = not (p1.isNaN or p2.isNaN or p3.isNaN or p4.isNaN)
      of ModX, DivX, ShlX:
        # by a positive constant only; `mod` keeps the sign of the dividend
        let k = n.exprKind
        var d = n
        d = sub(d)
        skip d # the type operand
        var aLo = zero()
        var aHi = zero()
        if valueBounds(c, d, aLo, aHi, depth+1):
          skip d
          var bLo = zero()
          var bHi = zero()
          if valueBounds(c, d, bLo, bHi, depth+1) and bLo == bHi and zero() < bLo:
            case k
            of ModX:
              let top = bLo - createXint(1'i64)
              if zero() <= aLo:
                lo = zero()
                hi = min(aHi, top)
              else:
                lo = -top
                hi = if aHi < zero(): zero() else: top
              result = true
            of DivX:
              if zero() <= aLo:
                lo = aLo div bLo
                hi = aHi div bLo
                result = not lo.isNaN and not hi.isNaN
            else:
              if zero() <= aLo and bLo < createXint(63'i64):
                var err = false
                let shift = asSigned(bLo, err)
                if not err:
                  lo = aLo shl int(shift)
                  hi = aHi shl int(shift)
                  result = not lo.isNaN and not hi.isNaN
      else:
        discard
      if not result and (n.isSymbol or
          n.exprKind in {DotX, DdotX, TupatX, HderefX, DerefX, ArratX, AtX, PatX}):
        # a location lies in whatever the flow facts bound it to
        var fLo = createNaN()
        var fHi = createNaN()
        operandRange(c, n, fLo, fHi)
        if not fLo.isNaN and not fHi.isNaN:
          lo = fLo
          hi = fHi
          result = true
        elif not c.prescanning:
          # ... and in whatever its type holds: a `uint8` is `0..255`, and so
          # is a `lent uint8` a loop variable is bound as
          var t = getType(c.typeCache, n)
          while t.typeKind in {LentT, SinkT, MutT, OutT}:
            t = sub(t)
          result = integerTypeBounds(c.bits, t, lo, hi)
      if not result and n.exprKind in {DotX, DdotX}:
        # A field of a `range` type is in that range whatever path it is read
        # by — through a pointer, too, where no flow fact may be kept because of
        # aliasing: every write to such a field owes the range.
        result = staticRangeBounds(getType(c.typeCache, n), lo, hi)

# --- Derived locations: `s.len`, `x.f.g`, `len(s)` ---
#
# `inferle` reasons about `VarId`s, and until now a `VarId` was always a plain
# variable (`VarId(symId)`, with `VarId(0)` the constant zero). That made the
# most important contract in the language unprovable: `seq`/`string` indexing is
# written as `.requires: i < s.len and i >= 0`, and `s.len` is neither a literal
# nor a local.
#
# So a *derived location* — a field path, or a call to a side-effect-free unary
# routine — gets a `VarId` of its own, allocated from the negative range
# (`InvalidVarId` is -1, derived ids start at -2) where it can never collide
# with a `SymId`. Two spellings of the same location must land on the same id,
# which is what "canonical key" means here:
#
#   s.len  written by the caller   ->  (call len s)
#   s.len  written inside seqimpl  ->  (dot s len)
#
# are the same location, and they converge because a call whose body is nothing
# but `result = <path over the parameter>` is *looked through*. Where the body
# is not that simple (`string.len`), the call itself is the key — sound because
# a `func` in Nimony "does not access global or thread-local variables and does
# not call any routine that has side effects" (doc/language.md), so its value is
# a function of its argument alone, and the argument's root is what a write
# invalidates.

const
  FirstDerivedVarId = -2
  MaxLocationDepth = 6   ## bounds accessor look-through and path nesting

proc peelExpr(n: Cursor): Cursor =
  ## Look through the wrappers a value picks up on its way into a parameter or
  ## out of a location: hidden conversions, the hidden address of a `var`
  ## argument, a literal's type suffix.
  result = n
  var guard = 0
  while guard < MaxLocationDepth:
    inc guard
    case result.exprKind
    of HconvX, ConvX:
      inc result
      skip result # the target type
    of HaddrX, HderefX, ParX, SufX:
      inc result
    else: break

proc argOf(n: Cursor; subst: Table[SymId, Cursor]): Cursor =
  ## `n` with a substituted symbol replaced by the expression it stands for: a
  ## callee's parameter by the argument written at the call site, or an
  ## accessor's parameter by the expression it was applied to. `subst` is empty
  ## wherever the symbols already are the ones to reason about.
  result = peelExpr(n)
  if result.isSymbol and subst.hasKey(result.symId):
    result = peelExpr(subst.getOrQuit(result.symId))

proc derivedIdOf(c: var FirContext; key: string; root: SymId): VarId =
  if c.derivedIds.hasKey(key):
    result = c.derivedIds.getOrQuit(key)
  else:
    c.derivedRoots.add root
    c.derivedKeys.add key
    result = VarId(FirstDerivedVarId - (c.derivedRoots.len - 1))
    c.derivedIds[key] = result

proc invalidateDerivedFrom(c: var FirContext; root: SymId) =
  ## Everything derived from `root` is stale once `root` — or anything inside
  ## it — is written. Coarse on purpose: writing `d.head` also drops `d.count`.
  for i in 0 ..< c.derivedRoots.len:
    if c.derivedRoots[i] == root:
      invalidateFactsAbout(c.facts, VarId(FirstDerivedVarId - i))

proc isElementWrite(n: Cursor): bool {.inline.} =
  ## A write to one element of an array or of a pointer's target: it changes
  ## that element and nothing else. No derived location is keyed through an
  ## element, so none can be its target.
  n.exprKind in {ArratX, PatX, AtX}

proc keyUnder(derived, written: string): bool =
  ## Is the location keyed `derived` reached through the one keyed `written`?
  ## Either it lies below it (`written` is a prefix, ending at a component
  ## boundary), or it is a call over some part of the root — an opaque function
  ## of the whole value, which any write may change.
  if derived.startsWith(written):
    result = derived.len == written.len or derived[written.len] in {'.', '-'}
  elif derived.len > 0 and derived[0] == 'c':
    # `c<fn>(<arg>)` is a function of the location `arg` alone: stale when the
    # write reaches into `arg` or lands on something `arg` lies within, not
    # when it hits a sibling — `p.kind = k` leaves `len(p.current)` alone.
    let open = derived.find('(')
    if open < 0 or derived[derived.len-1] != ')':
      result = true
    else:
      let arg = derived.substr(open+1, derived.len-2)
      result = keyUnder(arg, written) or keyUnder(written, arg)
  else:
    result = false

proc hasOverlappingFields(c: var FirContext; objType: Cursor): bool =
  ## A variant object or a union: writing one field may change another.
  var t = objType
  var guard = 0
  result = false
  while t.isSymbol and guard < 8:
    var declCursor = default(Cursor)
    if c.moduleTypes.hasKey(t.symId):
      declCursor = c.moduleTypes.getOrQuit(t.symId)
    else:
      let decl = tryLoadSym(t.symId)
      if decl.status != LacksNothing or decl.decl.symKind != TypeY: return true
      declCursor = decl.decl
    let td = asTypeDecl(declCursor)
    if hasPragma(td.pragmas, UnionP): return true
    t = td.body
    inc guard
  if t.typeKind in {PtrT, RefT, MutT}:
    t = sub(t)
    return hasOverlappingFields(c, t)
  if t.typeKind != ObjectT: return true
  var f = t
  f = sub(f)
  skip f # the inheritance slot
  while f.hasMore:
    if f.substructureKind == CaseU: return true
    skip f

proc invalidateAllDerived(c: var FirContext) =
  ## A write we cannot attribute to a root (through a pointer, say) may have hit
  ## any derived location. A slot without a root is the analysis's own
  ## (`#boundvalue`, an `old(…)` snapshot) and names no location a write
  ## reaches.
  for i in 0 ..< c.derivedRoots.len:
    if c.derivedRoots[i] != NoSymId:
      invalidateFactsAbout(c.facts, VarId(FirstDerivedVarId - i))

proc isKeyableCall(fnSym: SymId): bool =
  ## May a call to `fnSym` stand for a location of its own?
  ##
  ## Two conditions. It must be side-effect free, which in Nimony means it
  ## "does not access global or thread-local variables and does not call any
  ## routine that has side effects" (doc/language.md) — that is what makes its
  ## value a function of its argument alone, so that invalidating the argument's
  ## root invalidates the call. And it must return an integer, which keeps this
  ## escape hatch to the arithmetic facts it exists for (`len`) instead of
  ## quietly asserting that two calls returning fresh objects are the same
  ## object.
  let s = tryLoadSym(fnSym)
  if s.status != LacksNothing: return false
  if not isRoutine(s.decl.symKind): return false
  let r = asRoutine(s.decl)
  if cursorIsNil(r.pragmas) or cursorIsNil(r.retType): return false
  if r.retType.typeKind notin {IT, UT}: return false
  if hasPragma(r.pragmas, SideEffectP): return false
  if hasPragma(r.pragmas, NoSideEffectP): return true
  result = r.kind in {FuncY, ConverterY}

proc matchAccessor(decl: Cursor; param: var SymId; value: var Cursor): bool =
  ## Recognize a *transparent accessor*: a one-parameter routine whose body is
  ## nothing but `result = <expr>`, which is exactly what sem produces for
  ## `func len[T](s: seq[T]): int = s.len`. Both spellings of the assignment are
  ## accepted, because the same declaration is read from the Final IR
  ## (`(store value dest)`) and, for an imported routine, from the module's
  ## interface (`(asgn dest value)`).
  result = false
  if not isRoutine(decl.symKind): return false
  let r = asRoutine(decl, SkipInclBody)
  if cursorIsNil(r.params) or not r.params.isParamsTag: return false
  var p = r.params
  p = sub(p)
  if not p.hasMore: return false
  let first = takeLocal(p, SkipFinalParRi)
  if p.hasMore: return false          # more than one parameter
  if first.typ.typeKind in {OutT, MutT}: return false
  if cursorIsNil(r.body) or r.body.stmtKind != StmtsS: return false
  var resultSym = NoSymId
  var val = default(Cursor)
  var b = r.body
  b = sub(b)
  while b.hasMore:
    if b.symKind == ResultY:
      var l = b
      l = sub(l)
      resultSym = l.symId
      skip b
    elif b.stmtKind == AsgnS or b.finalIrKind == StoreV:
      if not cursorIsNil(val) or resultSym == NoSymId: return false
      let isStore = b.finalIrKind == StoreV
      var a = b
      a = sub(a)
      if isStore:
        val = a
        skip a
        if extractSymId(a) != resultSym: return false
      else:
        if extractSymId(a) != resultSym: return false
        skip a
        val = a
      skip b
    elif b.stmtKind in {RetS, AssumeS, AssertS} or b.finalIrKind in {AssumeV, AssertV}:
      # a proposition changes nothing at run time: `len` states its own
      # non-negativity next to the path it returns
      skip b
    else:
      return false
  if cursorIsNil(val): return false
  param = first.name.symId
  value = val
  result = true

proc accessorOf(c: var FirContext; fnSym: SymId; param: var SymId; value: var Cursor): bool =
  ## `c.accessors` is filled by a pre-pass over the module being analysed, which
  ## is what makes a *generic instance* such as `len.3.Ixyz` — created in this
  ## module and therefore not in `programs` yet — look-through-able. An imported
  ## routine is read from its interface instead.
  if c.accessors.hasKey(fnSym):
    let a = c.accessors.getOrQuit(fnSym)
    param = a.param
    value = a.value
    return true
  if fnSym in c.notAccessors: return false
  let s = tryLoadSym(fnSym)
  if s.status == LacksNothing and matchAccessor(s.decl, param, value):
    c.accessors[fnSym] = AccessorInfo(param: param, value: value)
    return true
  c.notAccessors.incl fnSym
  result = false

proc locationKey(c: var FirContext; n: Cursor; subst: Table[SymId, Cursor];
                 root: var SymId; key: var string; steps: var int;
                 depth: int): bool =
  ## Canonical key of a pure location. `steps == 0` means the location *is* the
  ## plain variable `root`; anything else is derived.
  if depth > MaxLocationDepth: return false
  let m = argOf(n, subst)
  let s = extractSymId(m)
  if s != NoSymId:
    if c.inlineVars.hasKey(s):
      # An `(inline)` temp stands for the expression it was bound to. The bound
      # of `for i in 0 ..< s.len` becomes such a temp, and without looking
      # through it the loop bound and the contract would never meet. A temp
      # bound to something that is not a location is still a variable of its
      # own, so a failed look-through falls through rather than failing.
      var r2 = NoSymId
      var k2 = ""
      var st2 = 0
      if locationKey(c, c.inlineVars.getOrQuit(s), subst, r2, k2, st2, depth+1) and r2 != NoSymId:
        root = r2
        key = k2
        steps = st2
        return true
    root = s
    key = "v" & $uint32(s)
    return true
  case m.exprKind
  of DotX:
    # `a.f`. A field reached through a pointer or a `ref` is no location: any
    # alias may write it, and nothing here tracks aliases.
    var r = m
    r = sub(r)
    if not locationKey(c, r, subst, root, key, steps, depth+1): return false
    skip r # the object
    if not r.isSymbol: return false
    key.add "."
    key.add $uint32(r.symId)
    inc steps
    result = true
  of TupatX:
    # `t[0]` of a tuple: a field like any other, named by its position
    var r = m
    r = sub(r)
    if not locationKey(c, r, subst, root, key, steps, depth+1): return false
    skip r # the tuple
    if not r.isIntLit: return false
    key.add ".#" & $r.intVal
    inc steps
    result = true
  of CallKinds:
    var r = m
    r = sub(r)
    let fnSym = extractSymId(r)
    if fnSym == NoSymId: return false
    skip r # the callee
    if not r.hasMore: return false
    let arg = r
    skip r
    if r.hasMore: return false # more than one argument
    var accessorParam = NoSymId
    var accessorValue = default(Cursor)
    if accessorOf(c, fnSym, accessorParam, accessorValue):
      # Key the *path the accessor returns*, so that `len(s)` and the `s.len`
      # written inside the defining module are one and the same location.
      # The argument is resolved against the substitution in flight *first*: in
      # a callee's `.requires`, `len(a)` applies the accessor to the callee's
      # parameter `a`, which stands for the argument at the call site. The
      # accessor's own path then needs its parameter and nothing else — keeping
      # the outer map there would substitute twice, and a recursive call with
      # swapped arguments maps `a` to `b` and `b` back to `a`.
      let resolved = argOf(arg, subst)
      if resolved.isSymbol or not resolved.isTagLit or subst.len == 0:
        var inner = initTable[SymId, Cursor]()
        inner[accessorParam] = resolved
        return locationKey(c, accessorValue, inner, root, key, steps, depth+1)
      # A path argument (`len(b.data)`) still names the callee's parameters
      # inside: key it under the substitution in flight, then graft the path
      # the accessor returns onto it.
      var argRoot = NoSymId
      var argKey = ""
      var argSteps = 0
      if not locationKey(c, arg, subst, argRoot, argKey, argSteps, depth+1): return false
      var own = NoSymId
      var ownKey = ""
      var ownSteps = 0
      let noSubst = initTable[SymId, Cursor]()
      if not locationKey(c, accessorValue, noSubst, own, ownKey, ownSteps, depth+1): return false
      let paramKey = "v" & $uint32(accessorParam)
      if own != accessorParam or not ownKey.startsWith(paramKey) or
          (ownKey.len > paramKey.len and ownKey[paramKey.len] notin {'.', '-'}):
        return false
      root = argRoot
      key = argKey & ownKey.substr(paramKey.len)
      steps = argSteps + ownSteps
      return true
    if not isKeyableCall(fnSym): return false
    if not locationKey(c, arg, subst, root, key, steps, depth+1): return false
    key = "c" & $uint32(fnSym) & "(" & key & ")"
    inc steps
    result = true
  else:
    result = false

proc invalidateWrittenPath(c: var FirContext; dest: Cursor; root: SymId) =
  ## A write to the location `dest`, rooted at `root`: what lies under it goes
  ## stale, its siblings do not — `a.trunc = true` says nothing about `a.nd`.
  ## Coarse for a variant object or a union, whose fields overlap.
  var r = NoSymId
  var key = ""
  var steps = 0
  let noSubst = initTable[SymId, Cursor]()
  var precise = dest.exprKind in {DotX, DdotX} and
    locationKey(c, dest, noSubst, r, key, steps, 0) and r == root and steps > 0
  if precise:
    var obj = dest
    obj = sub(obj)
    precise = not hasOverlappingFields(c, getType(c.typeCache, obj))
  if precise:
    for i in 0 ..< c.derivedRoots.len:
      if c.derivedRoots[i] == root and keyUnder(c.derivedKeys[i], key):
        # projected, not dropped: `let L = s.len - 1; s.len = L` must still
        # relate `L` to what `s.len` was
        projectOut(c.facts, VarId(FirstDerivedVarId - i))
  else:
    invalidateDerivedFrom(c, root)

proc noteDeclaredRange(c: var FirContext; v: VarId; typ: Cursor) =
  if v == InvalidVarId or v == VarId(0) or c.declaredRange.hasKey(v): return
  var lo = zero()
  var hi = zero()
  if staticRangeBounds(typ, lo, hi):
    c.declaredRange[v] = RangeBounds(lo: lo, hi: hi)

proc resultBound(r: Cursor; resultSyms: seq[SymId]; lo, hi: var xint) =
  ## Narrow `lo..hi` by one conjunct of an `.ensures` that compares `result`
  ## with an integer literal; any other conjunct says nothing here.
  case r.exprKind
  of AndX:
    var x = r
    x = sub(x)
    resultBound(x, resultSyms, lo, hi)
    skip x
    resultBound(x, resultSyms, lo, hi)
  of LeX, LtX:
    let strict = if r.exprKind == LtX: createXint(1'i64) else: zero()
    var x = r
    x = sub(x)
    skip x # the type operand
    let a = peelExpr(x)
    skip x
    let b = peelExpr(x)
    if a.kind == IntLit and b.isSymbol and b.symId in resultSyms:
      lo = max(lo, createXint(a.intVal) + strict)
    elif b.kind == IntLit and a.isSymbol and a.symId in resultSyms:
      hi = min(hi, createXint(b.intVal) - strict)
  else: discard

proc ensuredResultRange(c: var FirContext; call: Cursor; lo, hi: var xint): bool =
  ## The bounds a callee's `.ensures` puts on `result` alone — `0 <= result`
  ## for `len`. They hold whatever the arguments are, so they describe every
  ## value the call can have: a declared range in all but name, and one that no
  ## write to the arguments makes stale.
  result = false
  var fn = call
  fn = sub(fn)
  var fnType = skipProcTypeToParams(getType(c.typeCache, fn))
  if not fnType.isParamsTag: return
  skip fnType # params
  skip fnType # return type
  let ens = extractPragma(fnType, EnsuresP)
  if cursorIsNil(ens) or ens.exprKind != ExprX: return
  var resultSyms: seq[SymId] = @[]
  var cond = ens
  while cond.exprKind == ExprX:
    var inner = cond
    inner = sub(inner)
    if not inner.hasMore: break
    if inner.symKind == ResultY:
      resultSyms.add asLocal(inner).name.symId
    while inner.hasMore and not isLastSon(inner): skip inner
    cond = inner
  if resultSyms.len == 0: return
  lo = createXint(low(int64))
  hi = createXint(high(int64))
  resultBound(cond, resultSyms, lo, hi)
  result = lo > createXint(low(int64)) or hi < createXint(high(int64))

proc locationVarId(c: var FirContext; n: Cursor; subst: Table[SymId, Cursor]): VarId =
  ## The `VarId` standing for the location `n` denotes, or `InvalidVarId`.
  var root = NoSymId
  var key = ""
  var steps = 0
  if locationKey(c, n, subst, root, key, steps, 0) and root != NoSymId:
    result = if steps == 0: getVarId(c, root) else: derivedIdOf(c, key, root)
    if steps > 0 and not c.prescanning and not c.declaredRange.hasKey(result):
      let m = argOf(n, subst)
      var lo = zero()
      var hi = zero()
      if m.exprKind in CallKinds and ensuredResultRange(c, m, lo, hi):
        c.declaredRange[result] = RangeBounds(lo: lo, hi: hi)
      else:
        noteDeclaredRange(c, result, getType(c.typeCache, m))
  else:
    result = InvalidVarId

proc plainLocationVarId(c: var FirContext; n: Cursor): VarId =
  ## `locationVarId` for an expression written in the scope being analysed, so
  ## no parameter substitution is in flight.
  let noSubst = initTable[SymId, Cursor]()
  result = locationVarId(c, n, noSubst)

proc offsetAccessor(c: var FirContext; n: Cursor; subst: Table[SymId, Cursor];
                    v: var VarId; k: var xint): bool =
  ## `high(s)`: a transparent accessor whose body is a location plus a
  ## constant (`s.len - 1`), read as that location and that constant.
  result = false
  let m = argOf(n, subst)
  if m.exprKind notin CallKinds: return
  var r = m
  r = sub(r)
  let fnSym = extractSymId(r)
  if fnSym == NoSymId: return
  skip r # the callee
  if not r.hasMore: return
  let arg = r
  skip r
  if r.hasMore: return
  var param = NoSymId
  var value = default(Cursor)
  if not accessorOf(c, fnSym, param, value): return
  let body = peelExpr(value)
  if body.exprKind notin {AddX, SubX}: return
  var b = body
  b = sub(b)
  skip b # the type operand
  var inner = initTable[SymId, Cursor]()
  inner[param] = argOf(arg, subst)
  # Not `locationVarId`: that asks the type cache about the path, which names
  # the accessor's own parameter.
  var root = NoSymId
  var key = ""
  var steps = 0
  if not locationKey(c, b, inner, root, key, steps, 0) or root == NoSymId: return
  let loc = if steps == 0: getVarId(c, root) else: derivedIdOf(c, key, root)
  skip b
  var off = createNaN()
  case b.kind
  of IntLit: off = createXint(b.intVal)
  of UIntLit: off = createXint(b.uintVal)
  else: off = tryEvalOrdinal(c.bits, b)
  if off.isNaN: return
  v = loc
  k = if body.exprKind == SubX: -off else: off
  result = true

proc matchStep(decl: Cursor; info: var StepInfo): bool =
  ## Recognize a *step routine*: one whose whole body moves its first, `var`,
  ## parameter by a fixed amount. `inc`/`dec` are exactly that after sem:
  ##
  ##   func inc*[T: Ordinal](x: var T) {.inline.} = x = succ(x)
  ##   ⇒ (stmts (asgn (hderef x) (add T (hderef x) 1)))
  ##
  ## Matching the *shape* rather than the name means a hand-written stepper is
  ## understood too, and that nothing here has to trust a magic.
  result = false
  if not isRoutine(decl.symKind): return false
  let r = asRoutine(decl, SkipInclBody)
  if cursorIsNil(r.params) or not r.params.isParamsTag: return false
  var p = r.params
  p = sub(p)
  if not p.hasMore: return false
  let first = takeLocal(p, SkipFinalParRi)
  if first.typ.typeKind notin {MutT, OutT}: return false
  var second = NoSymId
  if p.hasMore:
    let s = takeLocal(p, SkipFinalParRi)
    second = s.name.symId
    if p.hasMore: return false        # more than two parameters
  if cursorIsNil(r.body) or r.body.stmtKind != StmtsS: return false

  var b = r.body
  b = sub(b)
  if not b.hasMore: return false
  let isStore = b.finalIrKind == StoreV
  if b.stmtKind != AsgnS and not isStore: return false
  var a = b
  a = sub(a)
  var dest = default(Cursor)
  var value = default(Cursor)
  if isStore:
    value = a
    skip a
    dest = a
  else:
    dest = a
    skip a
    value = a
  if extractSymId(dest) != first.name.symId: return false
  skip b
  if b.hasMore: return false          # a second statement: not a plain step

  let v = peelExpr(value)
  if v.exprKind notin {AddX, SubX}: return false
  var e = v
  e = sub(e)
  skip e # the type operand
  if extractSymId(e) != first.name.symId: return false
  skip e
  info = StepInfo(param: first.name.symId, isSub: v.exprKind == SubX,
                  stepSym: NoSymId, stepLit: createXint(0'i32))
  let sid = extractSymId(e)
  if sid != NoSymId and sid == second:
    info.stepSym = second
    return true
  # `inc` on a `Natural` steps by `(conv (rangetype …) 1)`
  let lit = peelExpr(e)
  case lit.kind
  of IntLit: info.stepLit = createXint(lit.intVal)
  of UIntLit: info.stepLit = createXint(lit.uintVal)
  else:
    info.stepLit = tryEvalOrdinal(0, e)   # `bits` is irrelevant for a literal
    if info.stepLit.isNaN: return false
  result = true

proc stepOf(c: var FirContext; fnSym: SymId; info: var StepInfo): bool =
  if c.steps.hasKey(fnSym):
    info = c.steps.getOrQuit(fnSym)
    return true
  if fnSym in c.notSteps: return false
  let s = tryLoadSym(fnSym)
  if s.status == LacksNothing and matchStep(s.decl, info):
    c.steps[fnSym] = info
    return true
  c.notSteps.incl fnSym
  result = false

proc directionOf(isSub: bool; step: xint): IvKind =
  if step.isNaN: return ivUnknown
  if step < createXint(0'i32):
    result = if isSub: ivIncreasing else: ivDecreasing
  else:
    result = if isSub: ivDecreasing else: ivIncreasing

proc directionOfDelta(delta: xint): IvKind =
  if delta.isNaN: ivUnknown
  elif delta < createXint(0'i32): ivDecreasing
  else: ivIncreasing

proc stepCallAt(c: var FirContext; callee: Cursor; loc: var VarId;
                delta: var xint; stepArg: var Cursor): bool =
  ## Is this a call to a step routine? Answers with the location it moves and by
  ## how much, signed. `inc(i, k)` with a `k` whose value is not known here is a
  ## write like any other — `inc` on an ordinal accepts a negative step.
  ## `callee` points at the callee, with the arguments as its siblings.
  result = false
  loc = InvalidVarId
  delta = createNaN()
  var r = callee
  let fnSym = extractSymId(r)
  if fnSym == NoSymId: return false
  var info = default(StepInfo)
  if not stepOf(c, fnSym, info): return false
  skip r # the callee
  if not r.hasMore: return false
  loc = plainLocationVarId(c, r)
  if loc == InvalidVarId: return false
  skip r
  var step = info.stepLit
  if info.stepSym != NoSymId:
    # the step is the second parameter: read it from this call site
    if not r.hasMore: return false
    let arg = peelExpr(r)
    case arg.kind
    of IntLit: step = createXint(arg.intVal)
    of UIntLit: step = createXint(arg.uintVal)
    else: step = tryEvalOrdinal(c.bits, arg)
    if step.isNaN: stepArg = arg
    skip r
  if r.hasMore: return false
  if step.isNaN: return false
  delta = if info.isSub: -step else: step
  result = true

proc stepCall(c: var FirContext; n: Cursor; loc: var VarId; delta: var xint): bool =
  ## `stepCallAt` for a cursor sitting on the whole `(call …)` node.
  if not n.isTagLit: return false
  var callee = n
  callee = sub(callee)
  var stepArg = default(Cursor)
  result = stepCallAt(c, callee, loc, delta, stepArg)

proc isVarOpenArray(c: var FirContext; t: Cursor): bool =
  ## `var openArray[T]`: a view whose elements may be written but which itself
  ## is never replaced — nothing done through it changes its length. `openArray`
  ## is the `{.view.}` type.
  result = false
  if t.typeKind == MutT:
    var e = t
    e = sub(e)
    if e.isSymbol:
      var decl = default(Cursor)
      if c.moduleTypes.hasKey(e.symId):
        decl = c.moduleTypes.getOrQuit(e.symId)
      else:
        let sym = tryLoadSym(e.symId)
        if sym.status == LacksNothing and sym.decl.symKind == TypeY: decl = sym.decl
      if not cursorIsNil(decl):
        result = hasPragma(asTypeDecl(decl).pragmas, ViewP)

proc isViewLen(c: FirContext; i: int; root: SymId): bool =
  ## Is derived location `i` the length field of the view `root` itself?
  result = false
  if c.derivedRoots[i] == root:
    let key = c.derivedKeys[i]
    let prefix = "v" & $uint32(root) & "."
    if key.startsWith(prefix):
      let rest = key.substr(prefix.len)
      if rest.len > 0 and rest.len <= 10:
        var id = 0'u64
        var allDigits = true
        for ch in rest:
          if ch in {'0'..'9'}: id = id * 10'u64 + uint64(ord(ch) - ord('0'))
          else: allDigits = false
        if allDigits and id <= uint64(high(uint32)):
          result = pool.symBasename(SymId(uint32(id))) == "len"

proc invalidateViewElements(c: var FirContext; root: SymId) =
  ## A `var openArray` handed to a call: its elements are anyone's guess
  ## afterwards, its length is not.
  invalidateFactsAbout(c.facts, getVarId(c, root))
  for i in 0 ..< c.derivedRoots.len:
    if c.derivedRoots[i] == root and not isViewLen(c, i, root):
      invalidateFactsAbout(c.facts, VarId(FirstDerivedVarId - i))

proc returnsPointee(c: var FirContext; fnSym: SymId): bool =
  ## Is the `var T` this routine returns the address of a pointer's target —
  ## `s.data[i]` for a `seq`, `x.a[idx]` for an `openArray`? A write through
  ## such a result lands in storage behind a pointer, which no location keyed
  ## inline in the argument (`len(s)`) can be; only the heap is disturbed.
  if c.pointeeFns.hasKey(fnSym): return c.pointeeFns.getOrQuit(fnSym)
  result = false
  var decl = default(Cursor)
  if c.moduleRoutines.hasKey(fnSym):
    decl = c.moduleRoutines.getOrQuit(fnSym)
  else:
    let sym = tryLoadSym(fnSym)
    if sym.status == LacksNothing: decl = sym.decl
  if not cursorIsNil(decl) and isRoutine(decl.symKind):
    let r = asRoutine(decl, SkipInclBody)
    if not cursorIsNil(r.body) and r.body.stmtKind == StmtsS and
        r.retType.typeKind == MutT:
      var b = r.body
      b = sub(b)
      var shapes = 0
      var others = 0
      while b.hasMore:
        if b.stmtKind == AsgnS or b.finalIrKind == StoreV:
          var a = b
          a = sub(a)
          var value = a
          if b.stmtKind == AsgnS:
            skip value
          var v = value
          while v.exprKind in {HaddrX, AddrX}:
            v = sub(v)
          if v.exprKind in {PatX, DerefX, DdotX}: inc shapes
          else: inc others
        elif b.symKind != ResultY and b.stmtKind notin {RetS, AssumeS} and
            b.finalIrKind notin {AssumeV}:
          inc others
        skip b
      result = shapes == 1 and others == 0
  c.pointeeFns[fnSym] = result

proc writesThroughPointee(c: var FirContext; dest: Cursor): bool =
  ## `data[i].key = v`: a path over `(hderef (call f …))` with `f` returning a
  ## pointer's target (`returnsPointee`).
  var d = dest
  while d.exprKind in {HaddrX, AddrX}:
    d = sub(d)
  while d.exprKind in {DotX, TupatX, ArratX}:
    d = sub(d)
  result = false
  if d.exprKind == HderefX:
    d = sub(d)
    if d.isSymbol and c.inlineVars.hasKey(d.symId):
      # `var x = data[i]` bound as an `(inline)` temp
      d = c.inlineVars.getOrQuit(d.symId)
    if d.exprKind in CallKinds:
      var fn = d
      fn = sub(fn)
      let fnSym = extractSymId(fn)
      result = fnSym != NoSymId and returnsPointee(c, fnSym)

proc derivedRootOf(c: var FirContext; n: Cursor): SymId =
  ## The variable a location hangs off, for invalidation.
  var root = NoSymId
  var key = ""
  var steps = 0
  let noSubst = initTable[SymId, Cursor]()
  if locationKey(c, n, noSubst, root, key, steps, 0):
    result = root
  else:
    result = NoSymId

proc collectAccessors(c: var FirContext; n: var Cursor) =
  ## Pre-pass: index every transparent accessor declared in this module, at any
  ## nesting level, before the traversal needs one. A *generic instance* such as
  ## `len.3.Ixyz` is created here rather than imported, so `tryLoadSym` cannot
  ## find it; without this pre-pass `s.len` written by a caller and the `s.len`
  ## written inside `seqimpl` would stay two unrelated locations.
  if not n.isTagLit:
    skip n
    return
  if n.symKind == TypeY:
    var name = n
    name = sub(name)
    if name.kind == SymbolDef: c.moduleTypes[name.symId] = n
  if n.symKind == ConstY:
    let local = asLocal(n)
    if local.name.kind == SymbolDef and not cursorIsNil(local.val):
      c.moduleConsts[local.name.symId] = local.val
  if n.symKind in RoutineKinds:
    block:
      var rname = n
      rname = sub(rname)
      if rname.kind == SymbolDef: c.moduleRoutines[rname.symId] = n
    if n.symKind == FuncY:
      var fname = n
      fname = sub(fname)
      if fname.kind == SymbolDef: c.moduleFuncs.incl fname.symId
    var param = NoSymId
    var value = default(Cursor)
    if matchAccessor(n, param, value):
      var name = n
      name = sub(name)
      if name.kind == SymbolDef:
        c.accessors[name.symId] = AccessorInfo(param: param, value: value)
  n.into:
    while n.hasMore:
      collectAccessors(c, n)

proc baseTypeBounds(typ: Cursor; bits: int; lo, hi: var xint): bool =
  ## The full range of the ordinal a `range[…]` is carved out of.
  ##
  ## An obligation against a bound that *is* the base type's own bound is
  ## vacuous — every value of the type satisfies it — and skipping it is what
  ## makes `Natural = range[0 .. high(int)]` cheap: only `0 <= x` is ever asked,
  ## never `x <= high(int)`, which is not derivable for an arbitrary `int` and
  ## would have made every `Natural` binding unprovable.
  var t = typ
  var guard = 0
  while t.isSymbol and guard < 8:
    let s = tryLoadSym(t.symId)
    if s.status != LacksNothing or s.decl.symKind != TypeY: return false
    t = asTypeDecl(s.decl).body
    inc guard
  if t.typeKind != RangetypeT: return false
  var b = t
  inc b # past the rangetype tag, at the base type
  let k = b.typeKind
  if k notin {IT, UT, CT}: return false
  var size = bits
  var sz = b
  inc sz
  if sz.kind == IntLit and sz.intVal > 0: size = int(sz.intVal)
  elif k == CT: size = 8
  if size <= 0 or size > 64: return false
  if k == IT:
    if size == 64:
      lo = createXint(low(int64))
      hi = createXint(high(int64))
    else:
      lo = -createXint(1'i64 shl (size - 1))
      hi = createXint((1'i64 shl (size - 1)) - 1)
  else:
    lo = createXint(0'i32)
    if size == 64:
      hi = createXint(high(uint64))
    else:
      hi = createXint((1'i64 shl size) - 1)
  result = true

proc impliesHere(c: var FirContext; fact: LeXplusC): bool =
  ## The flow facts, plus what the locations' own `range` types state.
  if implies(c.facts, fact): return true
  if fact.a == VarId(0) and c.declaredRange.hasKey(fact.b):
    # `0 <= b + k` holds when the declared `lo` is at least `-k`.
    return c.declaredRange.getOrQuit(fact.b).lo >= -fact.c
  if fact.b == VarId(0) and c.declaredRange.hasKey(fact.a):
    # `a <= 0 + k` holds when the declared `hi` is at most `k`.
    if c.declaredRange.getOrQuit(fact.a).hi <= fact.c: return true
  # Through a chain: `i <= d.count - 1` with `count: range[0..30]` bounds `i`,
  # but only once the declared range takes part in the closure as facts.
  result = false
  if c.declaredRange.len > 0:
    var extended = c.facts
    for v, r in c.declaredRange:
      extended.add query(VarId(0), v, -r.lo)
      extended.add query(v, VarId(0), r.hi)
    result = implies(extended, fact)

proc cannotProve(c: var FirContext; info: NifLineInfo; report: bool; msg: string) =
  ## The "cannot prove" half of a range obligation, which not every caller
  ## wants to hear. An array index outside `staticContracts` states nothing the
  ## programmer must discharge — the bound check simply stays — so it asks for
  ## silence here. A *disproof* (a literal outside the range) is not routed
  ## through this and is always reported.
  if report: buildErr c, info, msg

proc checkInRange(c: var FirContext; value: Cursor; lo, hi: xint;
                  needLo, needHi: bool; reportUnprovable = true) =
  ## Discharge `lo <= value <= hi` from what is known here. The obligation's
  ## bounds are parameters rather than a type, because the two things that carry
  ## one are not both types: an assignment takes them from the target's
  ## `range[lo..hi]` (`checkRangeAssign`), an array index from the index range
  ## the `(arrat …)` node carries (`analyseArrAt`). Everything below — the
  ## structural answers for `a + k`, `a - b`, `a shr k`, `a and k`, a call's
  ## `.ensures`, a literal — is the same question either way.
  # 1. The value's own *declared type* may already be a `range` that fits: a
  #    subset `range[aLo..aHi]` with `lo <= aLo` and `aHi <= hi` is provably in
  #    range. This is the type acting as its own proof (proper subtyping such as
  #    `range[2..5]` -> `range[0..10]`), and it is robust across control-flow
  #    joins where flow-derived facts would be intersected away.
  var aLo = zero()
  var aHi = zero()
  let valueType = getType(c.typeCache, value)
  if staticRangeBounds(valueType, aLo, aHi) and lo <= aLo and aHi <= hi: return
  if integerTypeBounds(c.bits, valueType, aLo, aHi) and lo <= aLo and aHi <= hi: return
  if valueBounds(c, value, aLo, aHi) and lo <= aLo and aHi <= hi: return

  # 2. Otherwise, discharge `lo <= value <= hi` from the facts known on this
  #    path (e.g. a preceding `if a >= 0 ... a <= 10` guard, or a `range`-typed
  #    parameter whose bounds were seeded on entry).
  var v = VarId(0)
  var off = zero()
  var isLit = false
  var r = value
  let sym = skipSymbol(r)

  # `v + k` / `v - k`, the shape every "one past the last state" assignment
  # takes. The obligation below is already stated as `lo <= v + off <= hi`, so
  # a stepped value only has to fill in `off`; without this a guarded
  # `if s < High: t = s + 1` proved nothing about `t` and the conversion had to
  # be spelled as a `cast`.
  if sym == NoSymId and value.exprKind in {AddX, SubX}:
    let isSub = value.exprKind == SubX
    var a = value
    a = sub(a)
    skip a # the type operand
    let baseStart = a
    var baseSym = skipSymbol(a)
    var baseLoc = if baseSym != NoSymId: getVarId(c, baseSym) else: InvalidVarId
    if baseSym == NoSymId:
      # a derived location as the base: `a.d[a.nd - 1]`
      baseLoc = plainLocationVarId(c, baseStart)
      if baseLoc != InvalidVarId:
        skip a
    if baseLoc != InvalidVarId:
      var k = createNaN()
      case a.kind
      of IntLit: k = createXint(a.intVal)
      of UIntLit: k = createXint(a.uintVal)
      else: k = tryEvalOrdinal(c.bits, a)
      if not k.isNaN:
        v = baseLoc
        off = if isSub: -k else: k
        let lower0 = query(VarId(0), v, off - lo)
        let upper0 = query(v, VarId(0), hi - off)
        if (not needLo or impliesHere(c, lower0)) and
           (not needHi or impliesHere(c, upper0)):
          return
        cannotProve c, value.info, reportUnprovable, "cannot prove '" & asNimCode(baseStart) &
          "' stays in range " & $lo & ".." & $hi
        return
      let other = if isSub and k.isNaN: plainLocationVarId(c, a) else: InvalidVarId
      if other != InvalidVarId and other != VarId(0):
        # `a - b` of two locations is a difference the facts state directly:
        # `lo <= a - b` is `b <= a - lo`, `a - b <= hi` is `a <= b + hi` —
        # what a block copy `buf[p - start]` owes
        if (not needLo or impliesHere(c, query(other, baseLoc, -lo))) and
           (not needHi or impliesHere(c, query(baseLoc, other, hi))):
          return

  # A call binding straight to its destination — the Final IR's normal form —
  # can only be judged by what the callee promises. Its `.ensures` is read into
  # an anonymous slot: phrasing it on the *destination* instead would make the
  # check vacuous, since a `Natural` destination's own declared range already
  # answers its own range question.
  if sym == NoSymId and value.isTagLit and value.exprKind in CallKinds:
    let slot = derivedIdOf(c, "#boundvalue", NoSymId)
    invalidateFactsAbout(c.facts, slot)
    assumeEnsures(c, value, slot)
    let lowerE = query(VarId(0), slot, -lo)
    let upperE = query(slot, VarId(0), hi)
    let ok = (not needLo or implies(c.facts, lowerE)) and
             (not needHi or implies(c.facts, upperE))
    invalidateFactsAbout(c.facts, slot)
    if ok: return

  # `m * a + o` for constants `m > 0` and `o`: `lo <= m*a + o <= hi` holds
  # exactly when `ceil((lo - o) / m) <= a <= floor((hi - o) / m)`. What a digit
  # table indexed by `2 * digits + 1` owes.
  if sym == NoSymId:
    var lin = value
    var linLo = zero()
    var linHi = zero()
    while lin.exprKind in {HconvX, ConvX}:
      var d = lin
      d = sub(d)
      if integerTypeBounds(c.bits, d, linLo, linHi) and linLo <= lo and hi <= linHi:
        skip d
        lin = d
      else:
        break
    var offset = zero()
    if lin.exprKind == AddX:
      var d = lin
      d = sub(d)
      skip d # the type operand
      let first = d
      skip d
      let k = tryEvalOrdinal(c.bits, d)
      if not k.isNaN:
        offset = k
        lin = first
    if lin.exprKind == MulX:
      var d = lin
      d = sub(d)
      skip d # the type operand
      var factor = tryEvalOrdinal(c.bits, d)
      skip d
      var operand = d
      if factor.isNaN:
        factor = tryEvalOrdinal(c.bits, d)
        operand = lin
        operand = sub(operand)
        skip operand # the type operand
      let loc = plainLocationVarId(c, operand)
      if loc != InvalidVarId and not factor.isNaN and zero() < factor:
        let one = createXint(1'i64)
        # floor division for the upper bound, ceiling division for the lower one
        let upper = hi - offset
        let maxA = if upper < zero(): -((-upper + factor - one) div factor) else: upper div factor
        let lower = lo - offset
        let minA = if lower <= zero(): -((-lower) div factor) else: (lower + factor - one) div factor
        var aLo = zero()
        var aHi = zero()
        # the type-given lower bound only counts once the upper one rules out a wrap
        let typeLow = needHi and
          integerTypeBounds(c.bits, getType(c.typeCache, operand), aLo, aHi) and minA <= aLo
        if (not needLo or typeLow or impliesHere(c, query(VarId(0), loc, -minA))) and
           (not needHi or impliesHere(c, query(loc, VarId(0), maxA))):
          return

  # `-a`: `lo <= -a <= hi` is `-hi <= a <= -lo`, which is what binds
  # `NegTen[int(-x)]` under `if x < 0: if x > -10:`.
  var negValue = value
  block:
    var tLo = zero()
    var tHi = zero()
    if negValue.exprKind in {HconvX, ConvX}:
      var d = negValue
      d = sub(d)
      if integerTypeBounds(c.bits, d, tLo, tHi) and tLo <= lo and hi <= tHi:
        skip d
        negValue = d
  if sym == NoSymId and negValue.exprKind == NegX:
    var d = negValue
    d = sub(d)
    skip d # the type operand
    let operand = plainLocationVarId(c, d)
    if operand != InvalidVarId and
       (not needLo or impliesHere(c, query(operand, VarId(0), -lo))) and
       (not needHi or impliesHere(c, query(VarId(0), operand, hi))):
      return

  # `a shr k` (arithmetic or logical) and `a and k`, answered structurally:
  # neither is expressible as `a <= b + c`. This is what binds the halving step
  # `y = y shr 1` and a masked index `x and 63` to a `Natural`.
  # `a div k` and `a mod k` for a constant `k > 0` and a non-negative `a`: the
  # quotient stays non-negative and below `(hi + 1) * k`, the remainder lies in
  # `0 .. k-1`. What a digit loop `x = x div 10` owes a `Natural`.
  if sym == NoSymId and value.exprKind in {DivX, ModX}:
    var d = value
    d = sub(d)
    skip d # the type operand
    let leftLoc = plainLocationVarId(c, d)
    skip d
    let k = tryEvalOrdinal(c.bits, d)
    if leftLoc != InvalidVarId and not k.isNaN and zero() < k and
       impliesHere(c, query(VarId(0), leftLoc, zero())):
      let one = createXint(1'i64)
      let ok =
        if value.exprKind == ModX:
          (not needLo or lo <= zero()) and (not needHi or k - one <= hi)
        else:
          (not needLo or lo <= zero()) and
            (not needHi or (zero() <= hi and
              impliesHere(c, query(leftLoc, VarId(0), (hi + one) * k - one))))
      if ok: return

  if sym == NoSymId and value.exprKind in {ShrX, AshrX, BitandX}:
    let isShift = value.exprKind in {ShrX, AshrX}
    var d = value
    d = sub(d)
    skip d # the type operand
    let leftLoc = plainLocationVarId(c, d)
    skip d
    var k = createNaN()
    case d.kind
    of IntLit: k = createXint(d.intVal)
    of UIntLit: k = createXint(d.uintVal)
    else: k = tryEvalOrdinal(c.bits, d)
    if not k.isNaN and zero() <= k:
      var ok = false
      if not isShift:
        # `a and k` keeps only bits of `k`: for `k >= 0` it lies in `0..k`,
        # whatever `a` is.
        ok = (not needLo or lo <= zero()) and (not needHi or k <= hi)
      elif leftLoc != InvalidVarId and impliesHere(c, query(VarId(0), leftLoc, zero())):
        # A non-negative `a` shifts to a non-negative value, and `a shr k <= hi`
        # holds exactly when `a <= ((hi + 1) shl k) - 1`.
        var err = false
        let shift = asSigned(k, err)
        let bound = if err or shift > 62 or hi < zero(): createNaN()
                    else: ((hi + createXint(1'i64)) shl int(shift)) - createXint(1'i64)
        ok = (not needLo or lo <= zero()) and
             (not needHi or (not bound.isNaN and
                             impliesHere(c, query(leftLoc, VarId(0), bound))))
      if ok: return
      cannotProve c, value.info, reportUnprovable, "cannot prove '" & asNimCode(value) &
        "' is in range " & $lo & ".." & $hi
      return

  # `a + b` between two locations. The sum itself is outside what `a <= b + c`
  # can say, but a *lower* bound on it follows from lower bounds on the parts:
  # `0 <= a` and `lo <= b` give `lo <= a + b`. That is what binds
  # `o = o + runeLenAt(s, o)` to a `Natural`, once `runeLenAt` says it returns a
  # `Positive`. An upper bound on a sum stays out of reach, so this only applies
  # where the target's upper bound is the base type's own.
  if sym == NoSymId and value.exprKind == AddX and not needHi:
    var d = value
    d = sub(d)
    skip d # the type operand
    let left = plainLocationVarId(c, d)
    skip d
    let right = plainLocationVarId(c, d)
    if left != InvalidVarId and right != InvalidVarId:
      let zeroLeft = query(VarId(0), left, zero())
      let loRight = query(VarId(0), right, -lo)
      let zeroRight = query(VarId(0), right, zero())
      let loLeft = query(VarId(0), left, -lo)
      if not needLo or
         (impliesHere(c, zeroLeft) and impliesHere(c, loRight)) or
         (impliesHere(c, zeroRight) and impliesHere(c, loLeft)):
        return
      cannotProve c, value.info, reportUnprovable, "cannot prove '" & asNimCode(value) &
        "' is in range " & $lo & ".." & $hi
      return

  # `a - b` between two locations. A sum of two variables is outside what
  # `a <= b + c` can say, but a *difference* is exactly inside it:
  # `lo <= a - b` is `b <= a - lo`, and `a - b <= hi` is `a <= b + hi`. That is
  # what lets a `Natural` computed as a difference — `result = j - i` with
  # `i <= j` known — be bound without a cast.
  if sym == NoSymId and value.exprKind == SubX:
    var d = value
    d = sub(d)
    skip d # the type operand
    let left = plainLocationVarId(c, d)
    skip d
    let right = plainLocationVarId(c, d)
    if left != InvalidVarId and right != InvalidVarId:
      let lower = query(right, left, -lo)
      let upper = query(left, right, hi)
      if (not needLo or impliesHere(c, lower)) and
         (not needHi or impliesHere(c, upper)):
        return
      cannotProve c, value.info, reportUnprovable, "cannot prove '" & asNimCode(value) &
        "' is in range " & $lo & ".." & $hi
      return

  case value.kind
  of IntLit: off = createXint(value.intVal); isLit = true
  of UIntLit: off = createXint(value.uintVal); isLit = true
  else:
    # Folding is tried BEFORE the symbol path: a `const` is a `Symbol` here
    # like any local, and treating it as an opaque variable left `Label(High)`
    # with `const High = 255` unprovable — the literal spelling of the same
    # value proved instantly. `tryEvalOrdinal` returns NaN for anything that is
    # not compile-time constant, so a genuine variable still takes the path
    # below.
    let folded = tryEvalOrdinal(c.bits, value)
    if not folded.isNaN:
      off = folded
      isLit = true
    elif sym != NoSymId:
      v = getVarId(c, sym)
    elif (let loc = plainLocationVarId(c, value); loc != InvalidVarId):
      # a derived location — `a.dp`, `s.len` — is judged exactly like a variable
      v = loc
    else:
      # A value we cannot model cannot be proven in range, so we reject it.
      cannotProve c, value.info, reportUnprovable, "cannot prove value is in range " & $lo & ".." & $hi
      return

  # lo <= v + off   <=>   0 <= v + (off - lo)
  let lower = query(VarId(0), v, off - lo)
  # v + off <= hi   <=>   v <= 0 + (hi - off)
  let upper = query(v, VarId(0), hi - off)
  let upperOk = not needHi or impliesHere(c, upper)
  # A symbol of an unsigned type is non-negative by its type, which no flow fact
  # states: `if x < 10: NegTen[int x]` with `x: uint64`. Through a conversion
  # that only holds once the upper bound is proven too — `int(x)` of a huge
  # `x` wraps — which is what the `needHi` requirement buys.
  var typeLowerOk = false
  if needLo and sym != NoSymId and not isLit:
    var sc = value
    while sc.exprKind in {HconvX, ConvX, BaseobjX}:
      inc sc
      skip sc
    let converted = value.exprKind in {HconvX, ConvX, BaseobjX}
    var symLo = zero()
    var symHi = zero()
    typeLowerOk = integerTypeBounds(c.bits, getType(c.typeCache, sc), symLo, symHi) and
      lo <= symLo + off and (not converted or (needHi and upperOk))
  if not ((not needLo or typeLowerOk or impliesHere(c, lower)) and upperOk):
    if isLit:
      buildErr c, value.info, "value out of range: " & $off & " notin " & $lo & ".." & $hi
    elif sym != NoSymId:
      cannotProve c, value.info, reportUnprovable, "cannot prove '" & asNimCode(sym) &
        "' is in range " & $lo & ".." & $hi
    else:
      cannotProve c, value.info, reportUnprovable, "cannot prove value is in range " & $lo & ".." & $hi

proc checkRangeAssign(c: var FirContext; targetType, value: Cursor) =
  ## Emit and discharge the `lo <= value <= hi` obligation for a value bound to a
  ## `range[lo..hi]`-typed target. Value conversions are handled at the
  ## conversion site (see the `ConvX`/`HconvX` case in `traverseExpr`), so we
  ## skip them here to avoid double-reporting.
  if value.exprKind in {ConvX, HconvX, CastX, BaseobjX}: return
  var lo = zero()
  var hi = zero()
  if not staticRangeBounds(targetType, lo, hi): return
  # A bound that coincides with the base type's own is nothing to prove.
  var baseLo = zero()
  var baseHi = zero()
  var needLo = true
  var needHi = true
  if baseTypeBounds(targetType, c.bits, baseLo, baseHi):
    needLo = lo > baseLo
    needHi = hi < baseHi
  if not needLo and not needHi: return
  checkInRange c, value, lo, hi, needLo, needHi

proc seedRangeFacts(c: var FirContext; sym: SymId; typ: Cursor) =
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
    # Also as a *type* bound, which survives the widening a loop applies to the
    # flow facts.
    if not c.declaredRange.hasKey(v):
      c.declaredRange[v] = RangeBounds(lo: lo, hi: hi)

# --- Fact extraction from conditions ---

proc constOrdinal(c: var FirContext; n: Cursor; val: var xint): bool =
  ## True when `n` is a compile-time ordinal.
  ##
  ## A `const` survives into the Final IR as a plain `Symbol`, so a guard
  ## written against a named bound — `if i <= MaxLabel:` — used to record a
  ## fact about an opaque variable and proved nothing, while the very same
  ## guard spelled `if i <= 255:` proved instantly. Folding it here is what
  ## makes the named form work. `tryEvalOrdinal` answers NaN for anything that
  ## is not constant, so an ordinary variable still takes the symbol path.
  ##
  ## Not only a symbol: an arithmetic expression over constants is one too, and
  ## that is what an array's `len` is. Sem folds `a.len` to `(hi - lo) + 1` over
  ## the index range's own literals rather than to a single number, so the bound
  ## of `while i < a.len` arrives here as a three-node tree — and until it folds,
  ## the most ordinary array loop there is proves nothing about its index.
  result = false
  if n.isTagLit and n.exprKind in {ConvX, HconvX} and c.constDepth < 8:
    # `ord(high(E))` is `(conv (i 64) lastField)`
    inc c.constDepth
    var r = n
    r = sub(r)
    let target = r
    skip r # the target type
    result = constOrdinal(c, r, val)
    if result:
      # only a value the target holds unchanged: `uint8(300)` is not 300
      var tLo = zero()
      var tHi = zero()
      if integerTypeBounds(c.bits, target, tLo, tHi):
        result = tLo <= val and val <= tHi
    dec c.constDepth
    if result: return
  if n.isTagLit and n.exprKind in {NegX, AddX, SubX, MulX, SufX} and c.constDepth < 8:
    # arithmetic over constants, named ones included: `-Log2Pow10Len`
    inc c.constDepth
    var r = n
    r = sub(r)
    if n.exprKind == SufX:
      result = constOrdinal(c, r, val)
    else:
      skip r # the type operand
      var a = zero()
      if constOrdinal(c, r, a):
        if n.exprKind == NegX:
          val = -a
          result = not val.isNaN
        else:
          skip r
          var b = zero()
          if constOrdinal(c, r, b):
            val = case n.exprKind
                  of AddX: a + b
                  of SubX: a - b
                  else: a * b
            result = not val.isNaN
    dec c.constDepth
    if result: return
  if n.kind == Symbol and c.moduleConsts.hasKey(n.symId) and c.constDepth < 8:
    # declared in this module: fold its own value (bounded — a `const` defined
    # through another one nests); the evaluator below still gets its turn
    inc c.constDepth
    result = constOrdinal(c, c.moduleConsts.getOrQuit(n.symId), val)
    dec c.constDepth
    if result: return
  val = tryEvalOrdinal(c.bits, n)
  result = not val.isNaN

proc operandRange(c: var FirContext; n: Cursor; lo, hi: var xint) =
  ## The tightest bounds the facts give an integer operand — a literal, a
  ## constant or a location — NaN for an end nothing bounds.
  lo = createNaN()
  hi = createNaN()
  var k = zero()
  if constOrdinal(c, n, k):
    lo = k
    hi = k
    return
  let v = plainLocationVarId(c, n)
  if v == InvalidVarId or v == VarId(0): return
  var known = snapshotFacts(c.facts)
  if c.declaredRange.hasKey(v):
    let r = c.declaredRange.getOrQuit(v)
    known.add query(VarId(0), v, -r.lo)
    known.add query(v, VarId(0), r.hi)
  let up = boundsFrom(known, v)                 # v <= 0 + d
  if up.hasKey(VarId(0)): hi = up.getOrQuit(VarId(0))
  let down = boundsFrom(known, v, reverse = true) # 0 <= v + d
  if down.hasKey(VarId(0)): lo = -down.getOrQuit(VarId(0))

proc rightHandSide(c: var FirContext; pc: var Cursor; fact: var LeXplusC;
                   call: var CallContext): bool =
  result = false
  var cval = createXint(0'i32)
  if constOrdinal(c, pc, cval):
    fact.b = VarId(0)
    fact.c = fact.c + cval
    result = true
    skip pc
  elif pc.exprKind in {AddX, SubX}:
    # The sign matters: `i <= s.len - 2` recorded as `i <= s.len + 2` is a
    # different fact, and once `s.len` became a location the engine can name
    # (rather than an opaque call it ignored) the difference started deciding
    # real contracts — `if i <= s.len - 2: use(s[i+1])` among them.
    let isSub = pc.exprKind == SubX
    pc.into:
      skip pc # type
      let loc2 = plainLocationVarId(c, pc)
      if loc2 != InvalidVarId:
        analyseIfDerived(c, pc, call)
        skip pc
        fact.b = loc2
        var k = createNaN()
        if not constOrdinal(c, pc, k): k = createNaN()
        if not k.isNaN:
          fact.c = fact.c + (if isSub: -k else: k)
          result = true
          skip pc
        else:
          traverseExpr c, pc, call
      else:
        # `k + loc`, the constant first: `absExponent <= 22 + slop`
        var k = createNaN()
        let constFirst = not isSub and constOrdinal(c, pc, k)
        if constFirst:
          skip pc
          let loc3 = plainLocationVarId(c, pc)
          if loc3 != InvalidVarId:
            analyseIfDerived(c, pc, call)
            fact.b = loc3
            fact.c = fact.c + k
            result = true
            skip pc
          else:
            traverseExpr c, pc, call
        else:
          traverseExpr c, pc, call
          traverseExpr c, pc, call
  elif (let loc = plainLocationVarId(c, pc); loc != InvalidVarId):
    fact.b = loc
    analyseIfDerived(c, pc, call)
    skip pc
    result = true
  elif (var ov = VarId(0); var ok = zero(); offsetAccessor(c, pc, initTable[SymId, Cursor](), ov, ok)):
    fact.b = ov
    fact.c = fact.c + ok
    skip pc
    result = true
  elif pc.isIntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pc.intVal)
    result = true
    inc pc
  elif pc.kind == UIntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pc.uintVal)
    result = true
    inc pc
  elif pc.exprKind == NilX:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(0'i32)
    result = true
    skip pc
  else:
    traverseExpr c, pc, call

proc leftHandStep(c: var FirContext; n: Cursor; fact: var LeXplusC;
                  call: var CallContext): bool =
  ## `v ± k` as the left operand of a comparison: `v + k <= b + c` is
  ## `v <= b + (c - k)`, so the constant moves across with its sign flipped.
  let isSub = n.exprKind == SubX
  var r = n
  r = sub(r)
  skip r # the type operand
  let v = plainLocationVarId(c, r)
  if v == InvalidVarId: return false
  analyseIfDerived(c, r, call)
  skip r
  var k = createNaN()
  case r.kind
  of IntLit: k = createXint(r.intVal)
  of UIntLit: k = createXint(r.uintVal)
  else: k = tryEvalOrdinal(c.bits, r)
  if k.isNaN: return false
  fact.a = v
  fact.c = fact.c + (if isSub: k else: -k)
  result = true

type
  CondKind = enum
    ckPlain       ## an ordinary `<=`/`<` comparison, or a truthy not-nil test
    ckEquality    ## `a == b`: true means *both* `a <= b` and `a >= b`
    ckDisequality ## `a != b`: `a <= b + c` cannot express it at all. Reading it
                  ## as the negation of one direction is not merely imprecise,
                  ## it is wrong — `if x != 3` used to yield `x >= 4`, which
                  ## "proved" `x > 3` on a path where `x` may well be 1.
    ckImplied     ## a consequence of the comparison, not the comparison: an
                  ## operand was replaced by its interval (`d - 1 > (s and 1)`
                  ## gives `d - 1 > 0`), so the false path learns nothing
    ckImpliedOnFalse ## `ckImplied` under an odd number of `not`s: the fact is
                     ## a consequence of the *false* path, and negating it would
                     ## not be one of the true path

proc translateCond(c: var FirContext; pc: var Cursor; kind: var CondKind;
                   call: var CallContext): LeXplusC =
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
    # An odd number of `not`s turns the equality into a disequality; the fact
    # built below is then the *equality*, which holds on the FALSE path.
    kind = if (negations and 1) == 0: ckEquality else: ckDisequality
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
      traverseExpr c, pc, call
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
      traverseExpr c, pc, call
    return result

  if r.isIntLit:
    result.a = VarId(0)
    result.c = -createXint(r.intVal)
    inc r
  elif r.kind == UIntLit:
    result.a = VarId(0)
    result.c = -createXint(r.uintVal)
    inc r
  elif (var lval = createXint(0'i32); constOrdinal(c, r, lval)):
    result.a = VarId(0)
    result.c = -lval
    skip r
  elif (let la = plainLocationVarId(c, r); la != InvalidVarId):
    result.a = la
    analyseIfDerived(c, r, call)
    skip r
  elif (var ov = VarId(0); var ok = zero(); offsetAccessor(c, r, initTable[SymId, Cursor](), ov, ok)):
    # `high(s) <= b + c` is `s.len <= b + c + 1`
    result.a = ov
    result.c = result.c - ok
    skip r
  elif r.exprKind in {AddX, SubX} and leftHandStep(c, r, result, call):
    # `i + 3 <= n` is `i <= n - 3`. Only the *right* side of a comparison used
    # to be read arithmetically, so the loop `while i + 3 <= n:` — which is how
    # a chunked walk states its bound, `base64.encode` among them — proved
    # nothing at all about `i`.
    skip r
  elif r.exprKind == NilX:
    result.a = VarId(0)
    skip r
  elif xk in {LeX, LtX} and r.exprKind in {BitandX, ShrX, AshrX} and
      (var leftLo = zero(); var leftHi = zero(); valueBounds(c, r, leftLo, leftHi)):
    # `e <= b + c` with `lo <= e` implies `lo <= b + c`
    result.a = VarId(0)
    result.c = -leftLo
    kind = ckImplied
    skip r
  else:
    traverseExpr c, pc, call
    return result
  if r.exprKind == NilX and kind != ckImplied:
    kind = ckPlain
  if xk in {LeX, LtX} and kind != ckImplied and r.exprKind in {BitandX, ShrX, AshrX} and
      (var rightLo = zero(); var rightHi = zero(); valueBounds(c, r, rightLo, rightHi)):
    # `a <= e` with `e <= hi` implies `a <= hi`
    result.b = VarId(0)
    result.c = result.c + rightHi
    kind = ckImplied
    skip r
  elif not rightHandSide(c, r, result, call):
    result.a = InvalidVarId
  # a < b  --> a <= b - 1:
  if xk == LtX:
    result.c = result.c - createXint(1'i32)
  if xk in {LeX, LtX, EqX}: r = cmpStart; skip r

  if kind in {ckDisequality, ckImplied}:
    # Hand back the *equality* un-negated; `analyseCondition` knows to assume it
    # on the false path and to assume nothing on the true one. An implied fact
    # stays un-negated too, and moves to the false path under an odd count.
    if kind == ckImplied and (negations and 1) == 1: kind = ckImpliedOnFalse
    while negations > 0:
      dec negations
      let h = notScopes.pop(); r = h; skip r
  else:
    unwindNegations()

  pc = r

proc strictDisequality(c: var FirContext; eq: LeXplusC): LeXplusC =
  ## `a != b + c` where `a >= b + c` is known is `a >= b + c + 1`, and where
  ## `a <= b + c` is known, `a <= b + c - 1`: `s.len != 0` beside `0 <= s.len`.
  ## Invalid when neither direction is known.
  result = LeXplusC(a: InvalidVarId, b: InvalidVarId, c: zero())
  if impliesHere(c, eq.geXplusC):
    result = eq
    negateFact(result)
  elif impliesHere(c, eq):
    result = eq.geXplusC
    negateFact(result)

proc analyseCondition(c: var FirContext; pc: var Cursor;
                      elseFacts: var seq[LeXplusC]; call: var CallContext): int =
  ## Returns number of facts added
  if pc.exprKind == AndX:
    # `a and b`: on the true path BOTH conjuncts hold, so both are facts.
    # `translateCond` only understands a single comparison, so a conjunction
    # used to contribute none at all and the everyday guard
    # `if i >= 0 and i <= High: …` proved nothing inside its own branch.
    # The else-path stays correct because `traverseIte` assumes the negation
    # only when exactly one fact came out — never for a conjunction, whose
    # negation is not the conjunction of the negations.
    let start = pc
    var r = sub(pc)
    result = analyseCondition(c, r, elseFacts, call)
    result = result + analyseCondition(c, r, elseFacts, call)
    # The negation of a conjunction is not the conjunction of the negations, so
    # the false path of an `and` learns nothing.
    elseFacts.setLen 0
    pc = start
    skip pc
    return result
  var kind = ckPlain
  let fact = translateCond(c, pc, kind, call)
  if not fact.isValid:
    return 0
  case kind
  of ckPlain:
    c.facts.add fact
    var negated = fact
    negateFact(negated)
    elseFacts.add negated
    result = 1
  of ckImplied:
    c.facts.add fact
    result = 1
  of ckImpliedOnFalse:
    elseFacts.add fact
    result = 0
  of ckEquality:
    # `not (a == b)` is a disequality, which states nothing by itself — but
    # next to a bound already known in one direction it is strict in that one.
    # Judged before the equality becomes a fact: afterwards both directions are
    # "known" and the false path would be handed a contradiction.
    let strict = strictDisequality(c, fact)
    if strict.isValid: elseFacts.add strict
    c.facts.add fact
    c.facts.add fact.geXplusC
    result = 2
  of ckDisequality:
    # On the false path the equality holds in both directions; the true path
    # learns only what `strictDisequality` can make of it.
    elseFacts.add fact
    elseFacts.add fact.geXplusC
    let strict = strictDisequality(c, fact)
    if strict.isValid:
      c.facts.add strict
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

proc analysableRoot(c: var FirContext; n: Cursor): SymId =
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

proc isNonNilExpr(c: var FirContext; n: Cursor): bool =
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

proc wantNotNil(c: var FirContext; n: Cursor) =
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
          if n.isSymbol and pool.symString(n.symId) == ("Success.0." & SystemModuleSuffix):
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

proc checkNilMatch(c: var FirContext; n: Cursor; expected: Cursor) =
  if markedAs(expected, NotnilU):
    wantNotNil c, n

proc wantNotNilDeref(c: var FirContext; n: Cursor) =
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

# `.requires` is discharged at the *call site*: the formal contract is read with
# every parameter standing for the argument the caller actually wrote, and the
# result is handed to the same `inferle` fact base every other obligation uses.
#
# The substitution is done by *lookup while reading* (`argOf`) rather than by
# building a rewritten copy of the contract: the contract and the arguments live
# in the same buffer, so a `Table[SymId, Cursor]` from parameter to argument is
# all a reader needs, and nothing has to be re-interned.
#
# Everything below is deliberately *pure*: unlike `translateCond`, which falls
# back to `traverseExpr` for operands it cannot model, these routines must not
# analyse what they walk — the arguments have already been analysed by the
# caller's own traversal, and doing it twice would double-report their errors.
# An operand that is not understood therefore yields "no fact", never an error.

proc oldArg(n: Cursor; arg: var Cursor): bool =
  ## Is `n` a call of `system.old`, the pre-call value of a location inside an
  ## `.ensures`? Recognized by its `semantics` pragma, not by its name.
  result = false
  if n.exprKind in CallKinds:
    var r = n
    r = sub(r)
    let fnSym = extractSymId(r)
    if fnSym != NoSymId and pool.symBasename(fnSym) == "old":
      skip r # the callee
      if r.hasMore:
        arg = r
        skip r
        if not r.hasMore:
          let sym = tryLoadSym(fnSym)
          result = sym.status == LacksNothing and isRoutine(sym.decl.symKind) and
            hasPragmaOfValue(asRoutine(sym.decl).pragmas, SemanticsP, "old")

proc constStringLen(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor];
                    length: var xint): bool =
  ## `len(s)` of a `const` string: a number, not a location. `HexChars[i]` with
  ## `const HexChars = "0123456789ABCDEF"` owes `i < len(HexChars)`, and only
  ## the literal's length discharges that.
  result = false
  if n.exprKind in CallKinds:
    var r = n
    r = sub(r)
    let fnSym = extractSymId(r)
    if fnSym != NoSymId and pool.symBasename(fnSym) == "len":
      skip r # the callee
      if r.hasMore:
        let strSym = extractSymId(argOf(r, paramMap))
        skip r
        if strSym != NoSymId and not r.hasMore:
          var value = default(Cursor)
          if c.moduleConsts.hasKey(strSym):
            # declared in this module, which `tryLoadSym` cannot load yet
            value = c.moduleConsts.getOrQuit(strSym)
          else:
            let sym = tryLoadSym(strSym)
            if sym.status == LacksNothing and sym.decl.symKind == ConstY:
              value = asLocal(sym.decl).val
          if not cursorIsNil(value) and value.kind == StrLit:
            length = createXint(int64(pool.strings[value.strId].len))
            result = true

proc pureOperand(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor];
                 v: var VarId; cnst: var xint): bool =
  ## One side of a comparison as `v + cnst`: an integer literal, a compile-time
  ## constant, `nil`, a symbol, or `sym +/- k`. `v` is `VarId(0)` for a pure
  ## constant — the engine's convention is that variable zero *is* the number
  ## zero, which is also what makes `nil` and `0` the same operand.
  v = VarId(0)
  cnst = createXint(0'i32)
  if c.substVar.len > 0:
    let rs = extractSymId(peelExpr(n))
    if rs != NoSymId and c.substVar.hasKey(rs):
      v = c.substVar.getOrQuit(rs)
      return true
  let m = argOf(n, paramMap)
  case m.kind
  of IntLit:
    cnst = createXint(m.intVal)
    return true
  of UIntLit:
    cnst = createXint(m.uintVal)
    return true
  else: discard
  if m.exprKind == NilX:
    return true
  var folded = createXint(0'i32)
  if constOrdinal(c, m, folded):
    # A named `const` is a plain `Symbol` here; fold it before treating it as an
    # opaque variable, exactly as `checkRangeAssign` does.
    cnst = folded
    return true
  if m.exprKind in {AddX, SubX}:
    let isSub = m.exprKind == SubX
    var r = m
    r = sub(r)
    skip r # the type operand
    # the base is an operand of its own: a location, `old(e)`, `high(s)`, or a
    # literal a parameter stands for at this call site (`i + 2` with `i = 0`)
    var base = VarId(0)
    var baseConst = zero()
    if not pureOperand(c, r, paramMap, base, baseConst): return false
    skip r
    let k = argOf(r, paramMap)
    var off = createXint(0'i32)
    case k.kind
    of IntLit: off = createXint(k.intVal)
    of UIntLit: off = createXint(k.uintVal)
    else:
      off = tryEvalOrdinal(c.bits, k)
      if off.isNaN: return false
    v = base
    cnst = baseConst + (if isSub: -off else: off)
    return true
  var oldOf = default(Cursor)
  if oldArg(m, oldOf):
    # The snapshot `snapshotOlds` took before the call's mutations.
    var root = NoSymId
    var key = ""
    var steps = 0
    if locationKey(c, oldOf, paramMap, root, key, steps, 0) and c.oldSlots.hasKey(key):
      v = c.oldSlots.getOrQuit(key)
      return true
    return false
  if constStringLen(c, m, paramMap, cnst):
    return true
  if offsetAccessor(c, n, paramMap, v, cnst):
    return true
  let loc = locationVarId(c, n, paramMap)
  if loc != InvalidVarId:
    v = loc
    return true
  result = false

proc pureCompare(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor];
                 wasEquality: var bool): LeXplusC =
  ## Translate one comparison of the contract into `a <= b + c`. An invalid
  ## result means "not modelled", never "false".
  result = LeXplusC(a: InvalidVarId, b: VarId(0), c: createXint(0'i32))
  let xk = n.exprKind
  if xk notin {LeX, LtX, EqX}:
    # A bare truthy operand: `requires: p` on a `ref`/`ptr` means `p != nil`.
    var v = VarId(0)
    var k = createXint(0'i32)
    if pureOperand(c, n, paramMap, v, k) and v != VarId(0) and k == createXint(0'i32):
      result = isNotNil(v)
    return result
  wasEquality = xk == EqX
  var r = n
  r = sub(r)
  skip r # the type operand
  var va = VarId(0)
  var ka = createXint(0'i32)
  if not pureOperand(c, r, paramMap, va, ka): return result
  skip r
  var vb = VarId(0)
  var kb = createXint(0'i32)
  if not pureOperand(c, r, paramMap, vb, kb): return result
  # `va + ka <= vb + kb`  <->  `va <= vb + (kb - ka)`
  result = LeXplusC(a: va, b: vb, c: kb - ka)
  if xk == LtX:
    result.c = result.c - createXint(1'i32)

proc proveFact(c: var FirContext; fact: LeXplusC): ProofRes =
  ## Three-valued and *precise*: `Disproven` is reserved for a fact whose
  ## negation the known facts imply. Conflating "we failed to prove it" with
  ## "it is false" would make `not` unsound and would turn every contract the
  ## engine merely cannot model into a reported violation.
  if not fact.isValid: return Unprovable
  if impliesHere(c, fact): return Proven
  var neg = fact
  negateFact(neg)
  if impliesHere(c, neg): return Disproven
  result = Unprovable

proc operandInterval(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor];
                     lo, hi: var xint): bool =
  ## The interval a comparison operand lies in without any flow fact: a constant
  ## (`pureOperand` with no variable part) or a bounded shape (`valueBounds`).
  var v = VarId(0)
  var k = zero()
  if pureOperand(c, n, paramMap, v, k) and v == VarId(0):
    lo = k
    hi = k
    result = true
  else:
    result = valueBounds(c, argOf(n, paramMap), lo, hi)

proc proveByIntervals(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor]): ProofRes =
  ## A comparison `pureCompare` cannot state as `a <= b + c`, decided by the
  ## intervals of its operands: `(n and 0xF0) shr 4 < len(HexChars)` is
  ## `0..15 < 16`.
  result = Unprovable
  let k = n.exprKind
  if k in {LeX, LtX, EqX}:
    var r = n
    r = sub(r)
    skip r # the type operand
    var aLo = zero()
    var aHi = zero()
    var bLo = zero()
    var bHi = zero()
    if operandInterval(c, r, paramMap, aLo, aHi):
      skip r
      if operandInterval(c, r, paramMap, bLo, bHi):
        case k
        of LeX:
          if aHi <= bLo: result = Proven
          elif bHi < aLo: result = Disproven
        of LtX:
          if aHi < bLo: result = Proven
          elif bHi <= aLo: result = Disproven
        else:
          if aLo == aHi and bLo == bHi and aLo == bLo: result = Proven
          elif aHi < bLo or bHi < aLo: result = Disproven

proc proveMasked(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor]): ProofRes =
  ## `x and m <= b` / `x and m < b` / `k <= x and m` for a mask `m` known not
  ## to be negative: `x and m` lies in `0 .. m`, so the upper question is asked
  ## about `m` and the lower one about `0`. A ring buffer indexes by
  ## `s.data[s.tail and (s.data.len - 1)]`.
  result = Unprovable
  let k = n.exprKind
  if k notin {LeX, LtX}: return
  var r = n
  r = sub(r)
  skip r # the type operand
  let left = argOf(r, paramMap)
  skip r
  let right = argOf(r, paramMap)
  let strict = if k == LtX: createXint(1'i64) else: zero()
  if left.exprKind == BitandX:
    var rv = VarId(0)
    var rk = zero()
    if not pureOperand(c, right, paramMap, rv, rk): return
    var d = left
    d = sub(d)
    skip d # the type operand
    for _ in 0 ..< 2:
      var mv = VarId(0)
      var mk = zero()
      if pureOperand(c, d, paramMap, mv, mk) and
          impliesHere(c, query(VarId(0), mv, mk)):
        # `m <= b - strict` is `mv + mk <= rv + rk - strict`
        if impliesHere(c, query(mv, rv, rk - mk - strict)): return Proven
      skip d
  elif right.exprKind == BitandX:
    var lv = VarId(0)
    var lk = zero()
    if not pureOperand(c, left, paramMap, lv, lk) or lv != VarId(0): return
    # `k <= x and m` holds for `k <= 0` once `m` is not negative
    if lk + strict <= zero():
      var d = right
      d = sub(d)
      skip d # the type operand
      for _ in 0 ..< 2:
        var mv = VarId(0)
        var mk = zero()
        if pureOperand(c, d, paramMap, mv, mk) and
            impliesHere(c, query(VarId(0), mv, mk)):
          return Proven
        skip d

proc proveCond(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor]): ProofRes =
  case n.exprKind
  of AndX:
    var r = sub(n)
    let a = proveCond(c, r, paramMap)
    skip r
    result = a and proveCond(c, r, paramMap)
  of OrX:
    var r = sub(n)
    let a = proveCond(c, r, paramMap)
    skip r
    result = a or proveCond(c, r, paramMap)
  of NotX:
    var r = sub(n)
    result = not proveCond(c, r, paramMap)
  of TrueX:
    result = Proven
  of FalseX:
    result = Disproven
  of ExprX:
    var r = n
    while r.exprKind == ExprX:
      r = sub(r) # throwaway copy; bounds the walk under vpr
      while r.hasMore and not isLastSon(r): skip r
    result = proveCond(c, r, paramMap)
  else:
    var wasEquality = false
    let fact = pureCompare(c, n, paramMap, wasEquality)
    if fact.isValid:
      result = proveFact(c, fact)
      if wasEquality and result == Proven:
        result = proveFact(c, fact.geXplusC)
    else:
      result = proveMasked(c, n, paramMap)
      if result == Unprovable:
        result = proveByIntervals(c, n, paramMap)
    when defined(contractLeaves):
      # `-d:contractLeaves` adds the per-conjunct verdict to `-d:contractStats`,
      # which is what tells "the index has no proven lower bound" apart from
      # "no proven upper bound" — a distinction that decides whether a site
      # wants a non-negative index *type* or a guard.
      stderr.writeLine "   LEAF " & $result & " " & asNimCode(n)

proc assumeCond(c: var FirContext; n: Cursor; subst: Table[SymId, Cursor]) =
  ## Record a proposition as fact. Two callers: a routine's own `.requires` on
  ## entry to its body — every call site had to discharge it, so the body may
  ## assume it, which is what lets a precondition be passed on to an inner call
  ## that demands the same thing — and a callee's `.ensures` at the call site.
  ## `subst` maps the proposition's parameters (and `result`) to what they stand
  ## for here; it is empty for the `.requires` case, where the symbols already
  ## are the ones to reason about. Whatever is not modelled contributes no fact.
  case n.exprKind
  of AndX:
    var r = sub(n)
    assumeCond(c, r, subst)
    skip r
    assumeCond(c, r, subst)
  of NotX:
    var r = sub(n)
    var wasEquality = false
    var fact = pureCompare(c, r, subst, wasEquality)
    if fact.isValid and not wasEquality:
      negateFact(fact)
      c.facts.add fact
  of ExprX:
    var r = n
    while r.exprKind == ExprX:
      r = sub(r)
      while r.hasMore and not isLastSon(r): skip r
    assumeCond(c, r, subst)
  else:
    var wasEquality = false
    let fact = pureCompare(c, n, subst, wasEquality)
    if fact.isValid:
      c.facts.add fact
      if wasEquality:
        c.facts.add fact.geXplusC
    elif n.exprKind in {LeX, LtX}:
      # One side is not a location but has a known interval: the comparison
      # still bounds the other side. `i <= d.count - 1` with a `range`-typed
      # `count` behind a pointer says `i <= high(count) - 1`.
      let strict = if n.exprKind == LtX: createXint(1'i64) else: zero()
      var r = n
      r = sub(r)
      skip r # the type operand
      let lhs = r
      skip r
      let rhs = r
      var v = VarId(0)
      var k = zero()
      var lo = zero()
      var hi = zero()
      if pureOperand(c, lhs, subst, v, k) and v != VarId(0) and
         operandInterval(c, rhs, subst, lo, hi):
        # `v + k <= rhs <= hi`
        c.facts.add query(v, VarId(0), hi - k - strict)
      elif pureOperand(c, rhs, subst, v, k) and v != VarId(0) and
           operandInterval(c, lhs, subst, lo, hi):
        # `lo <= lhs <= v + k`
        c.facts.add query(VarId(0), v, k - lo - strict)

proc verifyEnsures(c: var FirContext; resultVar: VarId; info: NifLineInfo) =
  ## Prove the routine's own `.ensures` where control leaves it: `result` stands
  ## for `resultVar`, `old(e)` for what `e` held on entry. A postcondition is
  ## the routine's promise to every caller, who takes it without looking; left
  ## unproven it would be a `{.assume.}` in disguise.
  if not cursorIsNil(c.ownEnsures) and c.tr.live:
    var cond = c.ownEnsures
    while cond.exprKind == ExprX:
      var inner = cond
      inner = sub(inner)
      if not inner.hasMore: break
      if inner.symKind == ResultY and resultVar != InvalidVarId:
        c.substVar[asLocal(inner).name.symId] = resultVar
      while inner.hasMore and not isLastSon(inner): skip inner
      cond = inner
    let callSlots = move c.oldSlots
    c.oldSlots = c.ownOldSlots
    let noSubst = initTable[SymId, Cursor]()
    let res = proveCond(c, cond, noSubst)
    c.oldSlots = callSlots
    c.substVar.clear()
    case res
    of Proven: discard
    of Disproven:
      buildErr c, info, "postcondition violated: " & asNimCode(cond)
    of Unprovable:
      if StaticContractsFeature in c.features:
        buildErr c, info, "cannot prove postcondition: " & asNimCode(cond)

proc assumeOwnContract(c: var FirContext; n: Cursor) =
  let noArgs = initTable[SymId, Cursor]()
  assumeCond(c, n, noArgs)

proc assumeEnsures(c: var FirContext; call: Cursor; resultVar: VarId) =
  ## A callee's `.ensures`, read at the call site with its parameters standing
  ## for the arguments and `result` for the location the call was bound to.
  ##
  ## This is how `len` states `0 <= result` without its *type* saying so —
  ## `Natural` as a return type would change what `var x = len(s)` infers, which
  ## is not a price worth paying for a fact a pragma can state directly.
  ##
  ## Only propositions the fact engine models contribute, and only what the
  ## callee actually promises: an `.ensures` is the callee's word, so it is
  ## taken at face value exactly as a `.requires` is at the other end.
  if resultVar == InvalidVarId: return
  if not call.isTagLit or call.exprKind notin CallKinds: return
  var fn = call
  fn = sub(fn)
  var fnType = skipProcTypeToParams(getType(c.typeCache, fn))
  if not fnType.isParamsTag: return

  # the proposition names the callee's parameters; bind them to the arguments
  var subst = initTable[SymId, Cursor]()
  let paramsStart = fnType
  var p = fnType
  p = sub(p)
  var arg = fn
  skip arg # past the callee
  while p.hasMore and arg.hasMore:
    let param = takeLocal(p, SkipFinalParRi)
    subst[param.name.symId] = arg
    skip arg

  fnType = paramsStart
  skip fnType # params
  skip fnType # return type
  let ens = extractPragma(fnType, EnsuresP)
  if cursorIsNil(ens) or ens.exprKind != ExprX: return

  # `(expr (result :r . . T .) <cond>)`, and the wrapper nests once per
  # sem-check of the declaration — see `finalir.forRangeAssumes`.
  var resultSyms: seq[SymId] = @[]
  var cond = ens
  while cond.exprKind == ExprX:
    var inner = cond
    inner = sub(inner)
    if not inner.hasMore: break
    if inner.symKind == ResultY:
      resultSyms.add asLocal(inner).name.symId
    while inner.hasMore and not isLastSon(inner): skip inner
    cond = inner
  if resultSyms.len == 0: return

  for rs in resultSyms:
    c.substVar[rs] = resultVar
  assumeCond(c, cond, subst)
  c.substVar.clear()

proc snapshotOlds(c: var FirContext; n: Cursor; paramMap: Table[SymId, Cursor]) =
  ## Every `old(e)` the callee's `.ensures` mentions gets a slot holding the
  ## facts `e` has *now*, before the call's mutations drop them: a copy of each
  ## fact about `e`, restated about the slot.
  var arg = default(Cursor)
  if oldArg(n, arg):
    var root = NoSymId
    var key = ""
    var steps = 0
    if locationKey(c, arg, paramMap, root, key, steps, 0) and not c.oldSlots.hasKey(key):
      let loc = locationVarId(c, arg, paramMap)
      let slot = derivedIdOf(c, "#old:" & key, NoSymId)
      # The slot is reused by every call: what the previous one left there may
      # be the only link between `e` and its bounds (`e == slot`, `i < slot`),
      # so it is projected out rather than dropped.
      projectOut(c.facts, slot)
      c.oldSlots[key] = slot
      if loc != InvalidVarId:
        let known = c.facts.len
        for i in 0 ..< known:
          let f = c.facts[i]
          if f.a == loc or f.b == loc:
            c.facts.add LeXplusC(a: (if f.a == loc: slot else: f.a),
                                 b: (if f.b == loc: slot else: f.b), c: f.c)
        # ... and that the slot *is* `e` for now, which a later join can keep
        # when the copies have taken different shapes on its two sides
        c.facts.add query(slot, loc)
        c.facts.add query(loc, slot)
  elif n.kind == TagLit:
    var r = n
    r = sub(r)
    while r.hasMore:
      snapshotOlds(c, r, paramMap)
      skip r

proc ensuresProposition(ens: Cursor): Cursor =
  ## `(expr (result :r . . T .) <cond>)`, nested once per sem-check of the
  ## declaration: the condition inside all the wrappers.
  result = ens
  var peeling = true
  while peeling and result.exprKind == ExprX:
    var inner = result
    inner = sub(inner)
    if inner.hasMore:
      while inner.hasMore and not isLastSon(inner): skip inner
      result = inner
    else:
      peeling = false

proc checkRequires(c: var FirContext; req: Cursor; paramMap: Table[SymId, Cursor];
                   info: NifLineInfo) =
  ## Discharge the callee's `.requires` at this call site.
  ##
  ## By default a contract whose *negation* follows from what is known here is
  ## an error, and one the engine cannot decide is left to the runtime guard
  ## hexer emits into the callee. The asymmetry is deliberate: `seq`/`string`
  ## indexing *is* a `.requires`, and the prover carries no loop induction yet,
  ## so `var i = 0; while i < s.len: use(s[i])` cannot be proven — demanding
  ## proof everywhere would reject most ordinary code, the standard library
  ## included.
  ##
  ## `{.feature: "staticContracts".}` demands the proof, module by module, and
  ## is where the language is headed; `{.feature: "runtimeContracts".}` opts out
  ## of the static judgement entirely and wins if both are given.
  if RuntimeContractsFeature in c.features: return
  # An obligation on a path control cannot reach is vacuous, and the facts there
  # are not merely weak but meaningless — a join on a dead path keeps *both*
  # arms of the preceding `if`, so contradictory facts hold at once and anything
  # at all can be "disproved". (`trFor` emits the exit label of every `for` so
  # that a loop no longer makes its own continuation look dead; what is left
  # here is genuinely unreachable code.)
  if not c.tr.live: return
  let res = proveCond(c, req, paramMap)
  when defined(contractStats):
    stderr.writeLine "CONTRACT " & $res & " " & infoToStr(info) & " " & asNimCode(req)
  case res
  of Proven:
    discard "obligation discharged"
  of Disproven:
    buildErr c, info, "contract violated: " & asNimCode(req)
  of Unprovable:
    if StaticContractsFeature in c.features:
      buildErr c, info, "cannot prove contract: " & asNimCode(req)

# --- Expression analysis ---

proc analyseOconstr(c: var FirContext; n: var Cursor) =
  n.into:
    let objType = n
    skip n # type
    while n.hasMore:
      assert n.substructureKind == KvU
      n.into:
        assert n.isSymbol
        let expected = lookupField(c.typeCache, objType, n.symId)
        assert not cursorIsNil(expected), "could not lookup type for " & pool.symString(n.symId)
        skip n # field name
        checkNilMatch c, n, expected
        skip n # value
        if n.hasMore:
          # optional inheritance
          skip n

proc analyseArrayConstr(c: var FirContext; n: var Cursor) =
  n.into:
    let expected = n.childCursor # element type of the array
    skip n # type
    while n.hasMore:
      checkNilMatch c, n, expected
      skip n

proc analyseTupConstr(c: var FirContext; n: var Cursor) =
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

proc arrayBound(c: var FirContext; n: Cursor; res: var xint): bool =
  ## A bound operand of `(arrat …)`: an integer literal, or anything the const
  ## evaluator folds — a generic array's `N` arrives here as a `const`, and an
  ## ordinal index range (`array['a'..'z', int]`) as a char.
  case n.kind
  of IntLit:
    res = createXint(n.intVal)
    result = true
  of UIntLit:
    res = createXint(n.uintVal)
    result = true
  else:
    res = tryEvalOrdinal(c.bits, n)
    result = not res.isNaN

proc checkIndexInBounds(c: var FirContext; idx, bounds: Cursor) =
  ## `bounds` points at the `hi [lo]` operands of an `(arrat …)`.
  ##
  ## Split out of `analyseArrAt` because the two are different jobs — the walk
  ## reads the node, this judges what it read — and originally for a second
  ## reason that no longer applies: `checkInRange` takes two `xint`s and a
  ## `Cursor` by value, and marshalling three aggregates at a call that also had
  ## the walk's own cursors live was more than arkham's x64 allocator could place
  ## ("out of registers for a clobber-exposed aggregate call argument", nativenif
  ## `takeParked`). Keeping them in separate frames is still the clearer shape.
  var hi = zero()
  if not arrayBound(c, bounds, hi): return
  var lo = zero()
  var r = bounds
  skip r
  if r.hasMore and not arrayBound(c, r, lo): return
  checkInRange c, idx, lo, hi, needLo = true, needHi = true,
               reportUnprovable = StaticContractsFeature in c.features

proc analyseArrAt(c: var FirContext; pc: var Cursor; call: var CallContext) =
  ## `(arrat arr idx [hi [lo]])`. The bounds sem attached ARE the obligation:
  ## they are exactly what `hexer/desugar.trArrAt` turns into the
  ## `nimIcheckAB(i, lo, hi)` call, so discharging one here is discharging that
  ## call. A node carrying none indexes something unchecked (an
  ## `UncheckedArray`, a pointer) and states nothing to prove.
  ##
  ## An index that cannot be proven is not an error by itself — the bound check
  ## stays and catches it at run time — so only `{.feature: "staticContracts".}`
  ## demands the proof. An index that is *disproven* (a literal outside the
  ## range) is an error either way. That is the same three-way answer
  ## `checkRequires` gives a `.requires`, and `runtimeContracts` (which `v2`
  ## implies) opts out of the judgement here for the same reason it does there.
  pc.into:
    traverseExpr c, pc, call            # the array operand
    let idx = pc
    traverseExpr c, pc, call            # the index
    # On a path control cannot reach the facts are not merely weak but
    # contradictory, and an obligation judged against them could report a
    # correct index as a violation. `checkRequires` bails for the same reason.
    if pc.hasMore and c.tr.live and RuntimeContractsFeature notin c.features:
      checkIndexInBounds c, idx, pc
    while pc.hasMore: skip pc

proc traverseExpr(c: var FirContext; pc: var Cursor; call: var CallContext) =
  case pc.kind
  of Symbol:
    let symId = pc.symId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {VarY, LetY, CursorY, PatternvarY, ResultY}:
      if c.tr.live and not isInitialized(c, symId):
        buildErr(c, pc.info, "cannot prove that " & asNimCode(pc) & " has been initialized")
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
      analyseCall c, pc, call
    of DotX:
      pc.into:
        traverseExpr c, pc, call # object
        skip pc # field name
        if pc.hasMore: skip pc # inheritance depth
        if pc.hasMore: skip pc # optional access-token string lit
    of DdotX:
      pc.into:
        wantNotNilDeref c, pc
        traverseExpr c, pc, call # object
        skip pc # field name
        if pc.hasMore: skip pc # inheritance depth
        if pc.hasMore: skip pc # optional access-token string lit
    of DerefX:
      pc.into:
        wantNotNilDeref c, pc
        traverseExpr c, pc, call
    of ArratX:
      analyseArrAt c, pc, call
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
        traverseExpr c, pc, call
    of NilX:
      # `(nil)` / `(nil <Type>)` / `(nil <Type> <arg>)` — nil literal,
      # possibly carrying its formal type subtree (which for itertype /
      # closure-proctype contains raw param SymbolDefs that the generic
      # expression walk would mis-classify). Nothing in here can hold
      # free variables we'd want to track, so skip the whole subtree.
      skip pc
    else:
      pc.loopInto:
        traverseExpr c, pc, call
  else:
    inc pc  # ParRi/close (classic) or stray suffix (nifcore)

proc borrowCheckForCall(c: var FirContext; args: Cursor) =
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

proc analyseCallArgs(c: var FirContext; n: var Cursor; call: var CallContext) =
  let callCursor = n
  let tt = getType(c.typeCache, n)
  var fnType = skipProcTypeToParams(tt)
  if not fnType.isParamsTag:
    # No signature to analyse against: the callee is an `(err ...)` a previous
    # phase left behind (`derefs` replaces a rejected call with one), so its
    # "type" has no params/pragmas slots to walk. The error is already
    # reported; walk the subtree and leave the rest to it. Same guard
    # `analyseCall` above already applies before reading `noreturn`.
    traverseExpr c, n, call # the `fn` itself
    while n.hasMore: traverseExpr c, n, call
    return
  var fnPragmas = fnType
  skip fnPragmas # params
  skip fnPragmas # return type
  let effect = calleeEffect(tt, fnPragmas)
  traverseExpr c, n, call # the `fn` itself
  let paramsStart = fnType
  fnType = sub(fnType)
  var paramMap = initTable[SymId, Cursor]()
  # Collect argument paths for aliasing check
  let args = n
  var needsBorrowCheck = false
  var mutatedRoots: seq[SymId] = @[]
  var viewRoots: seq[SymId] = @[]
  var mutatesUnknown = false
  while n.hasMore:
    if not fnType.hasMore:
      # All formal params consumed but args remain (e.g. varargs that were
      # consumed without a matching VarargsT param, or similar edge cases).
      # Traverse remaining args for their side effects.
      while n.hasMore:
        traverseExpr c, n, call
      break
    let previousFormalParam = fnType
    let param = takeLocal(fnType, SkipFinalParRi)
    # The argument *tree*, not its position: the contract is discharged by
    # substituting it for the parameter below. Saved before `traverseExpr`
    # advances `n`; the input buffer is never rewritten, so the cursor stays
    # valid for the rest of the call.
    paramMap[param.name.symId] = n
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
    # An argument bound to a `var`/`out` parameter may come back changed. The
    # Final IR marks that with an `(unknown …)` after the call, but only for an
    # argument that had to take its address here: a `var` parameter *forwarded*
    # to another one already is a location and picks up no `(haddr …)`, so
    # `proc f(s: var string) = s.setLen(n); s[0] = 'x'` went on believing
    # whatever was known about `s`. The formal parameter is right here, so
    # judge by it rather than by the shape of the argument.
    if pk in {MutT, OutT}:
      let root = derivedRootOf(c, n)
      let argSym = extractSymId(n)
      if argSym != NoSymId:
        let argInfo = getLocalInfo(c.typeCache, argSym)
        if argInfo.kind == ParamY and isVarOpenArray(c, argInfo.typ) and
            not isVarOpenArray(c, param.typ):
          buildErr c, n.info, "`" & pool.symString(argSym).split('.')[0] &
            "` is a `var openArray` parameter: only its elements may change, " &
            "and a `var` of another type could replace it"
      if root != NoSymId and isVarOpenArray(c, param.typ) and
          plainLocationVarId(c, n) == getVarId(c, root):
        # a view passed as a view: its length stays
        viewRoots.add root
      elif root != NoSymId:
        mutatedRoots.add root
      elif not writesThroughPointee(c, n):
        mutatesUnknown = true
    checkNilMatch c, n, param.typ
    traverseExpr c, n, call
  if needsBorrowCheck and not c.features.contains(LenientAliasingFeature):
    borrowCheckForCall c, args
  while fnType.hasMore: skip fnType
  fnType = paramsStart; skip fnType
  skip fnType # skip return type
  # now we have the pragmas:
  let req = extractPragma(fnType, RequiresP)
  if not cursorIsNil(req):
    # A precondition is judged on the state at *entry*, so this must run before
    # the mutation below invalidates it.
    checkRequires c, req, paramMap, callCursor.info
  # So is every `old(e)` of the postcondition.
  let ens = extractPragma(fnType, EnsuresP)
  c.oldSlots.clear()
  if not cursorIsNil(ens):
    snapshotOlds c, ens, paramMap
  # `inc i` *shifts* what is known about `i`; it does not erase it. Without
  # this every `inc` in a loop threw away the bounds the guard had just
  # established, which is most of what makes hand-written index code
  # unprovable.
  var stepLoc = InvalidVarId
  var delta = createNaN()
  var stepArg = default(Cursor)
  var isStep = stepCallAt(c, callCursor, stepLoc, delta, stepArg) and not delta.isNaN
  if isStep:
    variableChangedByDiff(c.facts, stepLoc, delta)
    call.stepped = stepLoc
  elif stepLoc != InvalidVarId and not cursorIsNil(stepArg):
    # `inc(i, L)` with an `L` the facts bound: `i` moves within that range
    var lo = createNaN()
    var hi = createNaN()
    operandRange(c, stepArg, lo, hi)
    if not lo.isNaN or not hi.isNaN:
      var info = default(StepInfo)
      discard stepOf(c, extractSymId(callCursor), info)
      if info.isSub:
        swap lo, hi
        lo = -lo
        hi = -hi
      variableMovedWithin(c.facts, stepLoc, lo, hi)
      call.stepped = stepLoc
      isStep = true
  for root in mutatedRoots:
    if isStep and getVarId(c, root) == stepLoc:
      # the step above described this write exactly
      invalidateDerivedFrom(c, root)
    else:
      invalidateFactsAbout(c.facts, getVarId(c, root))
      invalidateDerivedFrom(c, root)
  if mutatesUnknown:
    invalidateAllDerived(c)
  for root in viewRoots:
    invalidateViewElements(c, root)
  for root in viewRoots: call.views.add root
  # A routine without a result states its postcondition about its `var`
  # parameters alone, and nothing binds the call to a location for
  # `assumeEnsures` to read it at: it is taken here, after the mutations it
  # describes. `[]=` promising `s.len == old(s.len)` is what keeps a write loop
  # provable.
  if not cursorIsNil(ens) and c.oldSlots.len > 0:
    var retType = paramsStart
    skip retType
    if retType.isDotToken or retType.typeKind == VoidT:
      assumeCond c, ensuresProposition(ens), paramMap
      for root in mutatedRoots: call.ensured.add root

proc analyseCall(c: var FirContext; n: var Cursor; call: var CallContext) =
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
    analyseCallArgs(c, n, call)
  if isNoReturn:
    c.tr.live = false

# --- Assignment fact tracking ---

proc addAsgnFact(c: var FirContext; fact: LeXplusC) =
  if fact.isValid:
    c.facts.add fact
    c.facts.add fact.geXplusC
    if c.declaredRange.hasKey(fact.b):
      # `var n = c.bufLen` of a `range[0..63]` field: `n` starts out in that
      # range, as a flow fact of its own that a join can keep
      let r = c.declaredRange.getOrQuit(fact.b)
      c.facts.add query(VarId(0), fact.a, -(r.lo + fact.c))
      c.facts.add query(fact.a, VarId(0), r.hi + fact.c)

proc addBoundFacts(c: var FirContext; dest: VarId; value: Cursor) =
  ## A value `rightHandSide` cannot state as `b + c` may still have a known
  ## interval (`valueBounds`); the location it is stored in then lies in it.
  var lo = zero()
  var hi = zero()
  if valueBounds(c, value, lo, hi):
    c.facts.add query(VarId(0), dest, -lo)
    c.facts.add query(dest, VarId(0), hi)
  # a mask can bound it more tightly than the interval of a conversion around
  # it does: `int(h and mask)`
  if (let v = peelExpr(value); v.exprKind == BitandX):
    # `x and m` lies in `0..m` for a mask `m` known not to be negative: the
    # `hash and high(data)` every hash table starts its probe with.
    let noSubst = initTable[SymId, Cursor]()
    var r = v
    r = sub(r)
    skip r # the type operand
    for _ in 0 ..< 2:
      var mv = VarId(0)
      var mk = zero()
      if pureOperand(c, r, noSubst, mv, mk) and mv != VarId(0) and
          impliesHere(c, query(VarId(0), mv, mk)):
        # every such operand bounds it: `a.len and high(b)` is below both
        c.facts.add query(VarId(0), dest, zero())
        c.facts.add query(dest, mv, mk)
      skip r

proc cannotBeNil(c: var FirContext; n: Cursor): bool {.inline.} =
  let t = getType(c.typeCache, n)
  result = markedAs(t, NotnilU) or isNonNilExpr(c, n)

proc storeTargetType(c: var FirContext, n: Cursor): Cursor {.inline.} =
  var target = n
  if target.exprKind in {AddrX, HaddrX}:
    inc target
  result = getType(c.typeCache, target)

const HookPrefixes = ["=destroy", "=wasMoved", "=trace", "=copy", "=sink", "=dup"]

proc isHookProc(symId: SymId): bool =
  ## A type-bound hook, hand-written (`=wasMoved.0.m`) or synthesized by the
  ## lifter (`=dup_SFoo0m.0.m`). Their bodies run on memory that is not in a
  ## valid state: `=wasMoved` *produces* the moved-from state, `=destroy` and
  ## `=trace` *observe* it, and the lifter drops the nilability annotation from
  ## a hook's own signature (see `lifter.addParamType`) so that one hook serves
  ## both `ref T` and `ref T notnil`. Nil-checking a store inside such a body
  ## asks the impossible of it.
  let name = pool.symBasename(symId)
  if name.len == 0 or name[0] != '=': return false
  for p in HookPrefixes:
    if name.startsWith(p): return true
  result = false

proc checkStoreNilMatch(c: var FirContext; value: Cursor; expected: Cursor) {.inline.} =
  if not c.inHook:
    checkNilMatch c, value, expected

# --- Final-IR-specific traversal ---

proc sumWithBoundedTerm(c: var FirContext; value: Cursor; dest: VarId): bool =
  ## `x = y + d` or `x = y - d` for a location `y` and a `d` the facts bound
  ## (either end may be open). With `y` being `x` itself that moves what is
  ## known about `x` rather than erasing it (`variableMovedWithin`); otherwise
  ## `x` lies within `d` of `y`. `x = d + y` is the same sum.
  result = false
  let v = peelExpr(value)
  if v.exprKind notin {AddX, SubX}: return
  var r = v
  r = sub(r)
  skip r # the type operand
  var first = r
  skip r
  var second = r
  var y = plainLocationVarId(c, first)
  if y == InvalidVarId and v.exprKind == AddX:
    y = plainLocationVarId(c, second)
    second = first
  if y == InvalidVarId or y == VarId(0): return
  var lo = createNaN()
  var hi = createNaN()
  operandRange(c, second, lo, hi)
  if lo.isNaN and hi.isNaN: return
  if v.exprKind == SubX:
    swap lo, hi
    lo = -lo
    hi = -hi
  if y == dest:
    variableMovedWithin(c.facts, dest, lo, hi)
  else:
    projectOut(c.facts, dest)
    if not lo.isNaN: c.facts.add query(y, dest, -lo)   # y + lo <= x
    if not hi.isNaN: c.facts.add query(dest, y, hi)    # x <= y + hi
  result = true

proc traverseStore(c: var FirContext; n: var Cursor; call: var CallContext) =
  ## Handle (store value dest) - note reversed order from asgn
  let storeStart = n # skip store tag
  n = sub(n)

  # First analyze the value (source)
  let valueStart = n
  traverseExpr c, n, call

  # Check borrow conflicts for the destination
  let destMutPath = extractPath(c, n)
  if destMutPath.mode in {IsBorrowable, IsBorrowableFromGlobal}:
    checkBorrowConflict(c, destMutPath, n.info)
  if destMutPath.path.len > 0:
    checkEscapingBorrow(c, valueStart, destMutPath.path[0])

  # Now handle the destination (a Symbol; the NJVL `(v symId version)` form
  # is no longer produced, see the `VV` branch of `traverseStmt`)
  let destSymId = extractSymIdForStore(n)
  # `outer = toOpenArray(a)` rebinds what `outer` aliases, exactly as the
  # `let outer = toOpenArray(a)` form does in `traverseLocal`. Destinations that
  # outlive the proc are skipped: they are never `kill`ed, so the scope-exit
  # check below could not judge them anyway, and `checkEscapingBorrow` above
  # already had the final say on those.
  if destSymId != NoSymId and not outlivesProc(c, destSymId) and
     canCarryBorrow(c, valueStart):
    endBorrow(c, destSymId)
    var b = borrowCarriedBy(c, valueStart)
    if b.mode in {IsBorrowable, IsBorrowableFromGlobal}:
      b.borrower = destSymId
      c.activeBorrows.add b
  if destSymId != NoSymId:
    let symId = destSymId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {LetY, GletY, TletY}:
      if isInitialized(c, symId):
        c.buildErr n.info, "invalid reassignment to `let` variable"
    elif x.kind == ParamY and isVarOpenArray(c, x.typ):
      c.buildErr n.info, "a `var openArray` parameter cannot be replaced; only its elements can change"

    var fact = query(getVarId(c, symId), InvalidVarId, createXint(0'i32))
    markInit(c, symId)
    # Overwriting the variable invalidates every location derived from it:
    # after `s = other`, nothing is known about `s.len` any more.
    invalidateDerivedFrom(c, symId)

    # Check for not-nil type match
    let expected = storeTargetType(c, n)
    checkStoreNilMatch c, valueStart, expected
    checkRangeAssign c, expected, valueStart

    # Try to extract facts from the value
    var valueForFact = valueStart
    if rightHandSide(c, valueForFact, fact, call):
      if fact.a == fact.b:
        variableChangedByDiff(c.facts, fact.a, fact.c)
      else:
        projectOut(c.facts, fact.a)
        addAsgnFact c, fact
    elif not sumWithBoundedTerm(c, valueStart, fact.a):
      projectOut(c.facts, fact.a)
      addBoundFacts c, fact.a, valueStart

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
    # ... and whatever the callee promised about what it just returned.
    assumeEnsures c, valueStart, getVarId(c, symId)

    skip n
  else:
    let expected = storeTargetType(c, n)
    checkStoreNilMatch c, valueStart, expected
    checkRangeAssign c, expected, valueStart
    # A write to `d.count` (or through a pointer) must drop what is known about
    # the derived locations it can reach. Attributing it to a root is the common
    # case; when we cannot, every derived location is suspect.
    let destLoc = plainLocationVarId(c, n)
    let destRoot = derivedRootOf(c, n)
    if writesThroughPointee(c, n):
      discard "an element behind a pointer, where no location lies"
    elif destRoot != NoSymId:
      invalidateFactsAbout(c.facts, getVarId(c, destRoot))
      invalidateWrittenPath(c, n, destRoot)
    elif not isElementWrite(n):
      invalidateAllDerived(c)
    if destLoc != InvalidVarId:
      # ... and then record what the write established, so that
      # `d.count = n` proves `d.count == n` for the guard that follows.
      var fact = query(destLoc, InvalidVarId, createXint(0'i32))
      var valueForFact = valueStart
      if rightHandSide(c, valueForFact, fact, call) and fact.a != fact.b:
        addAsgnFact c, fact
    traverseExpr c, n, call

  n = storeStart; skip n

# --- Exit-summary plumbing (drives the journaled FlowTracker over c.flow) ---
#
# Every leave/bind/branch operation now goes through the `FlowTracker`, which
# journals the whole `FlowState` (init-set + facts) in place: a leave snapshots
# the state at its exit key, a bind joins the accumulated exit state back into
# fall-through, and an `ite`/`case`/`try` checkpoints once and rolls back rather
# than copying the state per branch.

proc leaveToLabel(c: var FirContext; label: SymId) = gotoLabel(c.tr, c.flow, label)
proc leaveToReturn(c: var FirContext) = gotoReturn(c.tr, c.flow)
proc leaveToRaise(c: var FirContext) = gotoRaise(c.tr, c.flow)
proc leaveToContinue(c: var FirContext) = gotoContinue(c.tr, c.flow)

proc traverseIte(c: var FirContext; n: var Cursor) =
  ## `(ite cond then else)`. Each arm is analyzed under the condition's polarity;
  ## the tracker merges the fall-through state (inits + facts) by liveness — a
  ## branch that always leaves drops out, unifying guard-clause and if-else
  ## style. The state is journaled, so a branch costs O(writes), not a copy.
  var call = freshCall()
  let iteStart = n # skip ite/itec tag
  n = sub(n)

  let savedBorrowsLen = c.activeBorrows.len
  # Split BEFORE the condition facts so they belong to the then-branch's delta
  # (the then-branch runs under `assume(cond)`); `commitThen` rolls them back.
  # `assert false` and `when`-style constants reach here as a literal condition.
  # Without judging it, the arm that cannot run is analysed as reachable, and
  # `assert false` at the end of a proc stops looking like the dead end it is.
  var constCond = -1   # -1 unknown, 0 always false, 1 always true
  block:
    var probe = n
    var negations = 0
    while probe.exprKind == NotX:
      inc negations
      probe = sub(probe)
    if probe.exprKind == TrueX: constCond = 1
    elif probe.exprKind == FalseX: constCond = 0
    if constCond >= 0 and (negations and 1) == 1:
      constCond = 1 - constCond

  var b = splitBranch(c.tr, c.flow)
  # `analyseCondition` states what the *true* path knows and hands back what the
  # false path knows; only it can tell the two apart (`a != b` is the case where
  # all the knowledge sits on the false path).
  var condFactsList: seq[LeXplusC] = @[]
  discard analyseCondition(c, n, condFactsList, call)

  # then-branch (under assume(c)):
  let liveBeforeThen = c.tr.live
  if constCond == 0: c.tr.live = false
  traverseStmt c, n, call
  # `commitThen` captures the then-branch (its init delta, facts, and exits) and
  # rolls `c.flow` back to the split baseline — which drops the condition facts,
  # since the split was taken before them.
  commitThen(c.tr, c.flow, b)
  c.activeBorrows.setLen(savedBorrowsLen)

  # else-branch (under assume(¬c)):
  if constCond == 1: c.tr.live = false
  elif constCond == 0: c.tr.live = liveBeforeThen
  for f in condFactsList:
    c.facts.add f
  if n.isDotToken:
    inc n
  else:
    traverseStmt c, n, call
  # `mergeBranches` joins the then-branch (in `b`) with the current else-branch,
  # merging both the init-set and the facts; a leaving arm drops out.
  mergeBranches(c.tr, c.flow, b)
  c.activeBorrows.setLen(savedBorrowsLen)

  n = iteStart; skip n

# --- Loop induction ---
#
# A loop body is walked once but runs many times, so a fact established ahead of
# the loop holds only on the first iteration. Dropping all of them is sound and
# was what this used to do — but it also threw away the one thing the everyday
# counting loop depends on:
#
#   var i = 0
#   while i < s.len:      # `i < s.len` comes from the guard
#     use(s[i])           # `0 <= i` has to come from somewhere
#     inc i
#
# `0 <= i` *is* a loop invariant, because every write to `i` in the body moves
# it up. So instead of forgetting everything the body writes, classify each
# written location as increasing / decreasing / unknown and keep exactly the
# facts that no such movement can break: in `a <= b + c`, `a` may only go down
# and `b` may only go up. That is the whole rule, and it is what makes strict
# contract checking usable on hand-written loops.

type
  LoopWrites = object
    kinds: Table[VarId, IvKind]
    nonNeg: HashSet[VarId]
      ## Locals the body declares with a non-negative `range` type. `o = o + k`
      ## is monotone whenever `k` is one of them, and the temp holding a
      ## `Positive`-returning call is exactly that — declared inside the body,
      ## so no fact about it exists yet when this scan runs.
    opaque: bool          ## a write we could not attribute to any location
    preserved: seq[VarId] ## derived locations the call just scanned promises
                          ## to leave alone (`s.len == old(s.len)`); holds
                          ## for the `(unknown …)` that follows it too
    pendingStep: VarId    ## the `(unknown v)` a step call emits right after
                          ## itself is that call's own doing, not a second,
                          ## unclassified write

proc joinIv(a, b: IvKind): IvKind =
  ## What two writes in the same body jointly guarantee. Raising a non-negative
  ## value keeps it non-negative, so `inc` and `shr` in one loop still leave
  ## `0 <= v` standing; nothing else survives a disagreement.
  if a == b: a
  elif a in {ivIncreasing, ivNonNegative} and b in {ivIncreasing, ivNonNegative}:
    ivNonNegative
  else: ivUnknown

proc noteWrite(w: var LoopWrites; v: VarId; kind: IvKind) =
  if v == InvalidVarId or v == VarId(0): return
  if w.kinds.hasKey(v):
    w.kinds[v] = joinIv(w.kinds.getOrQuit(v), kind)
  else:
    w.kinds[v] = kind

proc noteDerivedOf(c: var FirContext; w: var LoopWrites; root: SymId; keep: VarId) =
  ## Writing a location disturbs everything derived from the same root: after
  ## `s = other`, `s.len` is anyone's guess — unless the write is a call whose
  ## `.ensures` says otherwise (`w.preserved`).
  for i in 0 ..< c.derivedRoots.len:
    if c.derivedRoots[i] == root:
      let v = VarId(FirstDerivedVarId - i)
      if v != keep and v notin w.preserved: noteWrite(w, v, ivUnknown)

proc preservedByEnsures(c: var FirContext; call: Cursor): seq[VarId] =
  ## The locations a call's `.ensures` states unchanged: every conjunct shaped
  ## `e == old(e)`, with the parameters standing for the arguments.
  result = @[]
  var fn = call
  fn = sub(fn)
  # A declared routine, read from its declaration: this runs as the loop is
  # pre-scanned, before the locals of the body are known to the type cache, and
  # only a declared routine carries an `.ensures` anyway.
  var fnType = default(Cursor)
  let fnSym = extractSymId(fn)
  if fnSym != NoSymId:
    let routine = tryLoadSym(fnSym)
    if routine.status == LacksNothing and isRoutine(routine.decl.symKind):
      fnType = asRoutine(routine.decl).params
  if not cursorIsNil(fnType) and fnType.isParamsTag:
    var subst = initTable[SymId, Cursor]()
    var p = fnType
    p = sub(p)
    var arg = fn
    skip arg # past the callee
    while p.hasMore and arg.hasMore:
      let param = takeLocal(p, SkipFinalParRi)
      subst[param.name.symId] = arg
      skip arg
    skip fnType # params
    skip fnType # return type
    let ens = extractPragma(fnType, EnsuresP)
    if not cursorIsNil(ens):
      var todo = @[ensuresProposition(ens)]
      while todo.len > 0:
        let cond = todo.pop()
        case cond.exprKind
        of AndX:
          var r = cond
          r = sub(r)
          todo.add r
          skip r
          todo.add r
        of EqX:
          var r = cond
          r = sub(r)
          skip r # the type operand
          let lhs = r
          skip r
          var oldOf = default(Cursor)
          var plain = lhs
          if oldArg(argOf(lhs, subst), oldOf): plain = r
          elif not oldArg(argOf(r, subst), oldOf): plain = default(Cursor)
          if not cursorIsNil(plain):
            let a = locationVarId(c, plain, subst)
            if a != InvalidVarId and a == locationVarId(c, oldOf, subst):
              result.add a
        else:
          discard

proc stepOfValue(c: var FirContext; w: LoopWrites; destLoc: VarId;
                 value: Cursor): IvKind =
  ## `dest = dest ± k` written out, the form a loop that does not go through
  ## `inc` takes.
  result = ivUnknown
  if cursorIsNil(value): return
  let v = peelExpr(value)
  if v.exprKind in {ShrX, AshrX, BitandX}:
    # `v = v shr k` keeps `v >= 0` (given it held), and `v = v and k` makes it
    # so outright for a non-negative mask. Neither is monotone.
    var b = v
    b = sub(b)
    skip b # the type operand
    if plainLocationVarId(c, b) != destLoc: return
    skip b
    var k = createNaN()
    case b.kind
    of IntLit: k = createXint(b.intVal)
    of UIntLit: k = createXint(b.uintVal)
    else: k = tryEvalOrdinal(c.bits, b)
    if not k.isNaN and k >= zero(): result = ivNonNegative
    return
  if v.exprKind notin {AddX, SubX}: return
  var r = v
  r = sub(r)
  skip r # the type operand
  var operand = r
  skip r
  var konst = r
  if plainLocationVarId(c, operand) != destLoc:
    if v.exprKind == SubX: return
    # `dest = k + dest` is the same step written the other way round
    swap operand, konst
    if plainLocationVarId(c, operand) != destLoc: return
  var k = createNaN()
  case konst.kind
  of IntLit: k = createXint(konst.intVal)
  of UIntLit: k = createXint(konst.uintVal)
  else: k = tryEvalOrdinal(c.bits, konst)
  if k.isNaN:
    # Not a constant, but a value the body declared as non-negative moves the
    # destination just as monotonically.
    let kv = plainLocationVarId(c, konst)
    if kv != InvalidVarId and kv in w.nonNeg:
      return if v.exprKind == SubX: ivDecreasing else: ivIncreasing
    return ivUnknown
  result = directionOf(v.exprKind == SubX, k)

proc noteWriteTo(c: var FirContext; w: var LoopWrites; dest: Cursor; value: Cursor) {.nimcall.} =
  let root = derivedRootOf(c, dest)
  if root == NoSymId:
    if not isElementWrite(dest): w.opaque = true
    return
  let loc = plainLocationVarId(c, dest)
  var precise = false
  # The pre-scan runs before the body's locals are registered: the type of a
  # path rooted at one of them cannot be asked for yet, so such a write stays
  # coarse.
  if loc != InvalidVarId and dest.exprKind == DotX and
     getLocalInfo(c.typeCache, root).kind != NoSym:
    var obj = dest
    obj = sub(obj)
    precise = not hasOverlappingFields(c, getType(c.typeCache, obj))
  if precise:
    # the `invalidateWrittenPath` rule: what lies under the field, not its siblings
    let destKey = c.derivedKeys[FirstDerivedVarId - int(loc)]
    for i in 0 ..< c.derivedRoots.len:
      let v = VarId(FirstDerivedVarId - i)
      if c.derivedRoots[i] == root and v != loc and v notin w.preserved and
         keyUnder(c.derivedKeys[i], destKey):
        noteWrite(w, v, ivUnknown)
  else:
    noteDerivedOf(c, w, root, loc)
  if loc == InvalidVarId:
    noteWrite(w, getVarId(c, root), ivUnknown)
    return
  noteWrite(w, loc, stepOfValue(c, w, loc, value))
  if loc != getVarId(c, root):
    # Writing `d.count` says nothing about `d` itself, but nil-ness and the
    # like are keyed on the root, so stay conservative there.
    noteWrite(w, getVarId(c, root), ivUnknown)

proc scanLoopWrites(c: var FirContext; n: var Cursor; w: var LoopWrites) =
  if not n.isTagLit:
    skip n
    return
  let fk = n.finalIrKind
  if fk == StoreV:
    var r = n
    r = sub(r)
    let value = r
    skip r
    w.preserved.setLen 0
    noteWriteTo(c, w, r, value)
    w.pendingStep = InvalidVarId
    skip n
    return
  if fk == UnknownV:
    var r = n
    r = sub(r)
    if w.pendingStep != InvalidVarId and plainLocationVarId(c, r) == w.pendingStep:
      discard "the step call right before it already said which way this moves"
    else:
      noteWriteTo(c, w, r, default(Cursor))
    w.pendingStep = InvalidVarId
    w.preserved.setLen 0
    skip n
    return
  if n.exprKind in CallKinds:
    w.preserved.setLen 0
    var stepLoc = InvalidVarId
    var delta = createNaN()
    if stepCall(c, n, stepLoc, delta):
      noteWrite(w, stepLoc, directionOfDelta(delta))
      let root = derivedRootOf(c, n)
      if root != NoSymId: noteDerivedOf(c, w, root, stepLoc)
      w.pendingStep = stepLoc
      skip n
      return
    w.preserved = preservedByEnsures(c, n)
    var fnOf = n
    fnOf = sub(fnOf)
    var params = default(Cursor)
    let fnSym = extractSymId(fnOf)
    if fnSym != NoSymId:
      let routine = tryLoadSym(fnSym)
      if routine.status == LacksNothing and isRoutine(routine.decl.symKind):
        params = asRoutine(routine.decl).params
    # A `var`/`out` argument that is already a location takes no `(haddr …)`
    # and so gets no `(unknown …)` — see `analyseCallArgs`. Judged by the
    # formal parameter where the callee is known: a `var openArray` passed on
    # to a plain `openArray` is a bare symbol too, and writes nothing.
    var r = n
    r = sub(r)
    skip r # the callee
    var formal = default(Cursor)
    if not cursorIsNil(params) and params.isParamsTag:
      formal = params
      formal = sub(formal)
    while r.hasMore:
      var writes = true
      var viewFormal = false
      if not cursorIsNil(formal):
        if formal.hasMore:
          let param = takeLocal(formal, SkipFinalParRi)
          writes = param.typ.typeKind in {MutT, OutT}
          viewFormal = isVarOpenArray(c, param.typ)
      let sym = if r.isSymbol: r.symId else: NoSymId
      if viewFormal:
        # a view passed as a view keeps its length, through the `(unknown …)`
        # that follows the call too
        let viewRoot = extractSymId(r)
        if viewRoot != NoSymId:
          for i in 0 ..< c.derivedRoots.len:
            if isViewLen(c, i, viewRoot): w.preserved.add VarId(FirstDerivedVarId - i)
      if writes and sym != NoSymId:
        let info = getLocalInfo(c.typeCache, sym)
        if not cursorIsNil(info.typ) and info.typ.typeKind in {MutT, OutT}:
          noteWrite(w, getVarId(c, sym), ivUnknown)
          noteDerivedOf(c, w, sym, InvalidVarId)
      skip r
  elif isLocal(n.symKind):
    # A local declared inside the body is rebound every iteration.
    w.preserved.setLen 0
    let local = asLocal(n)
    if local.name.kind == SymbolDef:
      var lo = zero()
      var hi = zero()
      if staticRangeBounds(local.typ, lo, hi) and lo >= zero():
        w.nonNeg.incl getVarId(c, local.name.symId)
      noteWrite(w, getVarId(c, local.name.symId), ivUnknown)
      noteDerivedOf(c, w, local.name.symId, InvalidVarId)
  w.pendingStep = InvalidVarId
  n.into:
    while n.hasMore:
      scanLoopWrites(c, n, w)

proc isLoopInvariant(c: FirContext; w: LoopWrites; f: LeXplusC): bool =
  ## `a <= b + c` survives the loop when nothing the body does can break it:
  ## `a` may only move down and `b` may only move up.
  if w.opaque and (int(f.a) <= FirstDerivedVarId or int(f.b) <= FirstDerivedVarId):
    # An unattributable write may have hit any derived location.
    return false
  let aMoves = w.kinds.hasKey(f.a)
  let bMoves = w.kinds.hasKey(f.b)
  if not aMoves and not bMoves: return true
  if aMoves and w.kinds.getOrDefault(f.a) != ivDecreasing: return false
  if bMoves:
    case w.kinds.getOrDefault(f.b)
    of ivIncreasing: discard "a lower bound on a rising value still holds"
    of ivNonNegative:
      # Only `0 <= b` itself survives, not a bound above zero.
      if not (f.a == VarId(0) and f.c >= zero()): return false
    else: return false
  result = true

proc restrictFactsToLoopInvariants(c: var FirContext; w: LoopWrites) =
  # Strengthen before widening. `0 <= b` may only be *derivable* on entry — from
  # `0 <= y` and `y <= b` after `var b = y` — and the chain it hangs on is not
  # itself an invariant (`b = b shr 1` breaks `y <= b`). So ask the question
  # while the answer is still there, and record it as a fact of its own.
  var keepNonNeg: seq[VarId] = @[]
  for v, kind in w.kinds:
    if kind == ivNonNegative and implies(c.facts, query(VarId(0), v, zero())):
      keepNonNeg.add v
  # A bound between two locations the body leaves alone may exist only as a
  # chain through one it writes — `let s = i` under `i < n`, then a loop that
  # moves `i`, still has `s < n`. Keep the chain before the link goes.
  var bypasses: seq[LeXplusC] = @[]
  for v, kind in w.kinds:
    for ch in chainsThrough(c.facts, v):
      if isLoopInvariant(c, w, ch): bypasses.add ch
  var i = 0
  while i < c.facts.len:
    if isLoopInvariant(c, w, c.facts[i]):
      inc i
    else:
      removeFactAt(c.facts, i)   # journaled; the swapped-in slot is rechecked
  for v in keepNonNeg:
    c.facts.add query(VarId(0), v, zero())
  for ch in bypasses:
    c.facts.add ch

proc traverseLoop(c: var FirContext; n: var Cursor) =
  ## `(loop body)` — infinite; the body ends in `(continue .)` and exits
  ## forward via `(jmp loopExit)`. The while-condition is the leading guard
  ## `(ite (not cond) (jmp loopExit) .)` *inside* the body, so it needs no
  ## special handling here. Iteration-gained facts/inits flow only to the
  ## break sites (captured) and to the back-edge (discarded); the loop never
  ## falls through. The `(lab loopExit)` that follows installs the merged
  ## break state via `bindLoopExit`.
  var call = freshCall()
  n.into: # loop tag
    # Before the checkpoint, not after: the rollback below restores the state
    # the checkpoint captured, so an invalidation made after it would be undone
    # and the stale fact would be back in force *after* the loop — which is
    # where `var colon = -1; while …: colon = i; …; if colon >= 0:` went wrong.
    block:
      var w = LoopWrites(kinds: initTable[VarId, IvKind](),
                         nonNeg: initHashSet[VarId](), opaque: false,
                         pendingStep: InvalidVarId)
      var scan = n
      c.prescanning = true
      scanLoopWrites(c, scan, w)
      c.prescanning = false
      restrictFactsToLoopInvariants(c, w)
    let cp = c.flow.checkpoint()
    let savedBorrows = c.activeBorrows.len
    traverseStmt c, n, call        # the body `(stmts ...)`; ends by leaving
    dropContinue(c.tr)       # the loop header consumes the back-edge
    # The loop never falls through; reset the working state to the pre-loop base.
    # A following `(lab loopExit)` installs the join of the states its exits
    # jumped with (`bindLoopExit`).
    c.flow.rollbackTo cp
    # Borrows taken *inside* the body are loop-local (a `var p = addr coll[i]`
    # cannot outlive the iteration), so drop them — otherwise a later mutation of
    # the borrowed container after the loop is wrongly seen as still-borrowed.
    c.activeBorrows.setLen(savedBorrows)
  # The trailing `(lab loopExit)` (emitted iff a `break`/guard targeted it) is
  # *this* loop's exit. Record it so `traverseLabel` uses `bindLoopExit`.
  if n.isTagLit and n.finalIrKind == LabV:
    var peek = n
    inc peek
    c.loopExitLabels.incl peek.symId

proc traverseLabel(c: var FirContext; n: var Cursor) =
  ## `(lab L)` — the multi-join. Every forward `jmp L` has already been seen.
  var label = NoSymId
  n.into:
    label = n.symId
    inc n # symdef
  if c.loopExitLabels.contains(label):
    bindLoopExit(c.tr, c.flow, label)
  else:
    bindLabel(c.tr, c.flow, label)

proc traverseJmp(c: var FirContext; n: var Cursor) =
  ## `(jmp L)` — a forward structural transfer (loop-`break` included).
  var label = NoSymId
  n.into:
    label = n.symId
    inc n # symuse
  leaveToLabel(c, label)

proc traverseRet(c: var FirContext; n: var Cursor; call: var CallContext) =
  ## `(ret .X)` — primitive return, bound by the proc root. A `return value`
  ## with a non-`result` operand *provides* the result directly (the NJVL path
  ## rewrote this to `result = value`), so it initializes `result` on this exit.
  let info = n.info
  n.into:
    if n.isDotToken:
      verifyEnsures c, (if c.resultSym != NoSymId: getVarId(c, c.resultSym) else: InvalidVarId), info
      inc n
    else:
      var retLoc = plainLocationVarId(c, n)
      if retLoc == InvalidVarId and not cursorIsNil(c.ownEnsures):
        # `return i - 1`: the value gets a slot of its own to be judged in
        retLoc = derivedIdOf(c, "#ret", NoSymId)
        invalidateFactsAbout(c.facts, retLoc)
        var v = VarId(0)
        var k = zero()
        let noSubst = initTable[SymId, Cursor]()
        if pureOperand(c, n, noSubst, v, k):
          addAsgnFact c, query(retLoc, v, k)
        elif not sumWithBoundedTerm(c, n, retLoc):
          addBoundFacts c, retLoc, n
      verifyEnsures c, retLoc, info
      let providesResult = c.resultSym != NoSymId and
        not (n.isSymbol and n.symId == c.resultSym)
      if providesResult:
        # `return toOpenArray(a)` returns the value directly rather than storing
        # it into `result` first, so `traverseStore`'s escape check never sees it.
        checkBorrowOutlivesProc(c, n)
      traverseExpr c, n, call
      if providesResult:
        markInit(c, c.resultSym)
  leaveToReturn(c)

proc traverseRaise(c: var FirContext; n: var Cursor; call: var CallContext) =
  ## `(raise .X)` — primitive raise, bound by the nearest enclosing `except`.
  n.into:
    if n.isDotToken:
      inc n # bare re-raise
    else:
      traverseExpr c, n, call
  leaveToRaise(c)

proc addCaseFacts(c: var FirContext; selSym: SymId; ranges: Cursor) =
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
      var lo = query(a, VarId(0), createXint(r.intVal))
      c.facts.add lo.geXplusC # sel >= lo
    skip r
    if r.isIntLit:
      c.facts.add query(a, VarId(0), createXint(r.intVal)) # sel <= hi
  elif r.isIntLit:
    var f = query(a, VarId(0), createXint(r.intVal))
    c.facts.add f            # sel <= v
    c.facts.add f.geXplusC   # sel >= v

proc traverseCase(c: var FirContext; n: var Cursor) =
  ## `(case selector (of (ranges...) body)+ (else body)?)`. An N-way merge:
  ## every branch starts from the pre-case state, plus the bound facts implied
  ## by its `ranges`; the post-case fall-through is the intersection of the
  ## init-sets (and a fact-join) over the arms that fall through.
  var call = freshCall()
  let caseStart = n # skip 'case'
  n = sub(n)
  let selCursor = n
  let selSym = extractSymId(selCursor)
  traverseExpr c, n, call # selector (init-checked)

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
    traverseStmt c, bc, call
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

proc traverseTry(c: var FirContext; n: var Cursor) =
  ## `(try body (except ...)* (fin ...)?)`. Conservative: an `except` handler
  ## may run after *any* point of the body, so it can only assume the pre-try
  ## state; a `fin` is analyzed on the merged fall-through (its inits are not
  ## propagated onto exit paths — sound, since that only withholds knowledge).
  var call = freshCall()
  let tryStart = n # skip 'try'
  n = sub(n)
  let cp = c.flow.checkpoint()
  let savedBorrows = c.activeBorrows.len
  let baseLive = c.tr.live

  traverseStmt c, n, call # try body

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
        traverseStmt c, n, call
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
      traverseStmt c, n, call # finally body, on the merged fall-through
  n = tryStart; skip n # close 'try'

proc traverseLocal(c: var FirContext; n: var Cursor; call: var CallContext) =
  let kind = n.symKind
  let localStart = n
  let errMark = c.errors.len
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
  let initStart = n
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
  elif not isInline and establishesBorrow(c, n):
    # `let v = toOpenArray(s)`: `v` keeps aliasing `s` until `v` is killed, so
    # `s` must not be mutated in between. Unlike the `(haddr X)` case a path we
    # cannot follow is not an error here — the argument may legitimately be a
    # raw pointer or a temporary that the callee only reads.
    #
    # Inline temps are deliberately excluded. `if x notin s: s.add x` hoists the
    # view built for `contains` into one, and every hoisted temp is `kill`ed
    # together at the end of the proc — registering a borrow there would keep it
    # alive far past the statement that needed it. Their aliasing is covered
    # precisely, and only for the duration of the call, by the inline-var
    # look-through in `extractBorrowPath`.
    var path = extractPath(c, n)
    if path.mode in {IsBorrowable, IsBorrowableFromGlobal}:
      path.borrower = name
      c.activeBorrows.add path
  if not n.isDotToken and localType.typeKind in {PtrT, RefT, CstringT, PointerT, ProctypeT}:
    checkNilMatch c, n, localType
  if not n.isDotToken:
    checkRangeAssign c, localType, n
  traverseExpr c, n, call
  # `let last = x - 1` is a *fact*, and the Final IR states the bound of every
  # `for i in 0 ..< s.len` in exactly that shape. Only shapes the pure operand
  # reader understands contribute one; unlike `traverseStore` this deliberately
  # does not go through `rightHandSide`, whose fallback would analyse the
  # initializer a second time and report its contracts twice.
  # A *rejected* initializer must seed nothing. `var a: range[0..10] = 20`
  # would otherwise contribute `a == 20` next to `seedRangeFacts`' `0 <= a <= 10`
  # — a contradiction, and from a contradiction the engine proves anything, so
  # the next bad assignment in the same module went unreported.
  let initAccepted = c.errors.len == errMark
  if initAccepted and not initStart.isDotToken:
    var v = VarId(0)
    var k = createXint(0'i32)
    let noSubst = initTable[SymId, Cursor]()
    if pureOperand(c, initStart, noSubst, v, k):
      let a = getVarId(c, name)
      if a != v:
        addAsgnFact c, query(a, v, k)
    elif not sumWithBoundedTerm(c, initStart, getVarId(c, name)):
      addBoundFacts c, getVarId(c, name), initStart
  n = localStart; skip n
  # The local now holds a value proven to be within its range (if any), so
  # record that for downstream obligations that reference this symbol.
  if initAccepted:
    seedRangeFacts c, name, localType
    assumeEnsures c, initStart, getVarId(c, name)

proc traverseAssume(c: var FirContext; n: var Cursor) =
  ## `{.assume: cond.}`: the programmer's word, taken. The one statement that
  ## makes something a fact without proof, and the override for what the prover
  ## cannot discharge — a false assumption is undefined behaviour, and the line
  ## that says so is where to look. `finalir.nim` also states the range of a
  ## `for` loop variable this way (`forRangeAssumes`), taken from the
  ## iterator's `.ensures`. A conjunct the engine cannot model contributes no
  ## fact: there is nothing to report when we fail to understand a statement of
  ## what is true.
  let noSubst = initTable[SymId, Cursor]()
  # `old(e)` in a statement of the body means what `e` held on entry.
  let callSlots = move c.oldSlots
  c.oldSlots = c.ownOldSlots
  n.into:
    assumeCond(c, n, noSubst)
    skip n
  c.oldSlots = callSlots

proc traverseAssert(c: var FirContext; n: var Cursor) =
  ## `{.assert: cond.}`: a claim discharged at compile time and only there.
  ## Nothing is emitted for it at run time, so an assertion the prover does not
  ## prove is an error — undecided as much as violated, and whatever the module's
  ## contract features say. Afterwards `cond` is a fact: it has just been proven,
  ## or an error was reported and assuming it keeps one mistake from cascading.
  ## What cannot be proven has to be stated with `{.assume.}` instead.
  ##
  ## The `(report)` and `(error)` prefixes are the hooks of the contract IR
  ## tests: the first prints the verdict, the second expects the assertion to
  ## be unprovable.
  let info = n.info
  let noSubst = initTable[SymId, Cursor]()
  n.into:
    var report = false
    var expectUnprovable = false
    if n.pragmaKind == ReportP:
      report = true
      skip n
    if n.pragmaKind == ErrorP:
      expectUnprovable = true
      skip n
    let cond = n
    skip n
    # On a path control cannot reach every claim is vacuous (see `checkRequires`).
    if c.tr.live:
      let callSlots = move c.oldSlots
      c.oldSlots = c.ownOldSlots
      let res = proveCond(c, cond, noSubst)
      if expectUnprovable:
        if res == Proven:
          buildErr c, info, "assertion unexpectedly proven: " & asNimCode(cond)
        elif report:
          echo "OK (could indeed not prove) ", asNimCode(cond)
      else:
        case res
        of Proven:
          if report: echo "OK ", asNimCode(cond)
        of Disproven:
          buildErr c, info, "assertion violated: " & asNimCode(cond)
        of Unprovable:
          buildErr c, info, "cannot prove assertion: " & asNimCode(cond)
        assumeCond(c, cond, noSubst)
      c.oldSlots = callSlots

proc traverseProc(c: var FirContext; n: var Cursor) =
  var call = freshCall()
  let decl = n
  # Fresh, journaling flow state (init-set + facts) for this proc; the enclosing
  # proc's state (with its live checkpoints) is restored on the way out.
  let oldFlow = move c.flow
  c.flow = initFlowState()
  c.procCanRaise = false
  let oldInHook = c.inHook
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
  c.inHook = isHookProc(symId)
  var isGeneric = false
  var isExternProc = false
  var ownContract = default(Cursor)
  var ownEnsures = default(Cursor)
  let oldOwnEnsures = c.ownEnsures
  let oldOwnOldSlots = move c.ownOldSlots
  var outParams: seq[SymId] = @[]
  for i in 0 ..< BodyPos:
    if i == ProcPragmasPos:
      c.procCanRaise = hasPragma(n, RaisesP)
      isExternProc = hasPragma(n, ImportcP) or hasPragma(n, ImportcppP)
      ownContract = extractPragma(n, RequiresP)
      # An iterator's `.ensures` is about what it yields, not about an exit.
      if decl.symKind != IteratorY:
        ownEnsures = extractPragma(n, EnsuresP)
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

  # The body may *assume* its own `.requires`: every call site had to discharge
  # it (or is in a `runtimeContracts` module, where the guard hexer emits at the
  # top of this very body establishes it dynamically). Without this a contract
  # could not be passed on to an inner call with the same precondition.
  if not cursorIsNil(ownContract):
    assumeOwnContract c, ownContract
  # ... and has to deliver its own `.ensures`, measured against the state it
  # was entered with.
  c.ownEnsures = ownEnsures
  c.ownOldSlots = initTable[string, VarId]()
  if not cursorIsNil(ownEnsures):
    c.oldSlots.clear()
    snapshotOlds c, ownEnsures, initTable[SymId, Cursor]()
    c.ownOldSlots = move c.oldSlots

  # Analyze body. Generic procs are only checked once instantiated. Extern
  # (importc/importcpp) procs satisfy their contract at the C level and have no
  # meaningful Nim body — and the lowered body of an extern func with a doc /
  # `runnableExamples` body still ends in an implicit `(ret result)` that reads
  # the never-initialized `result`, so we must skip the *traversal*, not merely
  # the final init check.
  if not isGeneric and not isExternProc:
    traverseStmt c, n, call
    # Falling off the end is an exit too.
    verifyEnsures c, (if c.resultSym != NoSymId: getVarId(c, c.resultSym) else: InvalidVarId), decl.info
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
        buildErr c, info, "cannot prove that " & asNimCode(c.resultSym) & " has been initialized"
      for sym in outParams:
        if not isInitialized(c, sym):
          buildErr c, info, "cannot prove that " & asNimCode(sym) & " has been initialized"
  else:
    skip n
  n = procStart; skip n
  c.tr = ensureMove oldTr
  c.flow = ensureMove oldFlow
  c.resultSym = oldResultSym
  c.inlineVars = ensureMove oldInlineVars
  c.activeBorrows = ensureMove oldBorrows
  c.currentProcStart = oldProcStart
  c.ownEnsures = oldOwnEnsures
  c.ownOldSlots = oldOwnOldSlots
  c.inHook = oldInHook

proc traverseStmt(c: var FirContext; n: var Cursor; call: var CallContext) =
  ## `call` holds what the calls of the preceding statement established; an
  ## `(unknown …)` reads it and leaves it for the next one, every other
  ## statement starts it afresh.
  if n.finalIrKind != UnknownV:
    call = freshCall()
  case n.finalIrKind
  of IteV, ItecV:
    traverseIte c, n
  of LoopV:
    traverseLoop c, n
  of StoreV:
    traverseStore c, n, call
  of AssumeV:
    traverseAssume c, n
  of AssertV:
    traverseAssert c, n
  of LabV:
    traverseLabel c, n
  of JmpV:
    traverseJmp c, n
  of MflagV, VflagV, JtrueV:
    # NJVL control-flow flags and their `jtrue` setter. `xelim` used to
    # materialise short-circuit conditions into these for `nj.nim`; that
    # lowering went out with the pass, so nothing produces one any more. The
    # tags stay in the NIF spec (doc/tags.md) for the historical form, so the
    # branch has to exist.
    bug "cfvar in Final IR"
  of KillV:
    # Variables going out of scope. NJ emits one `kill` per scope, which is what
    # makes this the place to catch a borrow whose source dies under it.
    n.into:
      # The whole list has to be read before judging any single entry: a
      # borrower dying alongside its source is precisely what should happen.
      var dying: seq[SymId] = @[]
      var m = n
      while m.hasMore:
        let s = extractSymId(m)
        if s != NoSymId: dying.add s
        skip m
      for b in c.activeBorrows:
        if b.path.len > 0 and b.path[0] in dying and b.borrower notin dying:
          buildErr c, b.info, "borrow of '" & asNimCode(b.path[0]) &
            "' escapes its scope; it does not live long enough"
      # ... then the borrows held *by* the dying variables end here.
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
      if writesThroughPointee(c, n):
        discard "an element behind a pointer: only the heap changed"
      elif unknownPath.path.len > 0 and unknownPath.path[0] in call.views:
        invalidateViewElements(c, unknownPath.path[0])
      elif unknownPath.path.len > 0 and unknownPath.path[0] in call.ensured:
        # The call before this already dropped everything its mutation could
        # reach and restated what its `.ensures` promises.
        discard
      elif unknownPath.path.len > 0:
        let root = getVarId(c, unknownPath.path[0])
        if root != call.stepped:
          invalidateFactsAbout(c.facts, root)
        invalidateDerivedFrom(c, unknownPath.path[0])
      else:
        # No root to attribute the mutation to (a raw pointer, a temporary):
        # anything derived may have changed underneath us.
        invalidateAllDerived(c)
      skip n # the unknown location
  of ContinueV:
    # The loop back-edge.
    skip n
    leaveToContinue(c)
  of VV:
    # Versioned variable reference - should not appear as statement
    skip n
  of EtupatV:
    traverseExpr c, n, call
  of NoVTag:
    case n.stmtKind
    of StmtsS, ScopeS, BlockS:
      n.into:
        while n.hasMore:
          traverseStmt c, n, call
    of CaseS:
      traverseCase c, n
    of TryS:
      traverseTry c, n
    of RetS:
      traverseRet c, n, call
    of RaiseS:
      traverseRaise c, n, call
    of LocalDecls:
      traverseLocal c, n, call
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
      analyseCall c, n, call
    of DiscardS, YldS:
      n.into:
        traverseExpr c, n, call
    of EmitS, InclS, ExclS:
      skip n
    of PragmaxS:
      n.into:
        skip n # pragmas
        while n.hasMore:
          traverseStmt c, n, call
    of NoStmt:
      if n.exprKind in CallKinds:
        analyseCall c, n, call
      elif n.exprKind == PragmaxX:
        n.into:
          skip n # pragmas
          while n.hasMore:
            traverseStmt c, n, call
      elif n.exprKind in {DestroyX, CopyX, WasmovedX, SinkhX, TraceX}:
        n.into:
          traverseExpr c, n, call
          while n.hasMore:
            traverseExpr c, n, call
      else:
        traverseExpr c, n, call
    else:
      # Unknown statement - skip it wholesale
      skip n

proc traverseToplevel(c: var FirContext; n: var Cursor) =
  var call = freshCall()
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
    traverseStmt c, n, call

proc lowerToFinalIr(input: var TokenBuf; moduleSuffix: string; bits: int): TokenBuf =
  ## Run the Final-IR lowering (`finalir.nim`, which itself runs xelim first).
  var n = beginRead(input)
  var buf = createTokenBuf(input.len)
  buf.addSubtree n
  var pass = initPass(move buf, moduleSuffix, "xelim_finalir", bits)
  toFinalIr(pass)
  result = ensureMove pass.dest

proc analyzeContractsFinalIr*(input: var TokenBuf; moduleSuffix: string; features: set[Feature]; bits: int; verbose = false): TokenBuf =
  ## Main entry point: lowers `input` to the Final IR and analyzes contracts.
  ## When `verbose` is true, every contract/init failure dumps the enclosing
  ## proc's IR to stderr to aid debugging.
  var finalBuf = lowerToFinalIr(input, moduleSuffix, bits)

  var c = FirContext(
    errors: initTokenBuf(),
    typeCache: createTypeCache(bits),
    moduleSuffix: moduleSuffix,
    tr: initFlowTracker(),
    flow: initFlowState(),
    substVar: initTable[SymId, VarId](),
    oldSlots: initTable[string, VarId](),
    ownOldSlots: initTable[string, VarId](),
    loopExitLabels: initHashSet[SymId](),
    declaredRange: initTable[VarId, RangeBounds](),
    verbose: verbose,
    features: features,
    bits: bits
  )
  c.typeCache.openScope()
  block:
    # Index the module's transparent accessors before anything asks for one: a
    # generic instance such as `len.3.Ixyz` is declared *here*, not in
    # `programs`, so it cannot be found by `tryLoadSym`.
    var scan = beginRead(finalBuf)
    collectAccessors(c, scan)
    endRead(scan)

  var fin = beginRead(finalBuf)
  traverseToplevel c, fin

  c.typeCache.closeScope()
  result = ensureMove c.errors

when isMainModule:
  import std / [syncio, os]
  proc main(infile: string) =
    var input = parseFromFile(infile)
    # A standalone debug driver: no target, so the host's width is stated.
    discard analyzeContractsFinalIr(input, "main", {}, sizeof(int)*8)

  main(paramStr(1))

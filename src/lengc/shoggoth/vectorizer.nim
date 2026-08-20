#
#
#       NIFC Loop Vectorizer (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Vectorizes canonical counted map-loops into 128-bit AdvSIMD `(instr …)`
## applications (the structured-assembler route: `doc/final_ir.md`, "Final IR as
## a structured assembler"). Target-gated: only the native AArch64 backend
## lowers the emitted rows, so `optdriver` runs this pass only for that target.
##
## ## What is matched
##
## The innermost-loop shape hexer + the earlier shoggoth passes produce for
## `for j in jj ..< n: a[base+j] = pure-lane-wise-expr`:
##
## * `(while (lt iv n) body)` — `iv` an `(i 64)` local, `n` invariant;
## * index locals `(var :I (i 64) iv | (add T inv iv) | (add T iv inv))`;
## * bounds guards `(if (elif (not (lt I len)) (stmts (call panic …))))` and
##   `(if (elif (not (le 0 I)) …))`;
## * pointer temps `(var :P (ptr (f W)) .)` bound once by
##   `(asgn P (haddr (pat BASE I)))`, `BASE` an invariant lvalue path;
## * value locals `(var :E (f W) rhs)` with `rhs` a pure tree over
##   `(deref P)`, invariant scalars and float literals with `add`/`sub`/`mul`;
## * exactly ONE store `(asgn (pat BASE I) src)`, and/or accumulator updates
##   `(asgn S (add [T] S tree))` — S a scalar float local of the enclosing proc
##   (declared by a `(var …)` in this body, its address never taken, verified
##   by a whole-body scan) and `tree` a pure lane tree not mentioning S;
## * the increment `(asgn iv (add T iv 1))` as the final statement.
##
## ## What is emitted — loop versioning, no semantic assumptions
##
## ```
## vecok = false
## if iv + (VF-1) < n:            # at least one full vector iteration
##   if <every bounds guard, evaluated at its worst-case iteration>:
##     if <every load range disjoint from the store range, at runtime>:
##       vecok = true
## if vecok:
##   <pointer inits, broadcasts>
##   while iv + (VF-1) < n:  <vector body>;  iv += VF
## <the ORIGINAL loop, untouched>
## ```
##
## The scalar original serves as both the remainder loop and the fallback, so
## semantics — including which bounds check panics, and with what message —
## are preserved exactly: every index is `iv + const` with a unit step, so each
## guard is monotone in `iv` and its worst case sits at one end of the range;
## and an aliased store (`mmIkj(s, s, s)`-style calls) simply fails the
## disjointness test and runs the scalar loop.
##
## Emitted vector VALUES have type `(f 128)`: an opaque SIMD-register bag of
## bits whose lane meaning lives in the opcode's trailing lane-bits literal —
## see `lib/intrinsics.nim`. `vfmla` accumulates in place (`tie: 0`), so it is
## only fused onto an iteration-fresh accumulator and spelled
## `(asgn acc (instr vfmla acc a b bits))`.
##
## An accumulator update gets an `(f 128)` PARTIAL-SUM local per unroll slot,
## started at all-zero lanes; the scalar S is untouched by the vector loop and
## receives the lane sums ONCE at its end, via `vaddv` (horizontal add):
## `S = S + vaddv(acc)`. This is the one deliberate semantic deviation of the
## pass: lane-splitting a float sum reorders its additions, so the rounding
## can differ from the scalar loop's left-to-right order. The remainder still
## runs the original loop, in the original order.

import std / [tables, sets, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind, tag enums
import ".." / ".." / "models" / tags          # *TagId ordinals for synthesis
import patchsets

type
  VecIndex = object
    sym: SymId              ## the index local
    invSym: SymId           ## SymId(0) → literal displacement
    invLit: int64           ## used when invSym == SymId(0); plain `iv` → 0

  VecAccess = object
    ptrSym: SymId           ## the pointer temp (SymId(0) for the store)
    baseCur: Cursor         ## the invariant BASE lvalue path
    idx: int                ## index into `indexes`

  GuardKind = enum
    gLtLen                  ## `(lt I len)` — worst case at the LAST iteration
    gLeZero                 ## `(le 0 I)` — worst case at the FIRST iteration

  VecGuard = object
    kind: GuardKind
    idx: int                ## which index local the guard tests
    lenCur: Cursor          ## gLtLen: the invariant length expression

  VecValue = object
    sym: SymId              ## the scalar value local
    rhsCur: Cursor          ## its pure defining tree

  VecReduction = object
    sym: SymId              ## the loop-carried scalar accumulator
    treeCur: Cursor         ## the pure lane tree added to it each iteration

  LoopPlan = object
    ivSym: SymId
    boundCur: Cursor        ## `n` — invariant symbol or int literal
    elemBits: int           ## 32 or 64; uniform across every access
    hasStore: bool
    indexes: seq[VecIndex]
    accesses: seq[VecAccess]  ## the loads
    store: VecAccess
    storeSrc: Cursor
    guards: seq[VecGuard]
    values: seq[VecValue]
    reductions: seq[VecReduction]

  VecMode* = enum
    ## Which 128-bit target the emitted `(instr …)` rows are for. The rows are
    ## target-neutral and both modes currently emit exactly the same ones — the
    ## mode names the back end that will lower them, which is where the two will
    ## diverge (a wider AVX2 lane count has nothing to say to AdvSIMD).
    vecOff, vecNeon, vecSse

  Context = object
    orig: ptr TokenBuf
    patchset: Patchset
    synth: seq[TokenBuf]
    tempCounter: int
    moduleSuffix: string
    vecSuffix: string       ## suffix of the module-level intrinsic decls
    allowSub: bool          ## may a lane tree contain `sub`? False on the SSE
                            ## path: `vfsub` has no x86-64 lowering, because the
                            ## nifasm tag space is full and there was no id left
                            ## for `subpd`/`subps` (see `doc/instructions.md`).
                            ## A loop that needs one is left to the scalar code
                            ## rather than failing to compile.
    vectorized*: bool

proc createContext(orig: ptr TokenBuf; moduleSuffix, vecSuffix: string;
                   allowSub: bool): Context =
  Context(orig: orig, patchset: initPatchset(orig), synth: @[],
          tempCounter: 0, moduleSuffix: moduleSuffix, vecSuffix: vecSuffix,
          allowSub: allowSub)

# ── small tree utilities ────────────────────────────────────────────────────

proc sameTree(a, b: Cursor): bool =
  ## Structural equality of two subtrees, ignoring line info. Recursive rather
  ## than a flat token walk: `inc` advances by a token's full width (line-info
  ## suffixes included) while `subtreeWidth` counts slots, so a flat walk
  ## overruns on real, line-info-carrying files.
  if a.kind != b.kind:
    result = false
  else:
    case a.kind
    of TagLit:
      if a.cursorTagId != b.cursorTagId:
        result = false
      else:
        var x = sub(a)
        var y = sub(b)
        result = true
        while result and x.hasMore and y.hasMore:
          result = sameTree(x, y)
          skip x
          skip y
        result = result and x.hasMore == y.hasMore
    of Symbol, SymbolDef:
      result = symId(a) == symId(b)
    of IntLit, UIntLit:
      result = intVal(a) == intVal(b)
    of FloatLit:
      result = floatVal(a) == floatVal(b)
    else:
      result = true

proc collectAssigned(n: Cursor; assigned: var HashSet[SymId]) =
  ## Every symbol the subtree writes: `asgn`/`store` targets and every `var`
  ## declaration. Anything in this set is NOT loop-invariant.
  if n.hasMore and n.kind == TagLit:
    if n.stmtKind in {AsgnS, StoreS}:
      var lhs = n
      inc lhs
      if n.stmtKind == StoreS: skip lhs
      if lhs.kind == Symbol: assigned.incl symId(lhs)
    elif n.stmtKind in {VarS, GvarS, TvarS, ConstS}:
      var d = n
      inc d
      if d.kind == SymbolDef: assigned.incl symId(d)
    var m = n
    m.loopInto:
      collectAssigned(m, assigned)
      skip m

# ── matching ────────────────────────────────────────────────────────────────

proc skipOptType(c: var Cursor) =
  ## Comparisons/arithmetic may or may not carry a leading type child.
  if c.kind == TagLit and c.typeKind != NoType: skip c

proc sufFloatLit(n: Cursor): tuple[ok: bool; bits: int; inner: Cursor] =
  ## `(suf 0.0 "f32"/"f64")` → (true, 32/64, the literal); else ok=false.
  result = (false, 0, default(Cursor))
  if n.kind == TagLit and n.exprKind == SufC:
    var c = sub(n)
    if c.hasMore and c.kind == FloatLit:
      let inner = c
      skip c
      if c.hasMore and c.kind == StrLit:
        let suf = strVal(c)
        skip c
        if not c.hasMore and suf in ["f32", "f64"]:
          result = (true, (if suf == "f32": 32 else: 64), inner)

proc floatLeafBits(n: Cursor): int =
  ## The element width a literal leaf implies: 64 for a bare float literal,
  ## the suffix's width for a `(suf …)`; 0 for non-literals.
  if n.kind == FloatLit:
    result = 64
  else:
    let (ok, bits, _) = sufFloatLit(n)
    result = if ok: bits else: 0

type
  LocalKind = enum
    ## Every local the loop grammar admits plays exactly ONE role.
    lkNone            ## not a loop-body local
    lkPtrPending      ## `(var :P (ptr (f W)) .)` — declared, not yet bound
    lkPtrBound        ## … after its `(asgn P (haddr (pat BASE I)))`
    lkIndex           ## `(var :I (i 64) idx-expr)`
    lkValue           ## `(var :E (f W) lane-tree)`

  LocalInfo = object
    kind: LocalKind
    bits: int         ## ptr temps: the pointee's element width

  Matcher = object
    ## The loop matcher's shared state, threaded as `m` through the match
    ## procs below — the match-side sibling of `Emitter`. `locals` is the one
    ## sym → role map; `assigned` the body's write set (so absence from it
    ## means loop-invariant).
    plan: LoopPlan
    assigned: HashSet[SymId]
    locals: Table[SymId, LocalInfo]
    pendingPtrs: int  ## how many locals are still lkPtrPending
    sawStore, sawInc: bool

proc roleOf(m: Matcher; s: SymId): LocalKind =
  getOrDefault(m.locals, s).kind              # absent → lkNone

proc matchWhileHead(loop: Cursor; ivSym: var SymId; bound: var Cursor): bool =
  ## `(while (lt [T] iv n) body)` — `n` a symbol or int literal.
  result = false
  var c = sub(loop)
  if c.hasMore and c.kind == TagLit and c.exprKind == LtC:
    var e = sub(c)
    skipOptType e
    if e.kind == Symbol:
      ivSym = symId(e)
      skip e
      if e.hasMore and e.kind in {Symbol, IntLit}:
        bound = e
        skip e
        result = not e.hasMore

proc matchIvInc(n: Cursor; ivSym: SymId): bool =
  ## `(asgn iv (add T iv 1))`, the `1` possibly `(conv T 1)`-wrapped.
  result = false
  if n.stmtKind == AsgnS:
    var c = sub(n)
    if c.kind == Symbol and symId(c) == ivSym:
      skip c
      if c.hasMore and c.kind == TagLit and c.exprKind == AddC:
        var e = sub(c)
        skipOptType e
        if e.kind == Symbol and symId(e) == ivSym:
          skip e
          if e.hasMore:
            var isOne = false
            if e.kind == TagLit and e.exprKind in {ConvC, CastC}:
              var one = sub(e)
              skip one                        # the target type
              if one.hasMore and one.kind == IntLit and intVal(one) == 1:
                skip one
                isOne = not one.hasMore
            elif e.kind == IntLit and intVal(e) == 1:
              isOne = true
            if isOne:
              skip e
              result = not e.hasMore

proc matchIndexExpr(m: Matcher; e0: Cursor; ix: var VecIndex): bool =
  ## `iv` | `(add T inv iv)` | `(add T iv inv)` — inv an invariant symbol or
  ## an int literal.
  var e = e0
  if e.kind == Symbol and symId(e) == m.plan.ivSym:
    ix.invSym = SymId(0)
    ix.invLit = 0
    result = true
  elif e.kind == TagLit and e.exprKind == AddC:
    var c = sub(e)
    skipOptType c
    var sawIv = false
    var sawInv = false
    var operandsOk = true
    for which in 0 ..< 2:
      if not (operandsOk and c.hasMore):
        operandsOk = false
      elif c.kind == Symbol and symId(c) == m.plan.ivSym and not sawIv:
        sawIv = true
        skip c
      elif c.kind == Symbol and symId(c) notin m.assigned and not sawInv:
        ix.invSym = symId(c)
        sawInv = true
        skip c
      elif c.kind == IntLit and not sawInv:
        ix.invSym = SymId(0)
        ix.invLit = intVal(c)
        sawInv = true
        skip c
      else:
        operandsOk = false
    result = operandsOk and sawIv and sawInv and not c.hasMore
  else:
    result = false

proc invariantBase(n: Cursor; assigned: HashSet[SymId]): bool =
  ## An lvalue path whose every mentioned symbol is loop-invariant and whose
  ## shape is symbols, field selections and derefs only (no calls, no indexing).
  case n.kind
  of Symbol:
    result = symId(n) notin assigned
  of TagLit:
    if n.exprKind in {DotC, DerefC}:
      result = true
      var c = sub(n)
      while result and c.hasMore:
        result = invariantBase(c, assigned)
        skip c
    else:
      result = false
  of IntLit, UIntLit:
    result = true                             # a `dot`'s inheritance depth
  else:
    result = false

proc knownIndex(plan: LoopPlan; isym: SymId): int =
  ## `isym`'s slot in `plan.indexes`, or -1.
  result = -1
  for i in 0 ..< plan.indexes.len:
    if plan.indexes[i].sym == isym: result = i

proc matchPatLval(m: Matcher; n: Cursor; acc: var VecAccess): bool =
  ## `(pat BASE I)` with invariant BASE and known index local I.
  result = false
  if n.kind == TagLit and n.exprKind == PatC:
    var pc = sub(n)
    if pc.hasMore:
      acc.baseCur = pc
      if invariantBase(pc, m.assigned):
        skip pc
        if pc.hasMore and pc.kind == Symbol:
          acc.idx = knownIndex(m.plan, symId(pc))
          skip pc
          result = acc.idx >= 0 and not pc.hasMore

proc matchGuardCmp(m: Matcher; nc: Cursor; g: var VecGuard): bool =
  ## `(lt I len)` (len an invariant path) or `(le 0 I)` over a known index
  ## local I.
  result = false
  if nc.hasMore and nc.kind == TagLit and nc.exprKind in {LtC, LeC}:
    var cc = sub(nc)
    skipOptType cc
    if nc.exprKind == LtC:
      if cc.hasMore and cc.kind == Symbol:
        let isym = symId(cc)
        skip cc
        if cc.hasMore:
          g = VecGuard(kind: gLtLen, idx: knownIndex(m.plan, isym), lenCur: cc)
          let invariantLen = invariantBase(cc, m.assigned)
          skip cc
          result = invariantLen and g.idx >= 0 and not cc.hasMore
    elif cc.hasMore and cc.kind == IntLit and intVal(cc) == 0:
      skip cc
      if cc.hasMore and cc.kind == Symbol:
        g = VecGuard(kind: gLeZero, idx: knownIndex(m.plan, symId(cc)))
        skip cc
        result = g.idx >= 0 and not cc.hasMore

proc matchGuard(m: Matcher; n: Cursor; g: var VecGuard): bool =
  ## `(if (elif (not COND) (stmts (call sym …))))` where COND is
  ## `(lt I len)` or `(le 0 I)` over a known index local. The action's contents
  ## are irrelevant: the vector path only runs when the condition is FALSE for
  ## every iteration it covers, so the call can never fire there.
  result = false
  if n.stmtKind == IfS:
    var c = sub(n)
    if c.hasMore and c.kind == TagLit and c.substructureKind == ElifU:
      var e = sub(c)
      skip c
      # a single `elif`, no `else`; the condition is `(not CMP)`
      if not c.hasMore and e.hasMore and e.kind == TagLit and e.exprKind == NotC:
        var nc = sub(e)
        if matchGuardCmp(m, nc, g):
          skip nc
          if not nc.hasMore:                  # the `(not …)` has one child
            skip e                            # past the condition
            if e.hasMore and e.kind == TagLit and e.stmtKind == StmtsS:
              var act = sub(e)
              var nCalls = 0
              var onlyCalls = true
              while act.hasMore:
                if act.kind == TagLit and act.stmtKind == CallS: inc nCalls
                else: onlyCalls = false
                skip act
              skip e
              result = onlyCalls and nCalls == 1 and not e.hasMore
  when defined(vecDbg):
    if not result:
      var db = createTokenBuf(16, n.pool, n.tags)
      db.addSubtree n
      echo "  guard reject: ", toString(db)

proc pureLaneTree(m: Matcher; n: Cursor): bool =
  ## leaves: `(deref P)` (P a pointer temp), invariant scalar symbols,
  ## value locals, float literals; interior: binary `add`/`sub`/`mul`.
  case n.kind
  of FloatLit: result = true
  of Symbol:
    result = m.roleOf(symId(n)) == lkValue or symId(n) notin m.assigned
  of TagLit:
    if sufFloatLit(n).ok:
      result = true
    else:
      case n.exprKind
      of DerefC:
        var c = sub(n)
        result = c.hasMore and c.kind == Symbol and
                 m.roleOf(symId(c)) in {lkPtrPending, lkPtrBound}
        if result:
          skip c
          result = not c.hasMore
      of AddC, SubC, MulC:
        var c = sub(n)
        skipOptType c
        var arity = 0
        result = true
        while c.hasMore:
          inc arity
          if not pureLaneTree(m, c): result = false
          skip c
        if arity != 2: result = false
      else: result = false
  else: result = false

proc flattenStmts(n: Cursor; stmts: var seq[Cursor]) =
  if n.kind == TagLit and n.stmtKind in {StmtsS, ScopeS}:
    var m = n
    m.loopInto:
      flattenStmts(m, stmts)
      skip m
  elif n.kind == DotToken:
    discard
  else:
    stmts.add n

proc scanLitBits(n: Cursor; bits: var int; conflict: var bool) =
  ## Unify `bits` with the element width every literal leaf of `n` implies.
  let lb = floatLeafBits(n)
  if lb != 0:
    if bits == 0: bits = lb
    elif bits != lb: conflict = true
  elif n.kind == TagLit and n.exprKind in {AddC, SubC, MulC}:
    var cc = sub(n)
    skipOptType cc
    while cc.hasMore:
      scanLitBits(cc, bits, conflict)
      skip cc

proc matchVarStmt(m: var Matcher; n: Cursor): bool =
  ## One of the three local kinds the loop grammar allows: a pointer temp
  ## `(var :P (ptr (f W)) .)`, an index local `(var :I (i 64) idx-expr)` or a
  ## value local `(var :E (f W) pure-lane-tree)`. Records the local's role.
  result = false
  var d = sub(n)
  if d.hasMore and d.kind == SymbolDef:
    let nm = symId(d)
    inc d                                     # past the name
    skip d                                    # pragmas
    if d.hasMore:
      if d.kind == TagLit and d.typeKind == PtrT:
        # a pointer temp, bound later
        var pt = sub(d)
        var bits = 0
        if pt.hasMore and pt.kind == TagLit and pt.typeKind == FT:
          var b = pt
          inc b
          if b.kind == IntLit: bits = int(intVal(b))
        skip d                                # past the type
        if bits in {32, 64} and (not d.hasMore or d.kind == DotToken):
          m.locals[nm] = LocalInfo(kind: lkPtrPending, bits: bits)
          inc m.pendingPtrs
          result = true
      elif d.kind == TagLit and d.typeKind == IT:
        skip d                                # past the type
        var ix = VecIndex(sym: nm)
        if d.hasMore and d.kind != DotToken and matchIndexExpr(m, d, ix):
          m.plan.indexes.add ix
          m.locals[nm] = LocalInfo(kind: lkIndex)
          result = true
      elif d.kind == TagLit and d.typeKind == FT:
        var b = d
        inc b
        let bits = if b.kind == IntLit: int(intVal(b)) else: 0
        skip d                                # past the type
        if d.hasMore and d.kind != DotToken and pureLaneTree(m, d) and
           (m.plan.elemBits == 0 or bits == m.plan.elemBits):
          m.plan.elemBits = bits
          m.plan.values.add VecValue(sym: nm, rhsCur: d)
          m.locals[nm] = LocalInfo(kind: lkValue)
          result = true

proc matchPtrBind(m: var Matcher; a0: Cursor; p: SymId): bool =
  ## The rhs of `(asgn P (haddr (pat BASE I)))` — bind the pointer temp.
  result = false
  var a = a0
  if a.hasMore and a.kind == TagLit and a.exprKind == HaddrC:
    var h = sub(a)
    var acc = VecAccess(ptrSym: p)
    if h.hasMore and matchPatLval(m, h, acc):
      skip h
      let bits = m.locals[p].bits
      if not h.hasMore and (m.plan.elemBits == 0 or bits == m.plan.elemBits):
        m.plan.elemBits = bits
        m.plan.accesses.add acc
        m.locals[p].kind = lkPtrBound
        dec m.pendingPtrs
        result = true

proc matchStore(m: var Matcher; a0: Cursor): bool =
  ## `(asgn (pat BASE I) pure-lane-tree)` — the ONE store.
  result = false
  var a = a0
  var acc = VecAccess(ptrSym: SymId(0))
  if matchPatLval(m, a, acc):
    skip a                                    # past the lvalue
    if a.hasMore and pureLaneTree(m, a):
      m.plan.store = acc
      m.plan.storeSrc = a
      skip a
      result = not a.hasMore

proc matchReduction(m: var Matcher; a0: Cursor; s: SymId): bool =
  ## The rhs of `(asgn S rhs)` for an outer scalar S: `(add [T] S tree)` or
  ## `(add [T] tree S)` with `tree` a pure lane tree — a sum accumulation.
  ## Every OTHER use of S in the loop rejects on its own: S is in `assigned`,
  ## so it is not an invariant leaf, not an index and not part of a base.
  result = false
  var a = a0
  if a.hasMore and a.kind == TagLit and a.exprKind == AddC:
    var c = sub(a)
    var bitsOk = true
    if c.kind == TagLit and c.typeKind != NoType:
      # the add's type child, when present, must be the loop's float element
      # type — an integer type here is an integer reduction, which these
      # float instructions cannot carry
      if c.typeKind == FT:
        var b = c
        inc b
        let bits = if b.kind == IntLit: int(intVal(b)) else: 0
        if m.plan.elemBits == 0 and bits in {32, 64}:
          m.plan.elemBits = bits
        bitsOk = bits == m.plan.elemBits
      else:
        bitsOk = false
      skip c
    if bitsOk and c.hasMore:
      let x = c
      skip c
      if c.hasMore:
        let y = c
        skip c
        if not c.hasMore:
          var tree = default(Cursor)
          var found = false
          if x.kind == Symbol and symId(x) == s:
            tree = y
            found = true
          elif y.kind == Symbol and symId(y) == s:
            tree = x
            found = true
          if found and pureLaneTree(m, tree):
            m.plan.reductions.add VecReduction(sym: s, treeCur: tree)
            result = true

proc scanAccUses(n: Cursor; s: SymId; bits: int;
                 declOk, addrTaken: var bool) =
  if n.hasMore and n.kind == TagLit:
    if n.stmtKind == VarS:
      var d = sub(n)
      if d.hasMore and d.kind == SymbolDef and symId(d) == s:
        inc d                                 # past the name
        skip d                                # pragmas
        if d.hasMore and d.kind == TagLit and d.typeKind == FT:
          var b = d
          inc b
          if b.kind == IntLit and int(intVal(b)) == bits: declOk = true
    elif n.exprKind in {HaddrC, AddrC}:
      var c = sub(n)
      if c.hasMore and c.kind == Symbol and symId(c) == s: addrTaken = true
    var m = n
    m.loopInto:
      scanAccUses(m, s, bits, declOk, addrTaken)
      skip m

proc accSafeLocal(c: var Context; s: SymId; bits: int): bool =
  ## The vector loop keeps a reduction's partial sums in registers and writes
  ## the scalar ONCE, after — so nothing may observe or alias the scalar
  ## through memory mid-loop. Sufficient and checkable: S is declared by a
  ## `(var :S … (f bits) …)` in this body (a proc-local — not a param, not a
  ## global another module could point into) and no `(haddr S)`/`(addr S)`
  ## appears anywhere in it.
  var declOk = false
  var addrTaken = false
  scanAccUses(cursorAt(c.orig[], 0), s, bits, declOk, addrTaken)
  result = declOk and not addrTaken

proc matchLoop(c: var Context; loop: Cursor; plan: var LoopPlan): bool =
  ## Match the whole canonical loop; fill `plan`. Any unexpected statement
  ## rejects — the pass must understand EVERYTHING it vectorizes.
  when defined(vecDbg): echo "vectorizer: trying a while loop"
  result = false
  var m = Matcher()
  var bound = default(Cursor)
  if matchWhileHead(loop, m.plan.ivSym, bound):
    m.plan.boundCur = bound
    var body = loop
    inc body                                  # past `(while`
    skip body                                 # past the condition

    collectAssigned(body, m.assigned)
    m.assigned.incl m.plan.ivSym
    if not (bound.kind == Symbol and symId(bound) in m.assigned):
      var stmts: seq[Cursor] = @[]
      flattenStmts(body, stmts)

      # the iv itself is a valid access index (`c[iv]` — the fill loops); its
      # displacement is zero
      m.plan.indexes.add VecIndex(sym: m.plan.ivSym, invSym: SymId(0),
                                  invLit: 0)

      var ok = true
      var si = 0
      while ok and si < stmts.len:
        let n = stmts[si]
        if n.kind != TagLit:
          ok = false
        elif m.sawInc:
          # only inert labels may trail the increment (inliner returnLabels)
          ok = n.stmtKind == LabS
        else:
          case n.stmtKind
          of VarS:
            ok = matchVarStmt(m, n)
          of AsgnS:
            if matchIvInc(n, m.plan.ivSym):
              m.sawInc = true
            else:
              var a = sub(n)
              if not a.hasMore:
                ok = false
              elif a.kind == Symbol and m.roleOf(symId(a)) == lkPtrPending:
                let p = symId(a)
                skip a
                ok = matchPtrBind(m, a, p)
              elif a.kind == TagLit and a.exprKind == PatC:
                ok = not m.sawStore and matchStore(m, a)
                m.sawStore = ok
              elif a.kind == Symbol and m.roleOf(symId(a)) == lkNone:
                # an outer scalar: only a sum accumulation is admitted
                let s = symId(a)
                skip a
                ok = matchReduction(m, a, s)
              else:
                ok = false
          of IfS:
            var g = VecGuard(idx: -1)
            ok = matchGuard(m, n, g)
            if ok: m.plan.guards.add g
          of LabS:
            discard   # inliner residue; jumps would appear as stmts and reject
          else:
            ok = false
        when defined(vecDbg):
          if not ok:
            var db = createTokenBuf(8, n.pool, n.tags)
            db.addSubtree n
            echo "vectorizer reject at stmt ", si, ": ", toString(db)
        inc si

      # literal leaves imply a width too (the zero-fill loop has ONLY
      # literals); unify it with the access-implied one
      var litConflict = false
      for v in m.plan.values:
        scanLitBits(v.rhsCur, m.plan.elemBits, litConflict)
      if m.sawStore: scanLitBits(m.plan.storeSrc, m.plan.elemBits, litConflict)
      for r in m.plan.reductions:
        scanLitBits(r.treeCur, m.plan.elemBits, litConflict)
      m.plan.hasStore = m.sawStore
      result = ok and not litConflict and
               (m.sawStore or m.plan.reductions.len > 0) and m.sawInc and
               m.plan.elemBits in {32, 64} and m.pendingPtrs == 0
      # every accumulator must still be an OUTER scalar (no later `(var :S …)`
      # in the body claimed it a role) and provably register-private
      var ri = 0
      while result and ri < m.plan.reductions.len:
        result = m.roleOf(m.plan.reductions[ri].sym) == lkNone and
                 accSafeLocal(c, m.plan.reductions[ri].sym, m.plan.elemBits)
        inc ri
      if result:
        swap(plan, m.plan)
      when defined(vecDbg):
        if ok and not result:
          echo "vectorizer reject: final (store=", m.sawStore, " reds=",
               m.plan.reductions.len, " inc=", m.sawInc, " bits=",
               m.plan.elemBits, " pending=", m.pendingPtrs, " litConflict=",
               litConflict, ")"
    else:
      when defined(vecDbg): echo "vectorizer reject: bound assigned in loop"
  else:
    when defined(vecDbg):
      var wc = sub(loop)
      if wc.hasMore:
        var db = createTokenBuf(8, wc.pool, wc.tags)
        db.addSubtree wc
        echo "vectorizer reject: while head: ", toString(db)
      else:
        echo "vectorizer reject: while head (empty)"

# ── synthesis ───────────────────────────────────────────────────────────────

template tid(x: untyped): TagId = TagId(ord(x))

proc addI64(b: var TokenBuf) =
  b.openTag tid(ITagId)
  b.addIntLit 64
  b.closeTag()

proc addF(b: var TokenBuf; bits: int) =
  b.openTag tid(FTagId)
  b.addIntLit bits
  b.closeTag()

proc addBoolType(b: var TokenBuf) =
  b.openTag tid(BoolTagId)
  b.closeTag()

proc addBoolLit(b: var TokenBuf; v: bool) =
  b.openTag(if v: tid(TrueTagId) else: tid(FalseTagId))
  b.closeTag()

type
  IdxTail = enum
    ## The three iteration points an emitted index expression is evaluated at.
    ## An enum instead of a `tail: proc ()` parameter: every call site would
    ## otherwise allocate a closure per emitted expression.
    tailIv            ## `… + iv` — the current (first covered) iteration
    tailBound         ## `… + n` — one past the last covered iteration
    tailBoundMinus1   ## `… + (n - 1)` — the last covered iteration

proc addIvUse(b: var TokenBuf; plan: LoopPlan) =
  b.addSymUse plan.ivSym

proc addBoundUse(b: var TokenBuf; plan: LoopPlan) =
  b.addSubtree plan.boundCur

proc addIdxTail(b: var TokenBuf; plan: LoopPlan; tail: IdxTail) =
  case tail
  of tailIv: b.addIvUse plan
  of tailBound: b.addBoundUse plan
  of tailBoundMinus1:
    b.openTag tid(SubTagId)
    b.addI64()
    b.addBoundUse plan
    b.addIntLit 1
    b.closeTag()

proc addIdxExpr(b: var TokenBuf; ix: VecIndex; plan: LoopPlan; tail: IdxTail) =
  ## `inv + <tail>`, or plain `<tail>` when the displacement is zero.
  if ix.invSym == SymId(0) and ix.invLit == 0:
    b.addIdxTail(plan, tail)
  else:
    b.openTag tid(AddTagId)
    b.addI64()
    if ix.invSym != SymId(0): b.addSymUse ix.invSym
    else: b.addIntLit ix.invLit
    b.addIdxTail(plan, tail)
    b.closeTag()

proc addVarDeclHead(b: var TokenBuf; name: string) =
  ## Opens `(var :name .` — the caller adds the type and the initializer and
  ## closes the tag.
  b.openTag tid(VarTagId)
  b.addSymDef name
  b.addDotToken()

proc addRunCond(b: var TokenBuf; plan: LoopPlan; vf: int) =
  ## `(lt (add (i 64) iv VF-1) n)`
  b.openTag tid(LtTagId)
  b.openTag tid(AddTagId)
  b.addI64()
  b.addIvUse plan
  b.addIntLit int64(vf - 1)
  b.closeTag()
  b.addBoundUse plan
  b.closeTag()

proc addHaddrPatE(b: var TokenBuf; base: Cursor; ix: VecIndex; plan: LoopPlan;
                  tail: IdxTail) =
  ## `(haddr (pat BASE inv+<tail>))` — the index inlined as an expression,
  ## so no one-shot index local claims a register home (mmTiled64's six-deep
  ## nest ran the allocator dry on those).
  b.openTag tid(HaddrTagId)
  b.openTag tid(PatTagId)
  b.addSubtree base
  b.addIdxExpr(ix, plan, tail)
  b.closeTag()
  b.closeTag()

proc addCmpAddrs(b: var TokenBuf; plan: LoopPlan;
                 baseX: Cursor; ixX: VecIndex; tailX: IdxTail;
                 baseY: Cursor; ixY: VecIndex; tailY: IdxTail) =
  ## `(le (cast (u 64) (haddr (pat X …))) (cast (u 64) (haddr (pat Y …))))`
  b.openTag tid(LeTagId)
  for which in 0 ..< 2:
    b.openTag tid(CastTagId)
    b.openTag tid(UTagId)
    b.addIntLit 64
    b.closeTag()
    if which == 0: b.addHaddrPatE(baseX, ixX, plan, tailX)
    else: b.addHaddrPatE(baseY, ixY, plan, tailY)
    b.closeTag()
  b.closeTag()

type
  UniquePtr = object
    name: string            ## the running-pointer local of the vector loop
    baseCur: Cursor
    idx: VecIndex
    hasLoad: bool
    loadName: string        ## the per-iteration `(f 128)` load local
    derefCount: int         ## total `(deref P)` occurrences over all trees

proc sameIndexInfo(a, b: VecIndex): bool =
  a.invSym == b.invSym and (a.invSym != SymId(0) or a.invLit == b.invLit)

type
  Emitter = object
    ## Everything `emitReplacement` and the top-level emission procs below
    ## share. One flat object threaded as `e: var Emitter` instead of nested
    ## procs: closure environments would be heap-allocated per matched loop —
    ## and per emitted expression for callback-taking helpers.
    b: TokenBuf             ## the replacement tree under construction
    plan: LoopPlan
    vf, bits: int
    tempCounter: int
    moduleSuffix: string
    iFldrq, iFstrq, iVfadd, iVfsub, iVfmul, iVfmla, iVdup, iVaddv: string
    ptrs: seq[UniquePtr]    ## unique access points (loads + store)
    ptrOfAccess: Table[SymId, int]  ## load ptr temp → ptrs index
    storePtr: int           ## -1 when the loop only reduces
    bcSyms: Table[SymId, string]    ## invariant scalar → broadcast local
    bcLits: Table[int64, string]    ## float bit pattern → broadcast local
    valueSet: HashSet[SymId]
    valueVec: Table[SymId, string]  ## value local → its `(f 128)` local
    accNames: seq[array[2, string]] ## per reduction: partial sum per slot
    unrolled: bool

proc freshName(e: var Emitter; kind: string): string =
  inc e.tempCounter
  result = "vec." & kind & "." & $e.tempCounter & "." & e.moduleSuffix

proc uniquePtr(e: var Emitter; acc: VecAccess): int =
  result = 0
  while result < e.ptrs.len and
        not (sameTree(e.ptrs[result].baseCur, acc.baseCur) and
             sameIndexInfo(e.ptrs[result].idx, e.plan.indexes[acc.idx])):
    inc result
  if result == e.ptrs.len:
    e.ptrs.add UniquePtr(name: e.freshName("p"), baseCur: acc.baseCur,
                         idx: e.plan.indexes[acc.idx])

proc countDerefs(e: var Emitter; n: Cursor) =
  ## Total `(deref P)` occurrences per access point, for the fmla in-place
  ## fusion rule.
  if n.kind == Symbol:
    if symId(n) in e.ptrOfAccess:
      inc e.ptrs[e.ptrOfAccess[symId(n)]].derefCount
  elif n.kind == TagLit:
    var x = sub(n)
    while x.hasMore:
      countDerefs(e, x)
      skip x

proc addBroadcastDecl(e: var Emitter; leaf: Cursor): string =
  ## `(var :bc.N . (f 128) (instr vdup LEAF bits))`; returns the local's name.
  result = e.freshName("bc")
  e.b.addVarDeclHead result
  e.b.addF(128)
  e.b.openTag tid(InstrTagId)
  e.b.addSymUse e.iVdup
  e.b.addSubtree leaf
  e.b.addIntLit int64(e.bits)
  e.b.closeTag()
  e.b.closeTag()                              # var

proc collectBroadcasts(e: var Emitter; n: Cursor) =
  ## Broadcast declarations for the invariant scalar / literal leaves.
  case n.kind
  of Symbol:
    let s = symId(n)
    if s notin e.valueSet and s notin e.bcSyms:
      let nm = addBroadcastDecl(e, n)
      e.bcSyms[s] = nm
  of FloatLit:
    let key = cast[int64](floatVal(n))
    if key notin e.bcLits:
      let nm = addBroadcastDecl(e, n)
      e.bcLits[key] = nm
  of TagLit:
    let (isSuf, _, sinner) = sufFloatLit(n)
    if isSuf:
      let key = cast[int64](floatVal(sinner))
      if key notin e.bcLits:
        let nm = addBroadcastDecl(e, n)       # the WHOLE (suf …), typed f32
        e.bcLits[key] = nm
    elif n.exprKind in {AddC, SubC, MulC}:
      var cc = sub(n)
      skipOptType cc
      while cc.hasMore:
        collectBroadcasts(e, cc)
        skip cc
    # `(deref P)` contributes nothing
  else: discard

proc vecBin(e: var Emitter; op, aName, bName: string): string =
  result = e.freshName("t")
  e.b.addVarDeclHead result
  e.b.addF(128)
  e.b.openTag tid(InstrTagId)
  e.b.addSymUse op
  e.b.addSymUse aName
  e.b.addSymUse bName
  e.b.addIntLit int64(e.bits)
  e.b.closeTag()
  e.b.closeTag()                              # var

proc isMulTree(n: Cursor): bool =
  n.kind == TagLit and n.exprKind == MulC

proc countSymUses(n: Cursor; s: SymId; count: var int) =
  if n.kind == Symbol:
    if symId(n) == s: inc count
  elif n.kind == TagLit:
    var c = sub(n)
    while c.hasMore:
      countSymUses(c, s, count)
      skip c

proc addAccDecl(e: var Emitter; name: string) =
  ## `(var :acc . (f 128) (instr vdup 0.0 bits))` — an all-zero-lane start;
  ## the scalar accumulator stays untouched until after the vector loop.
  e.b.addVarDeclHead name
  e.b.addF(128)
  e.b.openTag tid(InstrTagId)
  e.b.addSymUse e.iVdup
  if e.bits == 32:
    e.b.openTag tid(SufTagId)
    e.b.addFloatLit 0.0
    e.b.addStrLit "f32"
    e.b.closeTag()
  else:
    e.b.addFloatLit 0.0
  e.b.addIntLit int64(e.bits)
  e.b.closeTag()
  e.b.closeTag()                              # var

proc accUpdate(e: var Emitter; accN, op: string; opnds: openArray[string]) =
  ## `(asgn acc (instr OP acc opnds… bits))` — accumulate in place, the
  ## spelling both `vfmla`'s tie and `vfadd`'s in-place write accept.
  e.b.openTag tid(AsgnTagId)
  e.b.addSymUse accN
  e.b.openTag tid(InstrTagId)
  e.b.addSymUse op
  e.b.addSymUse accN
  for o in opnds: e.b.addSymUse o
  e.b.addIntLit int64(e.bits)
  e.b.closeTag()
  e.b.closeTag()

proc vecEval(e: var Emitter; n: Cursor; fresh: var bool): string =
  ## Lane-wise evaluation; returns the name of the `(f 128)` local holding the
  ## value. `fresh` reports whether that local is iteration-fresh (mutable, so
  ## an enclosing fmla may accumulate into it in place).
  case n.kind
  of Symbol:
    fresh = false                              # a named value may be multi-use
    let s = symId(n)
    if s in e.valueVec:
      result = e.valueVec[s]
    else:
      result = e.bcSyms[s]
  of FloatLit:
    fresh = false
    result = e.bcLits[cast[int64](floatVal(n))]
  of TagLit:
    let (isSuf, _, sinner) = sufFloatLit(n)
    if isSuf:
      fresh = false
      result = e.bcLits[cast[int64](floatVal(sinner))]
    else:
      case n.exprKind
      of DerefC:
        var cc = sub(n)
        let k = e.ptrOfAccess[symId(cc)]
        fresh = e.ptrs[k].derefCount == 1
        result = e.ptrs[k].loadName
      of AddC, SubC, MulC:
        let kind = n.exprKind
        var cc = sub(n)
        skipOptType cc
        let lhs = cc
        skip cc
        let rhs = cc
        fresh = true
        if kind == AddC and (isMulTree(lhs) or isMulTree(rhs)):
          # fused multiply-add: acc + a*b, when acc is iteration-fresh
          let mulSide = if isMulTree(rhs): rhs else: lhs
          let accSide = if isMulTree(rhs): lhs else: rhs
          var mc = sub(mulSide)
          skipOptType mc
          let ma = mc
          skip mc
          let mb = mc
          var fA, fB, fAcc = false
          let aN = vecEval(e, ma, fA)
          let bN = vecEval(e, mb, fB)
          let accN = vecEval(e, accSide, fAcc)
          if fAcc:
            # (asgn acc (instr vfmla acc a b bits)) — in place, no copy
            e.b.openTag tid(AsgnTagId)
            e.b.addSymUse accN
            e.b.openTag tid(InstrTagId)
            e.b.addSymUse e.iVfmla
            e.b.addSymUse accN
            e.b.addSymUse aN
            e.b.addSymUse bN
            e.b.addIntLit int64(e.bits)
            e.b.closeTag()
            e.b.closeTag()
            result = accN
          else:
            let mN = vecBin(e, e.iVfmul, aN, bN)
            result = vecBin(e, e.iVfadd, mN, accN)
        else:
          var fL, fR = false
          let lN = vecEval(e, lhs, fL)
          let rN = vecEval(e, rhs, fR)
          result = case kind
                   of AddC: vecBin(e, e.iVfadd, lN, rN)
                   of SubC: vecBin(e, e.iVfsub, lN, rN)
                   else: vecBin(e, e.iVfmul, lN, rN)
      else:
        raiseAssert "vectorizer: unexpected lane tree"
  else:
    raiseAssert "vectorizer: unexpected lane leaf"

proc emitSlot(e: var Emitter; byteOff: int) =
  ## One vector-width strip of the loop body: fresh loads at `byteOff`, the
  ## lane-wise computation, and the store back at `byteOff`. An unrolled
  ## iteration emits the slots SEQUENTIALLY (slot 0's store precedes slot 1's
  ## loads): correct because the `vecok` disjointness/identity guarantees make
  ## every same-iteration store range distinct from every load range except
  ## the same-address case, which reads its own slot's offset — and the OOO
  ## core overlaps the independent slots on its own. A slot's vector temps
  ## (loads and op temps) are dead at its end, and the planer's early-free
  ## returns their SIMD registers before the next slot's declarations — only
  ## the partial-sum accumulators outlive a slot.
  for k in 0 ..< e.ptrs.len:
    if e.ptrs[k].hasLoad:
      e.ptrs[k].loadName = e.freshName("l")
      # (var :l (f 128) (instr fldrq p byteOff))
      e.b.addVarDeclHead e.ptrs[k].loadName
      e.b.addF(128)
      e.b.openTag tid(InstrTagId)
      e.b.addSymUse e.iFldrq
      e.b.addSymUse e.ptrs[k].name
      e.b.addIntLit int64(byteOff)
      e.b.closeTag()
      e.b.closeTag()                          # var
  e.valueVec.clear()
  for vi in 0 ..< e.plan.values.len:
    var fresh = false
    let nm = vecEval(e, e.plan.values[vi].rhsCur, fresh)
    e.valueVec[e.plan.values[vi].sym] = nm
  # the accumulator updates: each slot owns its own partial-sum local, so the
  # unrolled slots carry independent dependency chains
  let slot = byteOff div 16
  for ri in 0 ..< e.plan.reductions.len:
    let t = e.plan.reductions[ri].treeCur
    if isMulTree(t):
      # acc += a*b, fused: (asgn acc (instr vfmla acc a b bits))
      var mc = sub(t)
      skipOptType mc
      let ma = mc
      skip mc
      let mb = mc
      var fA, fB = false
      let aN = vecEval(e, ma, fA)
      let bN = vecEval(e, mb, fB)
      e.accUpdate(e.accNames[ri][slot], e.iVfmla, [aN, bN])
    else:
      var fT = false
      let tN = vecEval(e, t, fT)
      e.accUpdate(e.accNames[ri][slot], e.iVfadd, [tN])
  if e.plan.hasStore:
    var srcFresh = false
    let srcName = vecEval(e, e.plan.storeSrc, srcFresh)
    e.b.openTag tid(InstrTagId)
    e.b.addSymUse e.iFstrq
    e.b.addSymUse e.ptrs[e.storePtr].name
    e.b.addIntLit int64(byteOff)
    e.b.addSymUse srcName
    e.b.closeTag()

proc emitVecLoop(e: var Emitter; unrollCount: int) =
  ## `while iv + unroll*VF - 1 < n: <slots>; bump pointers; iv += unroll*VF`
  let step = unrollCount * e.vf
  e.b.openTag tid(WhileTagId)
  e.b.addRunCond(e.plan, step)
  e.b.openTag tid(StmtsTagId)
  for u in 0 ..< unrollCount:
    emitSlot(e, u * 16)
  for k in 0 ..< e.ptrs.len:
    e.b.openTag tid(AsgnTagId)
    e.b.addSymUse e.ptrs[k].name
    e.b.openTag tid(AddrTagId)
    e.b.openTag tid(PatTagId)
    e.b.addSymUse e.ptrs[k].name
    e.b.addIntLit int64(step)
    e.b.closeTag()
    e.b.closeTag()
    e.b.closeTag()
  e.b.openTag tid(AsgnTagId)
  e.b.addSymUse e.plan.ivSym
  e.b.openTag tid(AddTagId)
  e.b.addI64()
  e.b.addIvUse e.plan
  e.b.addIntLit int64(step)
  e.b.closeTag()
  e.b.closeTag()
  e.b.closeTag()                              # stmts (vector body)
  e.b.closeTag()                              # while

proc emitReplacement(c: var Context; plan0: LoopPlan; loopCur: Cursor): TokenBuf =
  ## Build the whole replacement `(stmts …)` tree.
  var plan = plan0

  # ── a single-use value local that IS a reduction's whole tree is inlined ──
  # (the hexer materializes `s += a[i]*b[i]` as `e = a*b; s += e`; inlining
  # exposes the mul to the fmla fusion and keeps the dead `e` out of the slot)
  for ri in 0 ..< plan.reductions.len:
    let t = plan.reductions[ri].treeCur
    if t.kind == Symbol:
      let vs = symId(t)
      var vi = -1
      for i in 0 ..< plan.values.len:
        if plan.values[i].sym == vs: vi = i
      if vi >= 0:
        var uses = 0
        for v in plan.values: countSymUses(v.rhsCur, vs, uses)
        if plan.hasStore: countSymUses(plan.storeSrc, vs, uses)
        for r in plan.reductions: countSymUses(r.treeCur, vs, uses)
        if uses == 1:
          plan.reductions[ri].treeCur = plan.values[vi].rhsCur
          for j in vi ..< plan.values.len - 1: plan.values[j] = plan.values[j+1]
          plan.values.setLen plan.values.len - 1

  var e = Emitter(plan: plan, vf: 128 div plan.elemBits, bits: plan.elemBits,
                  tempCounter: c.tempCounter, moduleSuffix: c.moduleSuffix,
                  iFldrq: "fldrq." & c.vecSuffix, iFstrq: "fstrq." & c.vecSuffix,
                  iVfadd: "vfadd." & c.vecSuffix, iVfsub: "vfsub." & c.vecSuffix,
                  iVfmul: "vfmul." & c.vecSuffix, iVfmla: "vfmla." & c.vecSuffix,
                  iVdup: "vdup." & c.vecSuffix, iVaddv: "vaddv." & c.vecSuffix,
                  storePtr: -1,
                  ptrOfAccess: initTable[SymId, int](),
                  bcSyms: initTable[SymId, string](),
                  bcLits: initTable[int64, string](),
                  valueSet: initHashSet[SymId](),
                  valueVec: initTable[SymId, string]())
  e.b = createTokenBuf(256, c.orig[].pool, c.orig[].tags)

  # ── unique access points; map every access (loads + store) onto one ──
  for acc in plan.accesses:
    let k = uniquePtr(e, acc)
    e.ptrs[k].hasLoad = true
    e.ptrOfAccess[acc.ptrSym] = k
  if plan.hasStore: e.storePtr = uniquePtr(e, plan.store)

  # deref counts, for the fmla in-place fusion rule
  for v in plan.values: countDerefs(e, v.rhsCur)
  if plan.hasStore: countDerefs(e, plan.storeSrc)
  for r in plan.reductions: countDerefs(e, r.treeCur)

  let okName = e.freshName("ok")

  e.b.openTag tid(StmtsTagId)
  let info = rawLineInfo(loopCur)
  if info.isValid: e.b.appendLineInfo info

  # ── vecok = false ──
  e.b.addVarDeclHead okName
  e.b.addBoolType()
  e.b.addBoolLit(false)
  e.b.closeTag()                              # var

  # ── the versioning block ──
  # Everything inside lives in a `(scope …)`: the lo/hi/worst-case index
  # temps are one-shot, and without the scope their register homes stay
  # claimed for the whole proc — enough to run the allocator dry in a
  # six-deep loop nest (mmTiled64).
  e.b.openTag tid(IfTagId)
  e.b.openTag tid(ElifTagId)
  e.b.addRunCond(plan, e.vf)
  e.b.openTag tid(StmtsTagId)
  e.b.openTag tid(ScopeTagId)

  # runtime disjointness of every loading access point vs the store's — a
  # pure reduction has no store, so nothing to check (its accumulators live
  # in registers; `accSafeLocal` proved memory cannot observe them)
  var djNames: seq[string] = @[]
  for k in 0 ..< e.ptrs.len:
    if e.storePtr >= 0 and k != e.storePtr and e.ptrs[k].hasLoad:
      # dj = &A[hi] <= &S[lo];  if not dj: dj = &S[hi] <= &A[lo] — the ranges
      # are [inv+iv, inv+n); every address is an inlined expression.
      let dj = e.freshName("dj")
      djNames.add dj
      let kIdx = e.ptrs[k].idx
      let sIdx = e.ptrs[e.storePtr].idx
      let kBase = e.ptrs[k].baseCur
      let sBase = e.ptrs[e.storePtr].baseCur
      e.b.addVarDeclHead dj
      e.b.addBoolType()
      e.b.addCmpAddrs(plan, kBase, kIdx, tailBound, sBase, sIdx, tailIv)
      e.b.closeTag()                          # var
      e.b.openTag tid(IfTagId)
      e.b.openTag tid(ElifTagId)
      e.b.openTag tid(NotTagId)
      e.b.addSymUse dj
      e.b.closeTag()
      e.b.openTag tid(StmtsTagId)
      e.b.openTag tid(AsgnTagId)
      e.b.addSymUse dj
      e.b.addCmpAddrs(plan, sBase, sIdx, tailBound, kBase, kIdx, tailIv)
      e.b.closeTag()                          # asgn
      e.b.closeTag()                          # stmts
      e.b.closeTag()                          # elif
      e.b.closeTag()                          # if

  # the hoisted guards' worst-case conditions, one nested `if` per condition
  type CondSpec = object
    isDj: bool
    kind: GuardKind
    gv: string              # dj bool name (isDj)
    gidx: int               # guard's index slot (not isDj)
    lenCur: Cursor
  var conds: seq[CondSpec] = @[]
  for g in plan.guards:
    conds.add CondSpec(isDj: false, kind: g.kind, gidx: g.idx, lenCur: g.lenCur)
  for dj in djNames:
    conds.add CondSpec(isDj: true, gv: dj)

  # the nested chain: if C1: if C2: … vecok = true. Every worst-case index is
  # an inlined expression: `lt` guards peak at the LAST covered iteration
  # (index = inv + (n-1)), `le 0` guards at the FIRST (index = inv + iv).
  for spec in conds:
    e.b.openTag tid(IfTagId)
    e.b.openTag tid(ElifTagId)
    if spec.isDj:
      e.b.addSymUse spec.gv
    elif spec.kind == gLtLen:
      e.b.openTag tid(LtTagId)
      e.b.addIdxExpr(plan.indexes[spec.gidx], plan, tailBoundMinus1)
      e.b.addSubtree spec.lenCur
      e.b.closeTag()
    else:
      e.b.openTag tid(LeTagId)
      e.b.addIntLit 0
      e.b.addIdxExpr(plan.indexes[spec.gidx], plan, tailIv)
      e.b.closeTag()
    e.b.openTag tid(StmtsTagId)
  e.b.openTag tid(AsgnTagId)
  e.b.addSymUse okName
  e.b.addBoolLit(true)
  e.b.closeTag()
  for spec in conds:
    e.b.closeTag()                            # stmts
    e.b.closeTag()                            # elif
    e.b.closeTag()                            # if

  e.b.closeTag()                              # scope
  e.b.closeTag()                              # stmts (versioning block)
  e.b.closeTag()                              # elif
  e.b.closeTag()                              # if (runnable check)

  # ── if vecok: preamble + vector loop ──
  e.b.openTag tid(IfTagId)
  e.b.openTag tid(ElifTagId)
  e.b.addSymUse okName
  e.b.openTag tid(StmtsTagId)
  e.b.openTag tid(ScopeTagId)

  # running pointers, their entry index inlined
  for k in 0 ..< e.ptrs.len:
    e.b.addVarDeclHead e.ptrs[k].name
    e.b.openTag tid(PtrTagId)
    e.b.addF(e.bits)
    e.b.closeTag()
    e.b.addHaddrPatE(e.ptrs[k].baseCur, e.ptrs[k].idx, plan, tailIv)
    e.b.closeTag()                            # var

  # broadcasts for the invariant scalar / literal leaves
  for v in plan.values: e.valueSet.incl v.sym
  for v in plan.values: collectBroadcasts(e, v.rhsCur)
  if plan.hasStore: collectBroadcasts(e, plan.storeSrc)
  for r in plan.reductions: collectBroadcasts(e, r.treeCur)

  # partial-sum names up front — the dry-run slot below references slot 0's
  for ri in 0 ..< plan.reductions.len:
    e.accNames.add [e.freshName("acc"), e.freshName("acc")]

  # ── unroll decision, from EXACT per-slot register pressure ──
  # Dry-run one slot into a throwaway buffer: every `(f 128)` local a slot
  # mints (loads and op temps alike) goes through `freshName`, so the counter
  # delta IS the slot's vector-local count — no duplicate of the fusion logic
  # to keep in sync. Vector locals home in the callee-saved SIMD pool
  # (v8–v15, 8 wide); unroll only when two slots plus the broadcasts leave
  # headroom for the proc's scalar float locals, since a vec local that fails
  # to get a register home is a loud compile error by design.
  var perSlot = 0
  block:
    var scratch = createTokenBuf(64, c.orig[].pool, c.orig[].tags)
    swap(e.b, scratch)
    let before = e.tempCounter
    emitSlot(e, 0)
    perSlot = e.tempCounter - before
    swap(e.b, scratch)                        # scratch (and its decls) discarded

  # a partial-sum local is loop-live like a broadcast, one per slot in use
  e.unrolled = 2 * perSlot + e.bcSyms.len + e.bcLits.len +
               2 * plan.reductions.len <= 6
  for ri in 0 ..< plan.reductions.len:
    e.addAccDecl(e.accNames[ri][0])
    if e.unrolled: e.addAccDecl(e.accNames[ri][1])
  if e.unrolled:
    emitVecLoop(e, 2)
  # the single-width loop: the main loop when not unrolled, the ≤1-iteration
  # mid-remainder when unrolled
  emitVecLoop(e, 1)

  # fold each reduction's lanes into its scalar, ONCE:
  # `(asgn S (add (f W) S (instr vaddv acc bits)))`
  for ri in 0 ..< plan.reductions.len:
    if e.unrolled:
      e.accUpdate(e.accNames[ri][0], e.iVfadd, [e.accNames[ri][1]])
    e.b.openTag tid(AsgnTagId)
    e.b.addSymUse plan.reductions[ri].sym
    e.b.openTag tid(AddTagId)
    e.b.addF(e.bits)
    e.b.addSymUse plan.reductions[ri].sym
    e.b.openTag tid(InstrTagId)
    e.b.addSymUse e.iVaddv
    e.b.addSymUse e.accNames[ri][0]
    e.b.addIntLit int64(e.bits)
    e.b.closeTag()                            # instr
    e.b.closeTag()                            # add
    e.b.closeTag()                            # asgn

  e.b.closeTag()                              # scope
  e.b.closeTag()                              # stmts (vecok branch)
  e.b.closeTag()                              # elif
  e.b.closeTag()                              # if vecok

  # the ORIGINAL loop: remainder + fallback, untouched
  e.b.addSubtree loopCur

  e.b.closeTag()                              # the replacement (stmts …)
  c.tempCounter = e.tempCounter
  result = createTokenBuf(0, c.orig[].pool, c.orig[].tags)
  swap(result, e.b)

# ── main traversal ──────────────────────────────────────────────────────────

proc containsSub(n: Cursor): bool =
  ## Any `sub` anywhere in the loop. Checked BEFORE planning rather than inside
  ## the lane-tree predicates: a refusal has to leave the original loop exactly
  ## as it was, and the cheapest place to be sure of that is before a plan
  ## exists. Coarse on purpose — an index computation's `sub` also blocks the
  ## loop — which only costs a vectorization on the target that cannot lower it.
  if n.kind != TagLit: return false
  if n.exprKind == SubC: return true
  var it = n
  it.loopInto:
    if containsSub(it): return true
    skip it
  result = false

proc tr(c: var Context; n: var Cursor) =
  if n.hasMore:
    if n.kind == TagLit:
      var matched = false
      if n.stmtKind == WhileS:
        var plan = LoopPlan()
        if (c.allowSub or not containsSub(n)) and matchLoop(c, n, plan):
          let pos = cursorToPosition(c.orig[], n)
          var nb = emitReplacement(c, plan, n)
          let idx = c.synth.len
          c.synth.add ensureMove(nb)
          c.patchset.addSubst(pos, cursorAt(c.synth[idx], 0))
          c.vectorized = true
          skip n                              # matched: body has no inner loops
          matched = true
      if not matched:
        n.loopInto:
          tr(c, n)
    else:
      inc n

# ── public entry ────────────────────────────────────────────────────────────

const VecIntrinsics* = [
  ("fldrq", 2), ("fstrq", 3), ("vfadd", 3), ("vfsub", 3), ("vfmul", 3),
  ("vfmla", 4), ("vdup", 2), ("vaddv", 2)]

proc addVecIntrinsicDecls*(dest: var TokenBuf; vecSuffix: string) =
  ## The `{.instruction.}` declarations the emitted `(instr …)` applications
  ## name: declaration-only (body `.`), appended once per module that
  ## vectorized anything. The names are `<tag>.<vecSuffix>`; the pragma string
  ## is the tag, which is what actually selects the row.
  for (tag, arity) in VecIntrinsics:
    dest.openTag tid(ProcTagId)
    dest.addSymDef tag & "." & vecSuffix
    dest.openTag tid(ParamsTagId)
    for i in 0 ..< arity:
      dest.openTag tid(ParamTagId)
      dest.addSymDef "a" & $i & "." & tag & "." & vecSuffix
      dest.addDotToken()
      # parameter types: ptr for the address, (i 64) for offsets/lane-bits,
      # (f 128) for vector values, (f 64) for `vdup`'s scalar
      if (tag == "fldrq" or tag == "fstrq") and i == 0:
        dest.openTag tid(PtrTagId)
        dest.openTag tid(FTagId)
        dest.addIntLit 64
        dest.closeTag()
        dest.closeTag()
      elif tag == "vdup" and i == 0:
        dest.openTag tid(FTagId)
        dest.addIntLit 64
        dest.closeTag()
      elif (tag == "fstrq" and i == 2):
        dest.openTag tid(FTagId)
        dest.addIntLit 128
        dest.closeTag()
      elif (tag in ["vfadd", "vfsub", "vfmul"] and i < 2) or
           (tag == "vfmla" and i < 3) or (tag == "vaddv" and i == 0):
        dest.openTag tid(FTagId)
        dest.addIntLit 128
        dest.closeTag()
      else:
        dest.openTag tid(ITagId)
        dest.addIntLit 64
        dest.closeTag()
      dest.closeTag()                         # param
    dest.closeTag()                           # params
    if tag == "fstrq":
      dest.addDotToken()                      # void result
    else:
      dest.openTag tid(FTagId)
      # `vaddv` is the one row that produces a SCALAR (the lane sum)
      dest.addIntLit (if tag == "vaddv": 64 else: 128)
      dest.closeTag()
    dest.openTag tid(PragmasTagId)
    dest.openTag tid(InstructionTagId)
    dest.addStrLit tag
    dest.closeTag()
    dest.closeTag()                           # pragmas
    dest.addDotToken()                        # no body
    dest.closeTag()                           # proc

proc runVectorizer*(buf: var TokenBuf; moduleSuffix, vecSuffix: string;
                    allowSub = true): bool =
  ## Vectorize every matching innermost loop of one proc body. Returns true
  ## when something was vectorized — the caller then appends the intrinsic
  ## declarations once per module via `addVecIntrinsicDecls`.
  var ctx = createContext(addr buf, moduleSuffix, vecSuffix, allowSub)
  var n = beginRead(buf)
  tr(ctx, n)
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)
  result = ctx.vectorized

# ── self-tests ──────────────────────────────────────────────────────────────

when isMainModule:
  import std / strutils

  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  block simple_axpy:
    # a[i] = a[i] + s * b[i] — the mmIkj inner-loop shape, distilled
    var buf = parse("""
(stmts
 (while (lt i.0 n.0)
  (stmts
   (var :p.0 . (ptr (f 64)) .)
   (var :ix.0 . (i 64) (add (i 64) k.0 i.0))
   (if (elif (not (lt ix.0 (dot b.0 len.0 0))) (stmts (call panic.0))))
   (if (elif (not (le 0 ix.0)) (stmts (call panic.0))))
   (asgn p.0 (haddr (pat (dot b.0 data.0 0) ix.0)))
   (var :q.0 . (ptr (f 64)) .)
   (var :ix.1 . (i 64) (add (i 64) j.0 i.0))
   (asgn q.0 (haddr (pat (dot (deref a.0) data.0 0) ix.1)))
   (var :e.0 . (f 64) (add (f 64) (deref q.0) (mul (f 64) s.0 (deref p.0))))
   (var :ix.2 . (i 64) (add (i 64) j.0 i.0))
   (asgn (pat (dot (deref a.0) data.0 0) ix.2) e.0)
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert runVectorizer(buf, "M", "vec.M")
    let s = toString(buf)
    doAssert s.contains("(instr fldrq.vec.M"), s
    doAssert s.contains("(instr vfmla.vec.M") or s.contains("vfmla.vec.M"), s
    doAssert s.contains("(instr fstrq.vec.M"), s
    doAssert s.contains("(instr vdup.vec.M"), s
    # 2 vec locals per slot + 1 broadcast fits the pool → 2x-unrolled main
    # loop + single-width mid-remainder + the scalar original
    doAssert s.count("(while") == 3,
      "unrolled loop + single-width loop + scalar remainder expected: " & s
    # unrolled: 2 slots x 2 loads + mid-loop 2 loads = 6; slot 1 loads at +16
    doAssert s.count("(instr fldrq.vec.M") == 6, s
    doAssert s.count("(instr fstrq.vec.M") == 3, s
    # the disjointness check exists (b vs a ranges)
    doAssert s.contains("vec.dj."), s

  block store_of_literal:
    # the zeroing loop: c[i] = 0.0
    var buf = parse("""
(stmts
 (while (lt i.0 n.0)
  (stmts
   (var :ix.0 . (i 64) i.0)
   (if (elif (not (lt ix.0 (dot (deref c.0) len.0 0))) (stmts (call panic.0))))
   (asgn (pat (dot (deref c.0) data.0 0) ix.0) 0.0)
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert runVectorizer(buf, "M", "vec.M")
    let s = toString(buf)
    doAssert s.contains("(instr vdup.vec.M"), s
    doAssert s.contains("(instr fstrq.vec.M"), s
    doAssert not s.contains("fldrq"), "no loads expected: " & s
    # zero per-slot locals → unrolls: 2 stores in the main loop + 1 in the mid
    doAssert s.count("(instr fstrq.vec.M") == 3, s
    doAssert s.count("(while") == 3, s

  block sum_reduction:
    # s = s + b[i] — a loop-carried scalar accumulator, no store
    var buf = parse("""
(stmts
 (var :s.0 . (f 64) 0.0)
 (while (lt i.0 n.0)
  (stmts
   (var :p.0 . (ptr (f 64)) .)
   (var :ix.0 . (i 64) i.0)
   (if (elif (not (lt ix.0 (dot b.0 len.0 0))) (stmts (call panic.0))))
   (asgn p.0 (haddr (pat (dot b.0 data.0 0) ix.0)))
   (asgn s.0 (add (f 64) s.0 (deref p.0)))
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert runVectorizer(buf, "M", "vec.M")
    let s = toString(buf)
    doAssert s.contains("(instr vaddv.vec.M"), s
    doAssert s.contains("(instr vdup.vec.M"), "zero-lane start expected: " & s
    doAssert not s.contains("fstrq"), "no store expected: " & s
    doAssert not s.contains("vec.dj."), "no disjointness check expected: " & s
    # 1 load per slot + 2 partial sums fits the pool → unrolled main loop +
    # single-width mid-remainder + the scalar original
    doAssert s.count("(while") == 3, s
    doAssert s.count("(instr fldrq.vec.M") == 3, s
    # the lanes fold into the scalar exactly ONCE
    doAssert s.count("(instr vaddv.vec.M") == 1, s

  block dot_reduction:
    # s += a[i]*b[i], the hexer shape: e = a*b; s = s + e — the single-use
    # value local must inline into the reduction and fuse to vfmla
    var buf = parse("""
(stmts
 (var :s.0 . (f 64) 0.0)
 (while (lt i.0 n.0)
  (stmts
   (var :p.0 . (ptr (f 64)) .)
   (var :ix.0 . (i 64) i.0)
   (asgn p.0 (haddr (pat (dot a.0 data.0 0) ix.0)))
   (var :q.0 . (ptr (f 64)) .)
   (var :ix.1 . (i 64) i.0)
   (asgn q.0 (haddr (pat (dot b.0 data.0 0) ix.1)))
   (var :e.0 . (f 64) (mul (f 64) (deref p.0) (deref q.0)))
   (asgn s.0 (add (f 64) s.0 e.0))
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert runVectorizer(buf, "M", "vec.M")
    let s = toString(buf)
    doAssert s.contains("(instr vfmla.vec.M"), "fused multiply-add expected: " & s
    doAssert not s.contains("(instr vfmul.vec.M"),
      "the mul must fold into the fmla: " & s
    doAssert s.count("(instr vaddv.vec.M") == 1, s
    # 2 loads per slot + 2 partial sums → still unrolls
    doAssert s.count("(while") == 3, s
    doAssert s.count("(instr fldrq.vec.M") == 6, s
    doAssert s.count("(instr vfmla.vec.M") == 3, s

  block reject_undeclared_acc:
    # s = s + b[i] with NO `(var :s.0 …)` in the body: a param or global —
    # not provably register-private, must NOT vectorize
    var buf = parse("""
(stmts
 (while (lt i.0 n.0)
  (stmts
   (var :p.0 . (ptr (f 64)) .)
   (var :ix.0 . (i 64) i.0)
   (asgn p.0 (haddr (pat (dot b.0 data.0 0) ix.0)))
   (asgn s.0 (add (f 64) s.0 (deref p.0)))
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert not runVectorizer(buf, "M", "vec.M")

  block reject_addr_taken_acc:
    # the accumulator's address escapes — memory could observe it mid-loop
    var buf = parse("""
(stmts
 (var :s.0 . (f 64) 0.0)
 (call sink.0 (haddr s.0))
 (while (lt i.0 n.0)
  (stmts
   (var :p.0 . (ptr (f 64)) .)
   (var :ix.0 . (i 64) i.0)
   (asgn p.0 (haddr (pat (dot b.0 data.0 0) ix.0)))
   (asgn s.0 (add (f 64) s.0 (deref p.0)))
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert not runVectorizer(buf, "M", "vec.M")

  block reject_product_reduction:
    # s = s * b[i] — not a sum; no lane-fold instruction carries it
    var buf = parse("""
(stmts
 (var :s.0 . (f 64) 1.0)
 (while (lt i.0 n.0)
  (stmts
   (var :p.0 . (ptr (f 64)) .)
   (var :ix.0 . (i 64) i.0)
   (asgn p.0 (haddr (pat (dot b.0 data.0 0) ix.0)))
   (asgn s.0 (mul (f 64) s.0 (deref p.0)))
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert not runVectorizer(buf, "M", "vec.M")

  block reject_call:
    var buf = parse("""
(stmts
 (while (lt i.0 n.0)
  (stmts
   (call f.0 i.0)
   (asgn i.0 (add (i 64) i.0 1)))))""")
    doAssert not runVectorizer(buf, "M", "vec.M")

  block reject_nonunit_iv:
    var buf = parse("""
(stmts
 (while (lt i.0 n.0)
  (stmts
   (var :ix.0 . (i 64) i.0)
   (asgn (pat (dot b.0 data.0 0) ix.0) 0.0)
   (asgn i.0 (add (i 64) i.0 2)))))""")
    doAssert not runVectorizer(buf, "M", "vec.M")

  echo "vectorizer.nim: all self-tests passed"

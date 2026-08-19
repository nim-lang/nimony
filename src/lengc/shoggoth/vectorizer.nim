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
## * exactly ONE store `(asgn (pat BASE I) src)`;
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

  LoopPlan = object
    ivSym: SymId
    boundCur: Cursor        ## `n` — invariant symbol or int literal
    elemBits: int           ## 32 or 64; uniform across every access
    indexes: seq[VecIndex]
    accesses: seq[VecAccess]  ## the loads
    store: VecAccess
    storeSrc: Cursor
    guards: seq[VecGuard]
    values: seq[VecValue]

  Context = object
    orig: ptr TokenBuf
    patchset: Patchset
    synth: seq[TokenBuf]
    tempCounter: int
    moduleSuffix: string
    vecSuffix: string       ## suffix of the module-level intrinsic decls
    vectorized*: bool

proc createContext(orig: ptr TokenBuf; moduleSuffix, vecSuffix: string): Context =
  Context(orig: orig, patchset: initPatchset(orig), synth: @[],
          tempCounter: 0, moduleSuffix: moduleSuffix, vecSuffix: vecSuffix)

# ── small tree utilities ────────────────────────────────────────────────────

proc sameTree(a, b: Cursor): bool =
  ## Structural equality of two subtrees, ignoring line info. Recursive rather
  ## than a flat token walk: `inc` advances by a token's full width (line-info
  ## suffixes included) while `subtreeWidth` counts slots, so a flat walk
  ## overruns on real, line-info-carrying files.
  if a.kind != b.kind: return false
  case a.kind
  of TagLit:
    if a.cursorTagId != b.cursorTagId: return false
    var x = sub(a)
    var y = sub(b)
    while x.hasMore and y.hasMore:
      if not sameTree(x, y): return false
      skip x
      skip y
    result = x.hasMore == y.hasMore
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
  if not n.hasMore or n.kind != TagLit: return
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
  if n.kind == FloatLit: return 64
  let (ok, bits, _) = sufFloatLit(n)
  if ok: bits else: 0

proc matchWhileHead(loop: Cursor; ivSym: var SymId; bound: var Cursor): bool =
  ## `(while (lt [T] iv n) body)` — `n` a symbol or int literal.
  var c = sub(loop)
  if not c.hasMore or c.kind != TagLit or c.exprKind != LtC: return false
  var e = sub(c)
  skipOptType e
  if e.kind != Symbol: return false
  ivSym = symId(e)
  skip e
  if not e.hasMore or e.kind notin {Symbol, IntLit}: return false
  bound = e
  skip e
  result = not e.hasMore

proc matchIvInc(n: Cursor; ivSym: SymId): bool =
  ## `(asgn iv (add T iv 1))`, the `1` possibly `(conv T 1)`-wrapped.
  if n.stmtKind != AsgnS: return false
  var c = sub(n)
  if c.kind != Symbol or symId(c) != ivSym: return false
  skip c
  if not c.hasMore or c.kind != TagLit or c.exprKind != AddC: return false
  var e = sub(c)
  skipOptType e
  if e.kind != Symbol or symId(e) != ivSym: return false
  skip e
  if not e.hasMore: return false
  if e.kind == TagLit and e.exprKind in {ConvC, CastC}:
    var one = sub(e)
    skip one                                  # the target type
    if not one.hasMore or one.kind != IntLit or intVal(one) != 1: return false
    skip one
    if one.hasMore: return false
  elif e.kind != IntLit or intVal(e) != 1:
    return false
  skip e
  result = not e.hasMore

proc matchIndexExpr(e0: Cursor; ivSym: SymId; assigned: HashSet[SymId];
                    ix: var VecIndex): bool =
  ## `iv` | `(add T inv iv)` | `(add T iv inv)` — inv an invariant symbol or
  ## an int literal.
  var e = e0
  if e.kind == Symbol and symId(e) == ivSym:
    ix.invSym = SymId(0)
    ix.invLit = 0
    return true
  if e.kind != TagLit or e.exprKind != AddC: return false
  var c = sub(e)
  skipOptType c
  var sawIv = false
  var sawInv = false
  for which in 0 ..< 2:
    if not c.hasMore: return false
    if c.kind == Symbol and symId(c) == ivSym and not sawIv:
      sawIv = true
    elif c.kind == Symbol and symId(c) notin assigned and not sawInv:
      ix.invSym = symId(c)
      sawInv = true
    elif c.kind == IntLit and not sawInv:
      ix.invSym = SymId(0)
      ix.invLit = intVal(c)
      sawInv = true
    else:
      return false
    skip c
  result = sawIv and sawInv and not c.hasMore

proc invariantBase(n: Cursor; assigned: HashSet[SymId]): bool =
  ## An lvalue path whose every mentioned symbol is loop-invariant and whose
  ## shape is symbols, field selections and derefs only (no calls, no indexing).
  case n.kind
  of Symbol:
    result = symId(n) notin assigned
  of TagLit:
    if n.exprKind notin {DotC, DerefC}: return false
    var c = sub(n)
    result = true
    while c.hasMore:
      if not invariantBase(c, assigned): return false
      skip c
  of IntLit, UIntLit:
    result = true                             # a `dot`'s inheritance depth
  else:
    result = false

proc matchPatLval(n: Cursor; plan: LoopPlan; assigned: HashSet[SymId];
                  acc: var VecAccess): bool =
  ## `(pat BASE I)` with invariant BASE and known index local I.
  if n.kind != TagLit or n.exprKind != PatC: return false
  var pc = sub(n)
  if not pc.hasMore: return false
  acc.baseCur = pc
  if not invariantBase(pc, assigned): return false
  skip pc
  if not pc.hasMore or pc.kind != Symbol: return false
  acc.idx = -1
  for i in 0 ..< plan.indexes.len:
    if plan.indexes[i].sym == symId(pc): acc.idx = i
  skip pc
  result = acc.idx >= 0 and not pc.hasMore

proc matchGuard(n: Cursor; plan: LoopPlan; assigned: HashSet[SymId];
                g: var VecGuard): bool =
  ## `(if (elif (not COND) (stmts (call sym …))))` where COND is
  ## `(lt I len)` or `(le 0 I)` over a known index local. The action's contents
  ## are irrelevant: the vector path only runs when the condition is FALSE for
  ## every iteration it covers, so the call can never fire there.
  template grej(msg: string) =
    when defined(vecDbg):
      echo "  guard reject: ", msg
    return false
  if n.stmtKind != IfS: grej "not if"
  var c = sub(n)
  if not c.hasMore or c.kind != TagLit or c.substructureKind != ElifU:
    grej "no elif"
  var e = sub(c)
  skip c
  if c.hasMore: grej "extra arm"              # a single `elif`, no `else`
  if not e.hasMore or e.kind != TagLit or e.exprKind != NotC: grej "cond not (not ...)"
  block cond:
    var nc = sub(e)
    if not nc.hasMore or nc.kind != TagLit or nc.exprKind notin {LtC, LeC}:
      grej "not lt/le"
    let isLt = nc.exprKind == LtC
    var cc = sub(nc)
    skipOptType cc
    if isLt:
      if not cc.hasMore or cc.kind != Symbol: grej "lt lhs not a symbol"
      let isym = symId(cc)
      skip cc
      if not cc.hasMore: grej "lt missing rhs"
      g = VecGuard(kind: gLtLen, idx: -1, lenCur: cc)
      if cc.kind == Symbol:
        if symId(cc) in assigned: grej "lt len assigned"
      elif not invariantBase(cc, assigned):
        when defined(vecDbg):
          var dbgBuf = createTokenBuf(16, cc.pool, cc.tags)
          dbgBuf.addSubtree cc
          echo "  guard reject: lt len not invariant path: ", toString(dbgBuf)
        return false
      skip cc
      if cc.hasMore: grej "lt extra operand"
      for i in 0 ..< plan.indexes.len:
        if plan.indexes[i].sym == isym: g.idx = i
    else:
      if not cc.hasMore or cc.kind != IntLit or intVal(cc) != 0: grej "le lhs not 0"
      skip cc
      if not cc.hasMore or cc.kind != Symbol: grej "le rhs not symbol"
      let isym = symId(cc)
      skip cc
      if cc.hasMore: grej "le extra operand"
      g = VecGuard(kind: gLeZero, idx: -1)
      for i in 0 ..< plan.indexes.len:
        if plan.indexes[i].sym == isym: g.idx = i
    if g.idx < 0: grej "unknown index local"
    skip nc
    if nc.hasMore: grej "not-arg trailing"
  skip e                                      # past the `(not …)` condition
  if not e.hasMore or e.kind != TagLit or e.stmtKind != StmtsS: grej "action not stmts"
  var act = sub(e)
  var nCalls = 0
  while act.hasMore:
    if act.kind == TagLit and act.stmtKind == CallS: inc nCalls
    else: grej "action stmt kind " & $act.stmtKind
    skip act
  skip e
  when defined(vecDbg):
    if nCalls != 1 or e.hasMore: echo "  guard reject: tail calls=", nCalls
  result = nCalls == 1 and not e.hasMore

proc pureLaneTree(n: Cursor; assigned: HashSet[SymId];
                  valueSyms: HashSet[SymId]; ptrSyms: HashSet[SymId]): bool =
  ## leaves: `(deref P)` (P a bound pointer temp), invariant scalar symbols,
  ## value locals, float literals; interior: binary `add`/`sub`/`mul`.
  case n.kind
  of FloatLit: result = true
  of Symbol:
    result = symId(n) in valueSyms or symId(n) notin assigned
  of TagLit:
    if sufFloatLit(n).ok: return true
    case n.exprKind
    of DerefC:
      var c = sub(n)
      result = c.hasMore and c.kind == Symbol and symId(c) in ptrSyms
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
        if not pureLaneTree(c, assigned, valueSyms, ptrSyms): result = false
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
    return
  if n.kind == TagLit and n.exprKind in {AddC, SubC, MulC}:
    var cc = sub(n)
    skipOptType cc
    while cc.hasMore:
      scanLitBits(cc, bits, conflict)
      skip cc

proc matchLoop(c: var Context; loop: Cursor; plan: var LoopPlan): bool =
  ## Match the whole canonical loop; fill `plan`. Any unexpected statement
  ## rejects — the pass must understand EVERYTHING it vectorizes.
  when defined(vecDbg): echo "vectorizer: trying a while loop"
  var bound = default(Cursor)
  if not matchWhileHead(loop, plan.ivSym, bound):
    when defined(vecDbg):
      var wc = sub(loop)
      if wc.hasMore:
        var db = createTokenBuf(8, wc.pool, wc.tags)
        db.addSubtree wc
        echo "vectorizer reject: while head: ", toString(db)
      else:
        echo "vectorizer reject: while head (empty)"
    return false
  plan.boundCur = bound
  var body = loop
  inc body                                    # past `(while`
  skip body                                   # past the condition

  var assigned = initHashSet[SymId]()
  collectAssigned(body, assigned)
  assigned.incl plan.ivSym
  if bound.kind == Symbol and symId(bound) in assigned:
    when defined(vecDbg): echo "vectorizer reject: bound assigned in loop"
    return false

  var stmts: seq[Cursor] = @[]
  flattenStmts(body, stmts)

  # the iv itself is a valid access index (`c[iv]` — the fill loops); its
  # displacement is zero
  plan.indexes.add VecIndex(sym: plan.ivSym, invSym: SymId(0), invLit: 0)

  var pendingPtr = initHashSet[SymId]()       # declared, not yet bound
  var ptrBits = initTable[SymId, int]()
  var valueSyms = initHashSet[SymId]()
  var ptrSyms = initHashSet[SymId]()
  var sawStore = false
  var sawInc = false
  plan.elemBits = 0

  template rej(msg: string) =
    when defined(vecDbg):
      echo "vectorizer reject: ", msg, " at stmt ", si
    return false
  for si in 0 ..< stmts.len:
    let n = stmts[si]
    if n.kind != TagLit: rej "non-tag stmt"
    if sawInc:
      # only inert labels may trail the increment (inliner returnLabels)
      if n.stmtKind == LabS: continue
      rej "stmt after increment"
    case n.stmtKind
    of VarS:
      var d = sub(n)
      if not d.hasMore or d.kind != SymbolDef: rej "var without name"
      let nm = symId(d)
      inc d                                   # past the name
      skip d                                  # pragmas
      if not d.hasMore: return false
      if d.kind == TagLit and d.typeKind == PtrT:
        # `(var :P (ptr (f W)) .)` — a pointer temp, bound later
        var pt = sub(d)
        var bits = 0
        if pt.hasMore and pt.kind == TagLit and pt.typeKind == FT:
          var b = pt
          inc b
          if b.kind == IntLit: bits = int(intVal(b))
        skip d                                # past the type
        if bits notin {32, 64}: rej "ptr temp bits"
        if d.hasMore and d.kind != DotToken: rej "initialized ptr temp"
        pendingPtr.incl nm
        ptrBits[nm] = bits
        ptrSyms.incl nm
      elif d.kind == TagLit and d.typeKind == IT:
        skip d                                # past the type
        if not d.hasMore or d.kind == DotToken: return false
        var ix = VecIndex(sym: nm)
        if not matchIndexExpr(d, plan.ivSym, assigned, ix):
          when defined(vecDbg):
            var db = createTokenBuf(8, d.pool, d.tags)
            db.addSubtree d
            echo "  index-expr reject: ", toString(db)
          rej "index expr"
        plan.indexes.add ix
      elif d.kind == TagLit and d.typeKind == FT:
        var b = d
        inc b
        let bits = if b.kind == IntLit: int(intVal(b)) else: 0
        skip d                                # past the type
        if not d.hasMore or d.kind == DotToken: return false
        if not pureLaneTree(d, assigned, valueSyms, ptrSyms): rej "value rhs"
        if plan.elemBits != 0 and bits != plan.elemBits: rej "mixed elem bits"
        if plan.elemBits == 0: plan.elemBits = bits
        plan.values.add VecValue(sym: nm, rhsCur: d)
        valueSyms.incl nm
      else:
        when defined(vecDbg):
          var db = createTokenBuf(8, n.pool, n.tags)
          db.addSubtree n
          echo "  var reject: ", toString(db)
        rej "var of unhandled type"
    of AsgnS:
      if matchIvInc(n, plan.ivSym):
        sawInc = true
      else:
        var a = sub(n)
        if not a.hasMore: return false
        if a.kind == Symbol and symId(a) in pendingPtr:
          # `(asgn P (haddr (pat BASE I)))` — bind the pointer temp
          let p = symId(a)
          skip a
          if not a.hasMore or a.kind != TagLit or a.exprKind != HaddrC:
            return false
          var h = sub(a)
          if not h.hasMore: return false
          var acc = VecAccess(ptrSym: p)
          if not matchPatLval(h, plan, assigned, acc): return false
          skip h
          if h.hasMore: return false
          let bits = ptrBits[p]
          if plan.elemBits != 0 and bits != plan.elemBits: return false
          if plan.elemBits == 0: plan.elemBits = bits
          plan.accesses.add acc
          pendingPtr.excl p
        elif a.kind == TagLit and a.exprKind == PatC:
          # the ONE store
          if sawStore: return false
          var acc = VecAccess(ptrSym: SymId(0))
          if not matchPatLval(a, plan, assigned, acc): return false
          skip a
          if not a.hasMore: return false
          if not pureLaneTree(a, assigned, valueSyms, ptrSyms): return false
          plan.store = acc
          plan.storeSrc = a
          skip a
          if a.hasMore: return false
          sawStore = true
        else:
          when defined(vecDbg):
            var db = createTokenBuf(8, n.pool, n.tags)
            db.addSubtree n
            echo "  asgn reject: ", toString(db)
          rej "asgn shape"
    of IfS:
      var g = VecGuard(idx: -1)
      if not matchGuard(n, plan, assigned, g): rej "guard shape"
      plan.guards.add g
    of LabS:
      discard          # inliner residue; jumps would appear as statements and reject
    else:
      rej "statement kind " & $n.stmtKind

  # literal leaves imply a width too (the zero-fill loop has ONLY literals);
  # unify it with the access-implied one
  var litConflict = false
  for v in plan.values: scanLitBits(v.rhsCur, plan.elemBits, litConflict)
  if sawStore: scanLitBits(plan.storeSrc, plan.elemBits, litConflict)
  if litConflict:
    when defined(vecDbg): echo "vectorizer reject: conflicting literal widths"
    return false
  result = sawStore and sawInc and plan.elemBits in {32, 64} and
           pendingPtr.len == 0
  when defined(vecDbg):
    if not result:
      echo "vectorizer reject: final (store=", sawStore, " inc=", sawInc,
           " bits=", plan.elemBits, " pending=", pendingPtr.len, ")"

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
    iFldrq, iFstrq, iVfadd, iVfsub, iVfmul, iVfmla, iVdup: string
    ptrs: seq[UniquePtr]    ## unique access points (loads + store)
    ptrOfAccess: Table[SymId, int]  ## load ptr temp → ptrs index
    storePtr: int
    bcSyms: Table[SymId, string]    ## invariant scalar → broadcast local
    bcLits: Table[int64, string]    ## float bit pattern → broadcast local
    valueSet: HashSet[SymId]
    valueVec: Table[SymId, string]  ## value local → its `(f 128)` local

proc freshName(e: var Emitter; kind: string): string =
  inc e.tempCounter
  result = "vec." & kind & "." & $e.tempCounter & "." & e.moduleSuffix

proc uniquePtr(e: var Emitter; acc: VecAccess): int =
  for i in 0 ..< e.ptrs.len:
    if sameTree(e.ptrs[i].baseCur, acc.baseCur) and
       sameIndexInfo(e.ptrs[i].idx, e.plan.indexes[acc.idx]):
      return i
  e.ptrs.add UniquePtr(name: e.freshName("p"), baseCur: acc.baseCur,
                       idx: e.plan.indexes[acc.idx])
  result = e.ptrs.len - 1

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
      return
    if n.exprKind in {AddC, SubC, MulC}:
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
      return e.bcLits[cast[int64](floatVal(sinner))]
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
          fresh = true
          return accN
        let mN = vecBin(e, e.iVfmul, aN, bN)
        fresh = true
        return vecBin(e, e.iVfadd, mN, accN)
      var fL, fR = false
      let lN = vecEval(e, lhs, fL)
      let rN = vecEval(e, rhs, fR)
      fresh = true
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
  ## core overlaps the independent slots on its own.
  for k in 0 ..< e.ptrs.len:
    if not e.ptrs[k].hasLoad: continue
    e.ptrs[k].loadName = e.freshName("l")
    # (var :l (f 128) (instr fldrq p byteOff))
    e.b.addVarDeclHead e.ptrs[k].loadName
    e.b.addF(128)
    e.b.openTag tid(InstrTagId)
    e.b.addSymUse e.iFldrq
    e.b.addSymUse e.ptrs[k].name
    e.b.addIntLit int64(byteOff)
    e.b.closeTag()
    e.b.closeTag()                            # var
  e.valueVec.clear()
  for vi in 0 ..< e.plan.values.len:
    var fresh = false
    let nm = vecEval(e, e.plan.values[vi].rhsCur, fresh)
    e.valueVec[e.plan.values[vi].sym] = nm
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

proc emitReplacement(c: var Context; plan: LoopPlan; loopCur: Cursor): TokenBuf =
  ## Build the whole replacement `(stmts …)` tree.
  var e = Emitter(plan: plan, vf: 128 div plan.elemBits, bits: plan.elemBits,
                  tempCounter: c.tempCounter, moduleSuffix: c.moduleSuffix,
                  iFldrq: "fldrq." & c.vecSuffix, iFstrq: "fstrq." & c.vecSuffix,
                  iVfadd: "vfadd." & c.vecSuffix, iVfsub: "vfsub." & c.vecSuffix,
                  iVfmul: "vfmul." & c.vecSuffix, iVfmla: "vfmla." & c.vecSuffix,
                  iVdup: "vdup." & c.vecSuffix, storePtr: -1,
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
  e.storePtr = uniquePtr(e, plan.store)

  # deref counts, for the fmla in-place fusion rule
  for v in plan.values: countDerefs(e, v.rhsCur)
  countDerefs(e, plan.storeSrc)

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

  # runtime disjointness of every loading access point vs the store's
  var djNames: seq[string] = @[]
  for k in 0 ..< e.ptrs.len:
    if k == e.storePtr or not e.ptrs[k].hasLoad: continue
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
    e.b.closeTag()                            # var
    e.b.openTag tid(IfTagId)
    e.b.openTag tid(ElifTagId)
    e.b.openTag tid(NotTagId)
    e.b.addSymUse dj
    e.b.closeTag()
    e.b.openTag tid(StmtsTagId)
    e.b.openTag tid(AsgnTagId)
    e.b.addSymUse dj
    e.b.addCmpAddrs(plan, sBase, sIdx, tailBound, kBase, kIdx, tailIv)
    e.b.closeTag()                            # asgn
    e.b.closeTag()                            # stmts
    e.b.closeTag()                            # elif
    e.b.closeTag()                            # if

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
  collectBroadcasts(e, plan.storeSrc)

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

  if 2 * perSlot + e.bcSyms.len + e.bcLits.len <= 6:
    emitVecLoop(e, 2)
  # the single-width loop: the main loop when not unrolled, the ≤1-iteration
  # mid-remainder when unrolled
  emitVecLoop(e, 1)

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

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of TagLit:
    if n.stmtKind == WhileS:
      var plan = LoopPlan()
      if matchLoop(c, n, plan):
        let pos = cursorToPosition(c.orig[], n)
        var nb = emitReplacement(c, plan, n)
        let idx = c.synth.len
        c.synth.add ensureMove(nb)
        c.patchset.addSubst(pos, cursorAt(c.synth[idx], 0))
        c.vectorized = true
        skip n                                # matched: body has no inner loops
        return
    n.loopInto:
      tr(c, n)
  else:
    inc n

# ── public entry ────────────────────────────────────────────────────────────

const VecIntrinsics* = [
  ("fldrq", 2), ("fstrq", 3), ("vfadd", 3), ("vfsub", 3), ("vfmul", 3),
  ("vfmla", 4), ("vdup", 2)]

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
           (tag == "vfmla" and i < 3):
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
      dest.addIntLit 128
      dest.closeTag()
    dest.openTag tid(PragmasTagId)
    dest.openTag tid(InstructionTagId)
    dest.addStrLit tag
    dest.closeTag()
    dest.closeTag()                           # pragmas
    dest.addDotToken()                        # no body
    dest.closeTag()                           # proc

proc runVectorizer*(buf: var TokenBuf; moduleSuffix, vecSuffix: string): bool =
  ## Vectorize every matching innermost loop of one proc body. Returns true
  ## when something was vectorized — the caller then appends the intrinsic
  ## declarations once per module via `addVecIntrinsicDecls`.
  var ctx = createContext(addr buf, moduleSuffix, vecSuffix)
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

  block reject_reduction:
    # s = s + a[i] — loop-carried scalar: must NOT vectorize
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

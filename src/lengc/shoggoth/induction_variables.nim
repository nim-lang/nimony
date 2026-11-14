#
#
#       NIFC Induction-Variable Strength Reduction (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## nifcore induction-variable strength reduction. Replaces `(at arr iv)` accesses
## inside a loop with `(deref p)` for a pointer `p` initialized before the loop
## and advanced one element per iteration. See `induction_variables_legacy.nim`
## (the parked nifcursors original) for the full pattern/rewrite description; this
## version runs over **nifcore** cursors and uses [patchsets](patchsets.nim) for
## the structural mutations.

import std / [tables, sets, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind, tag enums
import ".." / ".." / "models" / tags          # *TagId ordinals for synthesis
import patchsets

include ".." / ".." / "lib" / compat2   # getOrQuit (host Nim)
import ".." / nifmodules                      # MainModule (type context, threaded through)
import intrinsiceffects                       # is this `(instr …)` a pure value?
import ".." / typenav                         # getNominalType — the temp's declared type

type
  AccessInfo = object
    key: string        # base + invariant offset: accesses with the same key
                       # share one pointer (`c[ai + j]` read and written)
    pos: int           # position of the `(at|pat base idx)` subtree in orig
    atCursor: Cursor   # cursor at the same position (for `addSubtree`)
    base: Cursor       # the array/pointer the access indexes
    inv: Cursor        # `idx = iv + inv` (or `inv + iv`); nil for `idx = iv`

  Context = object
    scaledIndex: bool  ## the target addresses `a[i]` in one operand: keep the
                       ## index (count to zero) instead of walking pointers
    orig: ptr TokenBuf
    patchset: Patchset
    synth: seq[TokenBuf]
    tempCounter: int
    m: ptr MainModule  ## module type context; nil ⇒ emit `.` and let the backend
                       ## re-infer (what the self-tests, which parse a body in
                       ## isolation, still do)

proc createContext(orig: ptr TokenBuf;
                   m: ptr MainModule; scaledIndex: bool): Context =
  Context(orig: orig, scaledIndex: scaledIndex,
          patchset: initPatchset(orig),
          synth: @[],
          tempCounter: 0,
          m: m)

proc freshTempName(c: var Context; role = "p"): string =
  inc c.tempCounter
  # A pointer induction variable is a LOCAL of this body, so its name is a
  # local's: `<ident>.<disamb>`. The backtick keeps it out of the source
  # namespace, like every other minted name here. `role` tells the temps of the
  # count-to-zero form apart in dumps: `q` an end base, `j` the negative index.
  result = "`iv." & role & "." & $c.tempCounter

# ---- pattern detection ----------------------------------------------------

proc isIvIncPattern(c: Cursor; outIvSym: var SymId): bool =
  ## Matches `(asgn lhs (add T lhs 1))`. Sets `outIvSym` to `lhs`'s id.
  if c.kind != TagLit: return false
  if c.stmtKind != AsgnS: return false
  var probe = c
  inc probe                                  # past `(asgn`
  if probe.kind != Symbol: return false
  let lhsSym = symId(probe)
  skip probe                                 # past lhs
  if probe.kind != TagLit or probe.exprKind != AddC: return false
  var rhs = probe
  inc rhs                                    # past `(add`
  skip rhs                                   # past the type
  if rhs.kind != Symbol or symId(rhs) != lhsSym: return false
  skip rhs                                   # past `iv`
  if rhs.kind != IntLit: return false
  if intVal(rhs) != 1: return false
  outIvSym = lhsSym
  return true

proc loopBodyCursor(loopCursor: Cursor): Cursor =
  ## Cursor at the loop body. Handles `(while cond body)` and `(loop body)`.
  result = loopCursor
  case loopCursor.stmtKind
  of WhileS:
    inc result                               # past `(while`
    skip result                              # past cond
  of LoopS:
    inc result                               # past `(loop`
  else: discard

proc writeTargetOf(start: Cursor): Cursor =
  ## The lvalue a statement writes: `(asgn dest src)` → `dest`.
  result = start
  inc result                                 # past the tag

proc countWritesOf(start: Cursor; sym: SymId; counter: var int) =
  ## Counts Symbol-LHS assignments to `sym` anywhere in the subtree at `start`,
  ## including nested loops.
  if not start.hasMore: return
  case start.kind
  of TagLit:
    if start.stmtKind == AsgnS:
      let lhs = writeTargetOf(start)
      if lhs.kind == Symbol and symId(lhs) == sym:
        inc counter
    var n = start
    n.loopInto:
      countWritesOf(n, sym, counter)
      skip n
  else: discard

proc scanBody(c: var Context; n: Cursor;
              candidates: var seq[tuple[ivSym: SymId, incPos: int]];
              addrTaken: var HashSet[SymId]) =
  ## Single pre-scan: collects iv-inc candidates AND every symbol whose address
  ## is taken anywhere in the body.
  if not n.hasMore or n.kind != TagLit: return
  var ivSym = SymId(0)
  if isIvIncPattern(n, ivSym):
    candidates.add (ivSym, cursorToPosition(c.orig[], n))
    return                                   # iv-inc's children can't matter
  if n.exprKind in AddrKinds:
    var probe = n
    inc probe                                # past `(addr`/`(haddr`
    while probe.hasMore:
      if probe.kind == Symbol:
        addrTaken.incl symId(probe)
        break
      elif probe.kind == TagLit:
        inc probe                            # descend into chain
      else:
        inc probe
    return
  var m = n
  m.loopInto:
    scanBody(c, m, candidates, addrTaken)
    skip m

proc slotRoot(n: Cursor): SymId =
  ## The local whose own storage the lvalue `n` names — `x`, `x.f`, `x[i]` —
  ## or nothing when the spine goes through a `deref`/`pat`: that storage is
  ## someone else's, and `x` itself is only read.
  result = SymId(0)
  var c = n
  var done = false
  while not done:
    case c.kind
    of Symbol:
      result = symId(c)
      done = true
    of TagLit:
      case c.exprKind
      of DotC, AtC:
        c = c.childCursor
      of ConvC, CastC:
        c = c.childCursor                    # the type
        skip c
      else: done = true
    else: done = true

type
  LoopFacts = object
    ## What the loop does to the symbols an access is rooted at. Collected once
    ## per loop; used to decide whether the hoisted `addr base[…]` stays valid
    ## for the whole loop (see `baseIsStable`).
    assigned: HashSet[SymId]   ## symbol-LHS writes anywhere in the loop
    written: HashSet[SymId]    ## locals whose storage a store names: `x = …`,
                               ## `x.f = …`, `x[i] = …`
    addrTaken: HashSet[SymId]  ## `(addr S)` / `(haddr S.f)` anywhere in the loop
    declared: HashSet[SymId]   ## locals declared INSIDE the loop: a fresh value
                               ## per iteration, and absent where the hoisted
                               ## decls go
    ptrStoreTargets: seq[string] ## per store through a pointer: the pointer it
                                 ## writes through (`treeKey`), "" if unknown
    hasCall: bool              ## a callee can rebind a global or an escaped local
    m: ptr MainModule          ## to ask a row whether an `(instr …)` is pure;
                               ## nil ⇒ every intrinsic counts as a call

proc treeKey(n: Cursor; key: var string) =
  ## A structural fingerprint of a pure subtree: tags, symbols, literals.
  case n.kind
  of TagLit:
    key.add '('
    key.addInt int(n.cursorTagId)
    var m = n
    m.loopInto:
      key.add ' '
      treeKey(m, key)
      skip m
    key.add ')'
  of Symbol:
    key.add 's'
    key.addInt int(symId(n))
  of IntLit:
    key.add 'i'
    key.addInt intVal(n)
  of UIntLit:
    key.add 'u'
    key.add $uintVal(n)
  else:
    key.add '?'

proc pointerKey(lhs: Cursor): string =
  ## The pointer a store through a pointer writes through: the operand of the
  ## first `pat` or `deref` on the lvalue's spine, as a `treeKey`. `at` is
  ## followed into its operand — it may index an array FIELD of the object
  ## behind the pointer — and so is `dot`. "" when there is none to name.
  result = ""
  var c = lhs
  var done = false
  while not done:
    if c.kind == TagLit:
      case c.exprKind
      of PatC, DerefC:
        treeKey(c.childCursor, result)
        done = true
      of DotC, AtC:
        c = c.childCursor
      of ConvC, CastC:
        c = c.childCursor                    # the type
        skip c
      else: done = true
    else: done = true

proc collectLoopFacts(n: Cursor; f: var LoopFacts) =
  if not n.hasMore or n.kind != TagLit: return
  if n.stmtKind in {VarS, ConstS}:
    let name = n.childCursor
    if name.kind == SymbolDef: f.declared.incl symId(name)
  if n.stmtKind == AsgnS:
    let lhs = writeTargetOf(n)
    if lhs.kind == Symbol: f.assigned.incl symId(lhs)
    let root = slotRoot(lhs)
    if root != SymId(0): f.written.incl root
    else: f.ptrStoreTargets.add pointerKey(lhs)
  # An `(instr …)` counts unless its row is `efPure`: the loop rewrites below
  # hoist an address and assume nothing in the body wrote the memory it names, and
  # a `volatileStore` in the loop is exactly something that did. `Ctz` and friends
  # keep the loop rewritable, which is the whole reason this is per-row.
  if n.stmtKind == CallS or n.exprKind == CallC: f.hasCall = true
  elif (n.stmtKind == InstrS or n.exprKind == InstrC) and
       not (f.m != nil and instrIsPure(f.m[], n)): f.hasCall = true
  if n.exprKind in AddrKinds:
    let root = slotRoot(n.childCursor)
    if root != SymId(0): f.addrTaken.incl root
  var m = n
  m.loopInto:
    collectLoopFacts(m, f)
    skip m

proc symIsInvariant(c: Context; s: SymId; f: LoopFacts): bool =
  ## Same value on every iteration: the loop neither writes the symbol nor takes
  ## its address, and — unless it is a local — calls nothing that could.
  s notin f.declared and s notin f.written and s notin f.addrTaken and
    (c.orig[].pool.symIsLocal(s) or not f.hasCall)

proc baseIsStable(c: var Context; base: Cursor; f: LoopFacts): bool =
  ## Is `addr base[…]` — the address this rewrite hoists out of the loop — the
  ## same on every iteration?
  ##
  ## For an ARRAY base it always is: the address is the variable's own storage,
  ## which no assignment moves (`arr = other` copies *into* it) and no callee can
  ## relocate. That is the case this pass was written for.
  ##
  ## For a POINTER-like base (`ptr`/`aptr`/`flexarray`, all of which `typenav`
  ## admits under `(at …)`) the hoisted address IS the base's value, so a loop
  ## that rebinds it — directly, through a taken address, or through a callee
  ## that can reach it — would leave the pointer walking the old buffer. The
  ## same holds when the type does not resolve at all: unproven is not proven.
  ##
  ## A seq's payload is a FIELD: `(dot S data)` of a local copy is stable like
  ## the local. `(dot (deref P) data)` of a `var` parameter lives in memory, so
  ## additionally no call (which could grow the seq and move its payload), and
  ## every store through a pointer must go through this very base: a store into
  ## `P`'s payload cannot overwrite `P`'s header, since no seq contains its own
  ## header. A store through any other pointer — `P` itself included — could.
  result = false
  case base.kind
  of Symbol:
    if symId(base) in f.declared:
      result = false                         # does not exist before the loop
    elif c.m != nil and
       navigateToObjectBody(c.m[], getType(c.m[], base)).typeKind == ArrayT:
      result = true
    else:
      # only `base = …` rebinds it; an element store `base[i] = …` does not
      let b = symId(base)
      result = b notin f.assigned and b notin f.addrTaken and not f.hasCall
  of TagLit:
    if base.exprKind == DotC:
      let obj = base.childCursor
      if obj.kind == Symbol:
        result = symIsInvariant(c, symId(obj), f)
      elif obj.kind == TagLit and obj.exprKind == DerefC:
        let p = obj.childCursor
        if p.kind == Symbol and symIsInvariant(c, symId(p), f) and not f.hasCall:
          var own = ""
          treeKey(base, own)
          result = true
          for target in f.ptrStoreTargets:
            if target != own: result = false
  else: discard

proc operandIsInvariant(c: Context; inv: Cursor; f: LoopFacts): bool =
  ## The loop bound, or the `inv` of an `iv + inv` index: a literal (suffixed
  ## or not), or an invariant symbol. Nil — the index is the bare counter — is
  ## trivially invariant.
  if cursorIsNil(inv): result = true
  else:
    case inv.kind
    of IntLit, UIntLit: result = true
    of Symbol: result = symIsInvariant(c, symId(inv), f)
    of TagLit:
      result = inv.exprKind == SufC and inv.childCursor.kind in {IntLit, UIntLit}
    else: result = false

proc findIV(c: var Context; body: Cursor): tuple[ivSym: SymId, incPos: int, found: bool] =
  ## Identifies the loop body's induction variable (exactly one
  ## `(asgn iv (add T iv 1))`, no other writes, address not taken).
  result = (SymId(0), 0, false)
  if body.kind != TagLit: return

  var candidates: seq[tuple[ivSym: SymId, incPos: int]] = @[]
  var addrTaken = initHashSet[SymId]()
  if body.stmtKind == StmtsS:
    var m = body
    m.loopInto:
      scanBody(c, m, candidates, addrTaken)
      skip m
  else:
    scanBody(c, body, candidates, addrTaken)

  if candidates.len != 1: return
  let (ivSym, incPos) = candidates[0]
  if ivSym in addrTaken: return              # IV's address escapes — unsafe

  var writes = 0
  countWritesOf(body, ivSym, writes)
  if writes != 1: return

  result = (ivSym, incPos, true)

# ---- access collection ----------------------------------------------------

proc isIv(n: Cursor; ivSym: SymId): bool {.inline.} =
  n.kind == Symbol and symId(n) == ivSym

proc splitIndex(idx: Cursor; ivSym: SymId; inv: var Cursor): bool =
  ## Is `idx` the counter, or `(add T iv inv)` / `(add T inv iv)`? Sets `inv`
  ## (nil for the bare counter). The invariance of `inv` is checked later.
  inv = default(Cursor)
  if isIv(idx, ivSym):
    result = true
  elif idx.kind == TagLit and idx.exprKind == AddC:
    var x = idx.childCursor                  # the type
    skip x
    var y = x
    if y.hasMore: skip y
    result = false
    if x.hasMore and y.hasMore:
      var after = y
      skip after
      if not after.hasMore:
        if isIv(x, ivSym) and not isIv(y, ivSym):
          inv = y
          result = true
        elif isIv(y, ivSym) and not isIv(x, ivSym):
          inv = x
          result = true
  else:
    result = false

proc collectAccesses(c: var Context; start: Cursor; ivSym: SymId;
                     accesses: var seq[AccessInfo]) =
  ## Walks the subtree at `start` collecting `(at|pat base idx)` accesses whose
  ## index is the counter, or the counter plus something; does not descend into
  ## nested loops. Whether the base and the offset are loop-invariant is the
  ## caller's question.
  if not start.hasMore: return
  case start.kind
  of TagLit:
    if start.stmtKind in {WhileS, LoopS}:
      return
    var matched = false
    if start.exprKind in {AtC, PatC}:
      let base = start.childCursor
      var idx = base
      skip idx
      var inv = default(Cursor)
      if idx.hasMore and splitIndex(idx, ivSym, inv):
        var key = ""
        treeKey(base, key)
        key.add '+'
        if not cursorIsNil(inv): treeKey(inv, key)
        accesses.add AccessInfo(key: key, pos: cursorToPosition(c.orig[], start),
                                atCursor: start, base: base, inv: inv)
        matched = true                       # don't recurse into the matched access
    if not matched:
      var n = start
      n.loopInto:
        collectAccesses(c, n, ivSym, accesses)
        skip n
  else: discard

# ---- synthesis -----------------------------------------------------------

proc synthBuf(c: var Context; cap: int): TokenBuf =
  ## A scratch buffer sharing `orig`'s pool/tags so synthesized subtrees splice
  ## in bulk and their symbols/tags resolve identically.
  createTokenBuf(cap, c.orig[].pool, c.orig[].tags)

proc addPtrVarDecl(c: var Context; p: string; atCursor: Cursor): int =
  ## Synthesize `(var :p . . (addr <atCursor>))`.
  result = c.synth.len
  let info = rawLineInfo(atCursor)
  var buf = synthBuf(c, 16)
  buf.openTag TagId(ord(VarTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymDef p
  buf.addDotToken()                          # pragmas
  # Declared type: `(ptr typeof <atCursor>)`. Spelled out rather than left to the
  # backend's re-inference, for the same reason as `cse.emitTempType`.
  if c.m == nil:
    buf.addDotToken()                        # no type context: self-tests only
  else:
    buf.openTag TagId(ord(PtrTagId))
    buf.addSubtree getNominalType(c.m[], atCursor)
    buf.closeTag()
  buf.openTag TagId(ord(AddrTagId))
  buf.addSubtree atCursor
  buf.closeTag()                             # close addr
  buf.closeTag()                             # close var
  c.synth.add ensureMove(buf)

proc addPtrInc(c: var Context; p: string; info: NifLineInfo): int =
  ## Synthesize `(asgn p (addr (pat p 1)))`.
  result = c.synth.len
  var buf = synthBuf(c, 10)
  buf.openTag TagId(ord(AsgnTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymUse p
  buf.openTag TagId(ord(AddrTagId))
  buf.openTag TagId(ord(PatTagId))
  buf.addSymUse p
  buf.addIntLit 1
  buf.closeTag()                             # close pat
  buf.closeTag()                             # close addr
  buf.closeTag()                             # close asgn
  c.synth.add ensureMove(buf)

proc addDeref(c: var Context; p: string; info: NifLineInfo): int =
  ## Synthesize `(deref p)`.
  result = c.synth.len
  var buf = synthBuf(c, 4)
  buf.openTag TagId(ord(DerefTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymUse p
  buf.closeTag()
  c.synth.add ensureMove(buf)

proc addEndBaseDecl(c: var Context; q, p: string; atCursor, bound, ivType: Cursor;
                    ivSym: SymId): int =
  ## Synthesize `(var :q . (ptr T) (addr (pat p (sub ivT bound iv))))`: the
  ## element `bound - iv` past `p = addr arr[iv]`, i.e. `addr arr[bound]`.
  ## `pat` on `p` rather than `(at arr bound)` so no (possibly checked)
  ## one-past-the-end index is formed.
  result = c.synth.len
  let info = rawLineInfo(atCursor)
  var buf = synthBuf(c, 24)
  buf.openTag TagId(ord(VarTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymDef q
  buf.addDotToken()                          # pragmas
  if c.m == nil:
    buf.addDotToken()
  else:
    buf.openTag TagId(ord(PtrTagId))
    buf.addSubtree getNominalType(c.m[], atCursor)
    buf.closeTag()
  buf.openTag TagId(ord(AddrTagId))
  buf.openTag TagId(ord(PatTagId))
  buf.addSymUse p
  buf.openTag TagId(ord(SubTagId))
  buf.addSubtree ivType
  buf.addSubtree bound
  buf.addSymUse ivSym
  buf.closeTag()                             # close sub
  buf.closeTag()                             # close pat
  buf.closeTag()                             # close addr
  buf.closeTag()                             # close var
  c.synth.add ensureMove(buf)

proc addIndexDecl(c: var Context; j: string; ivType, bound: Cursor; ivSym: SymId;
                  info: NifLineInfo): int =
  ## Synthesize `(var :j . T (sub T iv bound))`: the counter, offset so that it
  ## reaches 0 exactly when `iv` reaches `bound`.
  result = c.synth.len
  var buf = synthBuf(c, 16)
  buf.openTag TagId(ord(VarTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymDef j
  buf.addDotToken()                          # pragmas
  buf.addSubtree ivType
  buf.openTag TagId(ord(SubTagId))
  buf.addSubtree ivType
  buf.addSymUse ivSym
  buf.addSubtree bound
  buf.closeTag()                             # close sub
  buf.closeTag()                             # close var
  c.synth.add ensureMove(buf)

proc addIndexInc(c: var Context; j: string; ivType: Cursor; info: NifLineInfo): int =
  ## Synthesize `(asgn j (add T j 1))`.
  result = c.synth.len
  var buf = synthBuf(c, 10)
  buf.openTag TagId(ord(AsgnTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymUse j
  buf.openTag TagId(ord(AddTagId))
  buf.addSubtree ivType
  buf.addSymUse j
  buf.addIntLit 1
  buf.closeTag()                             # close add
  buf.closeTag()                             # close asgn
  c.synth.add ensureMove(buf)

proc addZeroCond(c: var Context; cond: Cursor; j: string): int =
  ## The loop comparison `(op iv bound)` as `(op j 0)`, same `op`: `j` is
  ## `iv - bound`, so `lt`/`le`/`eq`/`neq` all agree.
  result = c.synth.len
  var buf = synthBuf(c, 6)
  buf.openTag cond.cursorTagId
  let info = rawLineInfo(cond)
  if info.isValid: buf.appendLineInfo info
  buf.addSymUse j
  buf.addIntLit 0
  buf.closeTag()
  c.synth.add ensureMove(buf)

proc addPatAccess(c: var Context; q, j: string; info: NifLineInfo): int =
  ## Synthesize `(pat q j)`: `arr[iv]` is `arr[bound + j]`.
  result = c.synth.len
  var buf = synthBuf(c, 6)
  buf.openTag TagId(ord(PatTagId))
  if info.isValid: buf.appendLineInfo info
  buf.addSymUse q
  buf.addSymUse j
  buf.closeTag()
  c.synth.add ensureMove(buf)

proc countReads(n: Cursor; sym: SymId): int =
  ## Occurrences of `sym` that READ it: everything but its declaration and the
  ## target of a plain `(asgn sym …)`.
  result = 0
  case n.kind
  of Symbol:
    if symId(n) == sym: result = 1
  of TagLit:
    var m = n
    var first = true
    let isAsgn = n.stmtKind == AsgnS
    m.loopInto:
      if not (first and isAsgn and m.kind == Symbol):
        result += countReads(m, sym)
      first = false
      skip m
  else: discard

proc loopCounterCond(body: Cursor; ivSym: SymId): Cursor =
  ## The comparison in the body's trailing `(continue (lt|le|neq iv bound))`, or
  ## nil. `!=` arrives as `(not (eq iv bound))`; then it is the inner `eq` — the
  ## `not` stays where it is. Only a rotated `(loop …)` has a trailing test; its
  ## guard ran the same test before the first iteration, so the decls hoisted in
  ## front of the loop only execute when the loop does — which is also why
  ## `iv - bound` cannot overflow there.
  result = default(Cursor)
  if body.kind == TagLit and body.stmtKind == StmtsS:
    var last = default(Cursor)
    var m = body
    m.loopInto:
      last = m
      skip m
    if not cursorIsNil(last) and last.kind == TagLit and last.stmtKind == ContinueS:
      var cond = last.childCursor
      if cond.kind == TagLit and cond.exprKind == NotC and
         cond.childCursor.kind == TagLit and cond.childCursor.exprKind == EqC:
        cond = cond.childCursor
      if cond.kind == TagLit and cond.exprKind in {LtC, LeC, NeqC, EqC}:
        let iv = cond.childCursor
        var bound = iv
        skip bound
        # exactly `(op iv bound)`: no type child, nothing after the bound
        if iv.kind == Symbol and symId(iv) == ivSym and bound.hasMore:
          var after = bound
          skip after
          if not after.hasMore: result = cond

# ---- main traversal -------------------------------------------------------

proc tr(c: var Context; n: var Cursor)   # forward

proc rewriteToPointers(c: var Context; loopPos: int; incPos: int;
                       stable: seq[AccessInfo]) =
  ## One pointer per array, bumped next to the counter's increment:
  ## `(at arr iv)` → `(deref p)`. The counter stays; it still has readers.
  var arrToP = initTable[string, string]()
  for acc in stable:
    var p: string
    if acc.key in arrToP:
      p = arrToP.getOrQuit(acc.key)
    else:
      p = freshTempName(c)
      arrToP[acc.key] = p
      let varIdx = addPtrVarDecl(c, p, acc.atCursor)
      c.patchset.addInsert(loopPos, cursorAt(c.synth[varIdx], 0))
      let incIdx = addPtrInc(c, p, rawLineInfo(acc.atCursor))
      c.patchset.addInsert(incPos, cursorAt(c.synth[incIdx], 0))
    let derefIdx = addDeref(c, p, rawLineInfo(acc.atCursor))
    c.patchset.addSubst(acc.pos, cursorAt(c.synth[derefIdx], 0))

proc rewriteToZero(c: var Context; loopPos: int; ivSym: SymId; incPos: int;
                   stable: seq[AccessInfo]; cond, bound, ivType: Cursor) =
  ## Count to zero: `j = iv - bound` runs up to 0, every array is addressed off
  ## its END, `q = addr arr[bound]`, as `(pat q j)`, and the loop test is
  ## `(op j 0)`. One increment serves every array, no bound stays live in the
  ## loop, and `(pat q j)` is a single scaled-index address on x64 and AArch64.
  let info = rawLineInfo(cond)
  let j = freshTempName(c, "j")
  let jIdx = addIndexDecl(c, j, ivType, bound, ivSym, info)
  c.patchset.addInsert(loopPos, cursorAt(c.synth[jIdx], 0))
  var arrToQ = initTable[string, string]()
  for acc in stable:
    var q: string
    if acc.key in arrToQ:
      q = arrToQ.getOrQuit(acc.key)
    else:
      let p = freshTempName(c)
      q = freshTempName(c, "q")
      arrToQ[acc.key] = q
      let pIdx = addPtrVarDecl(c, p, acc.atCursor)
      c.patchset.addInsert(loopPos, cursorAt(c.synth[pIdx], 0))
      let qIdx = addEndBaseDecl(c, q, p, acc.atCursor, bound, ivType, ivSym)
      c.patchset.addInsert(loopPos, cursorAt(c.synth[qIdx], 0))
    let patIdx = addPatAccess(c, q, j, rawLineInfo(acc.atCursor))
    c.patchset.addSubst(acc.pos, cursorAt(c.synth[patIdx], 0))
  let incIdx = addIndexInc(c, j, ivType, info)
  c.patchset.addSubst(incPos, cursorAt(c.synth[incIdx], 0))
  let condIdx = addZeroCond(c, cond, j)
  c.patchset.addSubst(cursorToPosition(c.orig[], cond), cursorAt(c.synth[condIdx], 0))

proc transformLoop(c: var Context; n: var Cursor) =
  let loopPos = cursorToPosition(c.orig[], n)
  let body = loopBodyCursor(n)
  let ivInfo = findIV(c, body)
  if ivInfo.found:
    var accesses: seq[AccessInfo] = @[]
    collectAccesses(c, body, ivInfo.ivSym, accesses)
    var facts = LoopFacts(assigned: initHashSet[SymId](), written: initHashSet[SymId](),
                          declared: initHashSet[SymId](),
                          addrTaken: initHashSet[SymId](), m: c.m)
    collectLoopFacts(body, facts)
    var stable: seq[AccessInfo] = @[]
    for acc in accesses:
      if baseIsStable(c, acc.base, facts) and operandIsInvariant(c, acc.inv, facts):
        stable.add acc
    if stable.len > 0:
      if not c.scaledIndex:
        # A RISC addresses `a[i]` with a shift and an add on every access; a
        # pointer bumped once per iteration is cheaper there.
        rewriteToPointers(c, loopPos, ivInfo.incPos, stable)
      else:
        # x86/ARM fold `a[i]` into the operand, so a pointer walk only adds a
        # bump per array per iteration (measured: +22 % on a two-array copy).
        # What pays there is counting to zero, when the counter does nothing
        # but index and test: every read of `iv` anywhere in the proc is a
        # reducible access, the loop test or the increment ("anywhere" rather
        # than "after the loop" keeps an enclosing loop's back edge from
        # needing its own argument), the bound is loop-invariant, and `iv` is
        # signed, so `iv - bound` goes negative. Otherwise the loop stays as is.
        var add = cursorAt(c.orig[], ivInfo.incPos)
        add = add.childCursor                # past `(asgn` → iv
        skip add                             # → `(add T iv 1)`
        let ivType = add.childCursor
        let cond = loopCounterCond(body, ivInfo.ivSym)
        if not cursorIsNil(cond) and ivType.kind == TagLit and ivType.typeKind == IT:
          var bound = cond.childCursor
          skip bound
          if operandIsInvariant(c, bound, facts) and
             countReads(beginRead(c.orig[]), ivInfo.ivSym) == stable.len + 2:
            rewriteToZero(c, loopPos, ivInfo.ivSym, ivInfo.incPos, stable, cond, bound, ivType)

  # Recurse into the loop body so nested loops are transformed too.
  n.loopInto:
    tr(c, n)

proc registerDecl(c: var Context; n: Cursor) =
  ## Records a local's declared type in the current typenav scope, so that a
  ## later `(at LOCAL iv)` can be typed. `optdriver.rebuildTree` opens the scope
  ## and registers the proc's *params*; the body's locals are ours to add, and
  ## since `tr` walks in source order every decl is registered before the loops
  ## that can use it. Mirrors the `VarS` case of `aliasing.walk`.
  ##
  ## Without this, a body-local array whose type is a nominal symbol — e.g. the
  ## 32-byte word buffer a `char` set operation desugars to,
  ## `` `t.0.IAarrayAuS8ZS32. `` rather than a literal `(array …)` — resolves to
  ## the `(err)` sentinel and `addPtrVarDecl` bakes `(ptr (err))` into the
  ## declared type, which the backends then reject.
  if c.m == nil: return
  var probe = n
  var nameStart = default(Cursor)
  var typeStart = default(Cursor)
  probe.into:
    nameStart = probe
    skip probe                             # name
    if probe.hasMore: skip probe           # pragmas
    if probe.hasMore:
      typeStart = probe
      skip probe                           # type
    while probe.hasMore: skip probe
  if nameStart.kind == SymbolDef and not cursorIsNil(typeStart):
    c.m[].registerLocal(nameStart.symId, typeStart)

proc tr(c: var Context; n: var Cursor) =
  if not n.hasMore: return
  case n.kind
  of TagLit:
    case n.stmtKind
    of WhileS, LoopS:
      transformLoop(c, n)
    of VarS, GvarS, TvarS, ConstS:
      registerDecl(c, n)
      n.loopInto:
        tr(c, n)
    of ScopeS:
      if c.m != nil: c.m[].openScope()
      n.loopInto:
        tr(c, n)
      if c.m != nil: c.m[].closeScope()
    else:
      n.loopInto:
        tr(c, n)
  else:
    inc n

# ---- public entry --------------------------------------------------------

proc runInductionVariables*(buf: var TokenBuf;
                            m: ptr MainModule = nil; scaledIndex = false) =
  ## Two-phase IV strength reduction: record rewrite patches, then rebuild
  ## `buf`. Without scaled-index addressing (`scaledIndex` false) every
  ## `(at arr iv)` becomes `(deref p)` for a pointer bumped next to the iv-inc;
  ## with it, a loop whose counter only indexes counts to zero instead
  ## (`rewriteToZero`) and every other loop is left alone.
  var ctx = createContext(addr buf, m, scaledIndex)
  var n = beginRead(buf)
  tr(ctx, n)
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

  template assertSame(buf: var TokenBuf; expected: string) =
    let got = toString(buf)
    let want = canon(expected)
    doAssert got == want, "MISMATCH\n  got:  " & got & "\n  want: " & want

  template assertUnchangedScaled(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runInductionVariables(buf, scaledIndex = true)
    doAssert toString(buf) == before, "expected unchanged (scaled index):\n  " & input

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = toString(buf)
    runInductionVariables buf
    doAssert toString(buf) == before, "expected unchanged:\n  " & input

  block simple_iv:
    var buf = parse(
      "(stmts (asgn i.0.M 0) (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (asgn i.0.M 0) (var :`iv.p.1 . . (addr (at arr.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref `iv.p.1)) " &
      "(asgn `iv.p.1 (addr (pat `iv.p.1 1))) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block multiple_accesses_same_array:
    var buf = parse(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn y.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (var :`iv.p.1 . . (addr (at arr.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref `iv.p.1)) " &
      "(asgn y.0.M (deref `iv.p.1)) (asgn `iv.p.1 (addr (pat `iv.p.1 1))) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)))))")

  block multiple_arrays:
    var buf = parse(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr1.0.M i.0.M)) (asgn y.0.M (at arr2.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (var :`iv.p.1 . . (addr (at arr1.0.M i.0.M))) " &
      "(var :`iv.p.2 . . (addr (at arr2.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref `iv.p.1)) " &
      "(asgn y.0.M (deref `iv.p.2)) (asgn `iv.p.1 (addr (pat `iv.p.1 1))) " &
      "(asgn `iv.p.2 (addr (pat `iv.p.2 1))) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block iv_used_outside_at:
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (call use.0.M i.0.M) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block no_iv_no_transform:
    assertUnchanged("(stmts (while c.0.M (stmts (asgn x.0.M 1))))")

  block base_reassigned_in_loop:
    # The hoisted `addr arr[0]` is only the loop-invariant address of a stable
    # storage if `arr` is an array. Here the type does not resolve (no module
    # context), and the loop rebinds `arr`, so the rewrite would leave the
    # pointer walking the old buffer.
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) " &
      "(asgn arr.0.M other.0.M) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block call_in_loop_with_unresolved_base:
    # Same reasoning one step removed: a callee can rebind a global or an escaped
    # local. With a resolved ARRAY type this transforms as before — the address
    # of an array variable is not something a callee can move.
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) " &
      "(call f.0.M) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block base_addr_taken_in_loop:
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) " &
      "(call f.0.M (addr arr.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block iv_written_twice_disqualifies:
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M 7) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block iv_addr_taken_disqualifies:
    assertUnchanged(
      "(stmts (while (lt (i 32) i.0.M 10) (stmts (call use.0.M (addr i.0.M)) (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")

  block nested_loops_inner_only:
    var buf = parse(
      "(stmts (while c.0.M (stmts (while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (while c.0.M (stmts (var :`iv.p.1 . . (addr (at arr.0.M i.0.M))) " &
      "(while (lt (i 32) i.0.M 10) (stmts (asgn x.0.M (deref `iv.p.1)) " &
      "(asgn `iv.p.1 (addr (pat `iv.p.1 1))) (asgn i.0.M (add (i 32) i.0.M 1)))))))")

  block count_to_zero:
    # A rotated loop whose counter only indexes and tests: `j = i - n` counts up
    # to 0, the array is addressed off `q = addr arr[n]`, and the test is
    # `(lt j 0)` — no bound left in the loop.
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))) .))")
    runInductionVariables(buf, scaledIndex = true)
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts " &
      "(var :`iv.j.1 . (i 32) (sub (i 32) i.0.M n.0.M)) " &
      "(var :`iv.p.2 . . (addr (at arr.0.M i.0.M))) " &
      "(var :`iv.q.3 . . (addr (pat `iv.p.2 (sub (i 32) n.0.M i.0.M)))) " &
      "(loop (stmts (asgn x.0.M (pat `iv.q.3 `iv.j.1)) " &
      "(asgn `iv.j.1 (add (i 32) `iv.j.1 1)) " &
      "(continue (lt `iv.j.1 0))))) .))")

  block count_to_zero_two_arrays:
    # One index serves both arrays: one increment per iteration, not one each.
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn (at b.0.M i.0.M) (at a.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))) .))")
    runInductionVariables(buf, scaledIndex = true)
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts " &
      "(var :`iv.j.1 . (i 32) (sub (i 32) i.0.M n.0.M)) " &
      "(var :`iv.p.2 . . (addr (at b.0.M i.0.M))) " &
      "(var :`iv.q.3 . . (addr (pat `iv.p.2 (sub (i 32) n.0.M i.0.M)))) " &
      "(var :`iv.p.4 . . (addr (at a.0.M i.0.M))) " &
      "(var :`iv.q.5 . . (addr (pat `iv.p.4 (sub (i 32) n.0.M i.0.M)))) " &
      "(loop (stmts (asgn (pat `iv.q.3 `iv.j.1) (pat `iv.q.5 `iv.j.1)) " &
      "(asgn `iv.j.1 (add (i 32) `iv.j.1 1)) " &
      "(continue (lt `iv.j.1 0))))) .))")

  block count_to_zero_not_eq:
    # `while i != n` is `(not (eq i n))`: the inner `eq` becomes `(eq j 0)`.
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (not (eq 0 n.0.M)) (stmts (loop (stmts " &
      "(asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)) " &
      "(continue (not (eq i.0.M n.0.M)))))) .))")
    runInductionVariables(buf, scaledIndex = true)
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (not (eq 0 n.0.M)) (stmts " &
      "(var :`iv.j.1 . (i 32) (sub (i 32) i.0.M n.0.M)) " &
      "(var :`iv.p.2 . . (addr (at arr.0.M i.0.M))) " &
      "(var :`iv.q.3 . . (addr (pat `iv.p.2 (sub (i 32) n.0.M i.0.M)))) " &
      "(loop (stmts (asgn x.0.M (pat `iv.q.3 `iv.j.1)) " &
      "(asgn `iv.j.1 (add (i 32) `iv.j.1 1)) " &
      "(continue (not (eq `iv.j.1 0)))))) .))")

  block count_to_zero_affine_index:
    # `a[i + k]` with an invariant `k`: the end base is `addr a[n + k]`.
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (at arr.0.M (add (i 32) i.0.M k.0.M))) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)) (continue (lt i.0.M n.0.M))))) .))")
    runInductionVariables(buf, scaledIndex = true)
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts " &
      "(var :`iv.j.1 . (i 32) (sub (i 32) i.0.M n.0.M)) " &
      "(var :`iv.p.2 . . (addr (at arr.0.M (add (i 32) i.0.M k.0.M)))) " &
      "(var :`iv.q.3 . . (addr (pat `iv.p.2 (sub (i 32) n.0.M i.0.M)))) " &
      "(loop (stmts (asgn x.0.M (pat `iv.q.3 `iv.j.1)) " &
      "(asgn `iv.j.1 (add (i 32) `iv.j.1 1)) " &
      "(continue (lt `iv.j.1 0))))) .))")

  block count_to_zero_field_base:
    # A seq's payload, `(pat (dot b data) i)` of a local copy `b`.
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (pat (dot b.0.M data.0.M 0) i.0.M)) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)) (continue (lt i.0.M n.0.M))))) .))")
    runInductionVariables(buf, scaledIndex = true)
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts " &
      "(var :`iv.j.1 . (i 32) (sub (i 32) i.0.M n.0.M)) " &
      "(var :`iv.p.2 . . (addr (pat (dot b.0.M data.0.M 0) i.0.M))) " &
      "(var :`iv.q.3 . . (addr (pat `iv.p.2 (sub (i 32) n.0.M i.0.M)))) " &
      "(loop (stmts (asgn x.0.M (pat `iv.q.3 `iv.j.1)) " &
      "(asgn `iv.j.1 (add (i 32) `iv.j.1 1)) " &
      "(continue (lt `iv.j.1 0))))) .))")

  block deref_base_own_payload_store:
    # The only store goes into `(*c).data`'s own payload: it cannot rebind
    # `(*c).data`, so the base is hoisted.
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn (pat (dot (deref c.0.M) data.0.M 0) i.0.M) x.0.M) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)) (continue (lt i.0.M n.0.M))))) .))")
    runInductionVariables(buf, scaledIndex = true)
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts " &
      "(var :`iv.j.1 . (i 32) (sub (i 32) i.0.M n.0.M)) " &
      "(var :`iv.p.2 . . (addr (pat (dot (deref c.0.M) data.0.M 0) i.0.M))) " &
      "(var :`iv.q.3 . . (addr (pat `iv.p.2 (sub (i 32) n.0.M i.0.M)))) " &
      "(loop (stmts (asgn (pat `iv.q.3 `iv.j.1) x.0.M) " &
      "(asgn `iv.j.1 (add (i 32) `iv.j.1 1)) " &
      "(continue (lt `iv.j.1 0))))) .))")

  block deref_base_foreign_store_left_alone:
    # A store through another pointer could hit `(*c).data`: not hoisted.
    assertUnchangedScaled(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (pat (dot (deref c.0.M) data.0.M 0) i.0.M)) " &
      "(asgn (deref q.0.M) y.0.M) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)) (continue (lt i.0.M n.0.M))))) .))")

  block deref_base_header_store_left_alone:
    # `(*c).len = …` writes the header itself, through `c`.
    assertUnchangedScaled(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (pat (dot (deref c.0.M) data.0.M 0) i.0.M)) " &
      "(asgn (dot (deref c.0.M) len.0.M 0) y.0.M) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)) (continue (lt i.0.M n.0.M))))) .))")

  block base_declared_in_loop_left_alone:
    # SROA re-derives a seq's payload into a fresh local per iteration: the
    # base does not exist where the hoisted decls would go.
    assertUnchangedScaled(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(var :d.1 . (aptr (i 64)) (dot s.0.M data.0.M 0)) " &
      "(asgn x.0.M (pat d.1 i.0.M)) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)) (continue (lt i.0.M n.0.M))))) .))")
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (while (lt (i 32) i.0.M 10) (stmts " &
      "(var :d.1 . (aptr (i 64)) (dot s.0.M data.0.M 0)) " &
      "(asgn x.0.M (pat d.1 i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)))))")
    let before = toString(buf)
    runInductionVariables buf
    doAssert toString(buf) == before, "pointer walk hoisted a loop-local base"

  block unsigned_counter_left_alone:
    # `i - n` would wrap instead of going negative, and a scaled-index target
    # gains nothing from a pointer walk: the loop is not touched.
    assertUnchangedScaled(
      "(stmts (var :i.0.M . . 0u) (ite (lt 0u n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (u 32) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))) .))")

  block counter_read_left_alone_scaled:
    # The counter has another reader: on a scaled-index target, no pointer walk.
    assertUnchangedScaled(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (at arr.0.M i.0.M)) (call use.0.M i.0.M) " &
      "(asgn i.0.M (add (i 32) i.0.M 1)) (continue (lt i.0.M n.0.M))))) .))")

  block counter_read_after_loop_stays:
    # `use(i)` after the loop reads the counter: only the access is reduced.
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (at arr.0.M i.0.M)) (asgn i.0.M (add (i 32) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))) .) (call use.0.M i.0.M))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts " &
      "(var :`iv.p.1 . . (addr (at arr.0.M i.0.M))) " &
      "(loop (stmts (asgn x.0.M (deref `iv.p.1)) " &
      "(asgn `iv.p.1 (addr (pat `iv.p.1 1))) (asgn i.0.M (add (i 32) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))) .) (call use.0.M i.0.M))")

  block bound_assigned_in_loop_stays:
    var buf = parse(
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts (loop (stmts " &
      "(asgn x.0.M (at arr.0.M i.0.M)) (asgn n.0.M 5) (asgn i.0.M (add (i 32) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))) .))")
    runInductionVariables buf
    assertSame(buf,
      "(stmts (var :i.0.M . . 0) (ite (lt 0 n.0.M) (stmts " &
      "(var :`iv.p.1 . . (addr (at arr.0.M i.0.M))) " &
      "(loop (stmts (asgn x.0.M (deref `iv.p.1)) (asgn n.0.M 5) " &
      "(asgn `iv.p.1 (addr (pat `iv.p.1 1))) (asgn i.0.M (add (i 32) i.0.M 1)) " &
      "(continue (lt i.0.M n.0.M))))) .))")

  echo "induction_variables.nim: all self-tests passed"

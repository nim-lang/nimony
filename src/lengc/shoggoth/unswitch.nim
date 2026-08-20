#
#
#           NIFC Loop Unswitching (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Hoists a loop-INVARIANT `if` condition out of a `while` by duplicating the
## loop — classic loop unswitching. The motivating shape is an inlined string
## accessor inside a per-char loop:
##
##   (while (lt i len)
##     … (if (elif (le SSO-TEST 14) (stmts SHORT)) (else LONG)) …)
##
## `s[i]`'s "is this string short?" test is re-evaluated EVERY iteration — in
## nifbuilder's `addSymbol` the same test runs four times per character — while
## gcc -O3 evaluates it once and splits the whole path. After unswitching:
##
##   (if (elif SSO-TEST≤14 (stmts (while … SHORT-only …)))
##       (else       (stmts (while … LONG-only …))))
##
## and *every* `if` inside the copies whose condition is structurally identical
## to the hoisted one collapses to the known branch — the four tests become one,
## executed once before the loop.
##
## Soundness gates:
##  * the condition is a PURE, trap-free expression: symbols, literals, field
##    reads of named slots, conversions, comparisons and non-trapping arithmetic
##    — no calls, no deref/indexing, no div/mod (it is evaluated unconditionally
##    now, even when the loop body would never have reached it);
##  * every symbol the condition reads is INVARIANT across the loop: never
##    assigned inside it, and — when the symbol is address-taken anywhere in the
##    proc, or is a global — the loop body must additionally contain no call and
##    no store the scan cannot attribute to a named local slot (nothing can
##    write memory behind the scan's back);
##  * the loop is small enough to duplicate (`MaxUnswitchSpan` tokens).
##
## The second copy's declarations (labels AND locals) are freshened
## (`` `usN.<suffix> ``) so the module keeps unique symbols. Fixpoint driver:
## one loop per round; iterated rounds hoist a condition out of nested loops
## level by level (the inner unswitch leaves `(if C (while…)…)` directly in the
## outer body, which the next round can hoist again).

import std / [assertions, tables, sets]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "lib" / symparser       # isLocalName
import ".." / ".." / "models" / tags         # tag ids for synthesis
import patchsets

const
  MaxUnswitchSpan = 600   ## max tokens of a `while` we are willing to duplicate
  MaxRounds = 16

type
  Candidate = object
    whilePos: int               ## the loop to duplicate (orig position)
    condPos: int                ## the invariant condition (orig position)

  Context = object
    orig: ptr TokenBuf
    suffix: string
    counter: int
    procAddrTaken: HashSet[SymId]

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

proc symNameOf(c: Context; s: SymId): string {.inline.} =
  c.orig[].pool.syms[s]

# ── structural equality ──────────────────────────────────────────────────────

proc sameTree(a, b: Cursor): bool =
  ## Token-wise structural equality of two subtrees (line info ignored).
  if a.kind != b.kind: return false
  case a.kind
  of TagLit:
    if a.cursorTagId != b.cursorTagId: return false
    var x = a
    var y = b
    result = true
    x.into:
      y.into:
        while x.hasMore and y.hasMore:
          if result and not sameTree(x, y): result = false
          skip x
          skip y
        if x.hasMore or y.hasMore: result = false
        while x.hasMore: skip x
        while y.hasMore: skip y
  of Symbol, SymbolDef: result = a.symId == b.symId
  of IntLit: result = intVal(a) == intVal(b)
  of UIntLit: result = uintVal(a) == uintVal(b)
  of CharLit: result = charLit(a) == charLit(b)
  of StrLit, Ident: result = strVal(a) == strVal(b)
  of FloatLit: result = floatVal(a) == floatVal(b)
  of DotToken: result = true
  else: result = false

# ── condition purity + symbol collection ─────────────────────────────────────

proc pureCondImpl(n: var Cursor; syms: var seq[SymId]): bool =
  ## Advances `n` past the subtree; true iff it is safe to evaluate the
  ## expression unconditionally and its value depends only on `syms`.
  case n.kind
  of Symbol:
    syms.add n.symId
    inc n
    result = true
  of IntLit, UIntLit, CharLit, FloatLit:
    inc n
    result = true
  of TagLit:
    case n.exprKind
    of TrueC, FalseC:
      skip n
      result = true
    of SufC:
      # (suf LIT "suffix")
      skip n
      result = true
    of NotC, AndC, OrC, EqC, NeqC, LeC, LtC, AddC, SubC, MulC, NegC,
       ShlC, ShrC, BitandC, BitorC, BitxorC, BitnotC:
      result = true
      n.into:
        while n.hasMore:
          if not pureCondImpl(n, syms): result = false
    of ConvC, CastC:
      # (conv TYPE X) — the type subtree carries type symbols, not values.
      result = true
      n.into:
        if n.hasMore: skip n                  # the target type
        while n.hasMore:
          if not pureCondImpl(n, syms): result = false
    of DotC:
      # (dot BASE field depth) — a field read of a named slot. The BASE spine
      # must itself be pure (a symbol / nested dot / conv); the field symbol
      # and depth are metadata, not values.
      result = true
      n.into:
        if n.hasMore:
          if not pureCondImpl(n, syms): result = false
        while n.hasMore: skip n
    else:
      # calls, deref/pat/at (may trap or read mutable memory behind a pointer),
      # div/mod (trap), addr, constructors, `ovf`, …: not hoistable.
      skip n
      result = false
  else:
    inc n
    result = false

proc pureCond(c: Cursor; syms: var seq[SymId]): bool =
  var n = c
  result = pureCondImpl(n, syms)

# ── loop-body effect scan ────────────────────────────────────────────────────

proc slotRoot(c: Cursor): SymId =
  ## The named slot an lvalue writes, or SymId(0) for a through-pointer or
  ## unmodelled destination (the deref-aware root, mirroring the inliner's
  ## `slotRootOf`).
  result = SymId(0)
  var n = c
  while true:
    case n.kind
    of Symbol: return n.symId
    of TagLit:
      case n.exprKind
      of DerefC, PatC: return SymId(0)
      of DotC, AtC: inc n
      of ConvC, CastC:
        inc n; skip n                          # past the type
      else: return SymId(0)
    else: return SymId(0)

proc scanEffects(c: Cursor; assigned: var HashSet[SymId];
                 opaque: var bool) =
  ## Collect the symbols the subtree assigns; set `opaque` when it could write
  ## memory the scan cannot attribute to a named slot (through-pointer store,
  ## any call/instr) — `jtrue` writes its listed flags, so those count as
  ## assignments, not as opaque.
  if c.kind != TagLit: return
  case c.stmtKind
  of AsgnS, StoreS:
    var dst = child0(c)
    if c.stmtKind == StoreS: skip dst          # (store value dest)
    let s = slotRoot(dst)
    if s == SymId(0): opaque = true
    else: assigned.incl s
  of VarS, ConstS, GvarS, TvarS, MflagS, VflagS:
    # A declaration INSIDE the loop re-initializes its symbol every iteration —
    # a loop-local existence is per-iteration by construction, so its value can
    # never be loop-invariant. (Missing this hoisted an enum-range check whose
    # `ii` was the loop body's own conversion temp: the hoisted condition read
    # a variable that did not exist yet.)
    let d = child0(c)
    if d.kind == SymbolDef: assigned.incl d.symId
  of JtrueS:
    var n = c
    n.loopInto:
      if n.kind == Symbol: assigned.incl n.symId
      skip n
    return
  else: discard
  if c.exprKind in {CallC, InstrC} or c.stmtKind in {CallS, InstrS}:
    opaque = true
  var n = c
  n.loopInto:
    scanEffects(n, assigned, opaque)
    skip n

proc collectAddrTaken(c: Cursor; acc: var HashSet[SymId]) =
  if c.kind != TagLit: return
  if c.exprKind in AddrKinds:
    let s = slotRoot(child0(c))
    if s != SymId(0): acc.incl s
  var n = c
  n.loopInto:
    collectAddrTaken(n, acc)
    skip n

# ── candidate discovery ──────────────────────────────────────────────────────

proc subtreeSpan(orig: ptr TokenBuf; n: Cursor): int =
  let a = cursorToPosition(orig[], n)
  var e = n
  skip e
  cursorToPosition(orig[], e) - a

proc invariantConds(c: var Context; n: Cursor;
                    assigned: HashSet[SymId]; opaque: bool;
                    conds: var seq[int]) =
  ## Every elif condition under `n` (not descending into nested whiles) that is
  ## pure and invariant w.r.t. the enclosing loop, as orig positions.
  if n.kind != TagLit: return
  if n.stmtKind == WhileS: return              # inner loop: its own candidate
  if n.stmtKind == IfS:
    var it = n
    it.loopInto:
      if it.substructureKind == ElifU:
        let cond = child0(it)
        if not (cond.kind == TagLit and cond.exprKind in {TrueC, FalseC}):
          var syms: seq[SymId] = @[]
          if pureCond(cond, syms) and syms.len > 0:
            var ok = true
            for s in syms:
              if s in assigned: ok = false
              elif (s in c.procAddrTaken or not isLocalName(symNameOf(c, s))) and
                   opaque: ok = false
            if ok:
              conds.add cursorToPosition(c.orig[], cond)
        # a nested if inside this elif's body is caught by the recursion below
      skip it
  var it = n
  it.loopInto:
    invariantConds(c, it, assigned, opaque, conds)
    skip it

proc scanWhiles(c: var Context; n: Cursor; cands: var seq[Candidate]) =
  ## Find every unswitchable loop (innermost matches first — a nested while is
  ## scanned by the generic recursion below and its ifs are invisible to the
  ## enclosing loop's `invariantConds`).
  if n.kind != TagLit: return
  if n.stmtKind == WhileS and subtreeSpan(c.orig, n) <= MaxUnswitchSpan:
    var assigned = initHashSet[SymId]()
    var opaque = false
    scanEffects(n, assigned, opaque)
    var conds: seq[int] = @[]
    var body = n
    body.into:
      if body.hasMore: skip body               # the loop condition
      while body.hasMore:
        invariantConds(c, body, assigned, opaque, conds)
        skip body
    # PROFITABILITY: only split on a condition the loop tests at least TWICE
    # (the inlined-accessor shape — e.g. the SSO test, four times per char in
    # `addSymbol`). A once-tested invariant would be hoisted too, but each
    # unswitch DOUBLES the loop and the rounds compose: on real modules the
    # unrestricted rule tripled `addSymbol` for single-occurrence conditions
    # with no repeated test to delete. Pick the most-repeated condition.
    var bestPos = -1
    var bestCount = 1
    for i in 0 ..< conds.len:
      let ci = cursorAt(c.orig[], conds[i])
      var cnt = 1
      for j in i + 1 ..< conds.len:
        if sameTree(ci, cursorAt(c.orig[], conds[j])): inc cnt
      if cnt > bestCount:
        bestCount = cnt
        bestPos = conds[i]
    if bestPos >= 0:
      cands.add Candidate(whilePos: cursorToPosition(c.orig[], n),
                          condPos: bestPos)
  var it = n
  it.loopInto:
    scanWhiles(c, it, cands)
    skip it

# ── specialized duplication ──────────────────────────────────────────────────

proc collectDefs(n: Cursor; c: var Context; rename: var Table[SymId, string]) =
  ## Mint a fresh symbol NAME for every SymbolDef in the subtree (labels +
  ## locals of the second copy must not collide with the first); the name is
  ## interned by `addSymDef`/`addSymUse` at emission.
  var it = n
  case it.kind
  of SymbolDef:
    if not rename.hasKey(it.symId):
      inc c.counter
      rename[it.symId] = "`us" & $c.counter & "." & c.suffix
  of TagLit:
    it.loopInto:
      collectDefs(it, c, rename)
      skip it
  else: discard

proc emitSpec(c: var Context; dest: var TokenBuf; n: Cursor; cond: Cursor;
              condTrue: bool; rename: Table[SymId, string])

proc emitSpecChildren(c: var Context; dest: var TokenBuf; n: Cursor;
                      cond: Cursor; condTrue: bool;
                      rename: Table[SymId, string]) =
  var it = n
  it.loopInto:
    emitSpec(c, dest, it, cond, condTrue, rename)
    skip it

proc emitSpec(c: var Context; dest: var TokenBuf; n: Cursor; cond: Cursor;
              condTrue: bool; rename: Table[SymId, string]) =
  ## Copy subtree `n`, renaming defs/uses per `rename` and folding every `if`
  ## one of whose elif conditions is structurally `cond` to the `condTrue`
  ## branch shape.
  case n.kind
  of TagLit:
    if n.stmtKind == IfS:
      # does any elif carry the hoisted condition?
      var matchIdx = -1
      block probeElifs:
        var idx = 0
        var it = n
        it.loopInto:
          if it.substructureKind == ElifU and matchIdx < 0:
            if sameTree(child0(it), cond): matchIdx = idx
          inc idx
          skip it
      if matchIdx < 0:
        let li = rawLineInfo(n)
        dest.openTag n.cursorTagId
        if li.isValid: dest.appendLineInfo li
        emitSpecChildren(c, dest, n, cond, condTrue, rename)
        dest.closeTag()
        return
      # rebuild the if with the matched elif resolved
      if condTrue:
        # earlier elifs keep guarding; the matched elif becomes the final else;
        # everything after it is unreachable. With no earlier elif the branch
        # body splices directly.
        var branchBody = createTokenBuf(32, dest.pool, dest.tags)
        block takeBody:
          var idx = 0
          var it = n
          it.loopInto:
            if idx == matchIdx:
              var b = it
              b.into:
                if b.hasMore: skip b           # the condition
                while b.hasMore:
                  emitSpec(c, branchBody, b, cond, condTrue, rename)
                  skip b
            inc idx
            skip it
        if matchIdx == 0:
          # the whole if reduces to the branch body (already one (stmts …)
          # per elif shape; several children still splice legally)
          dest.addBufferSamePool branchBody
        else:
          dest.openTag n.cursorTagId
          var idx = 0
          var it = n
          it.loopInto:
            if idx < matchIdx and it.substructureKind == ElifU:
              emitSpec(c, dest, it, cond, condTrue, rename)
            inc idx
            skip it
          dest.openTag TagId(ord(ElseTagId))
          dest.addBufferSamePool branchBody
          dest.closeTag()
          dest.closeTag()
      else:
        # drop the matched elif; keep everything else in order
        var kept = 0
        var emitted = createTokenBuf(64, dest.pool, dest.tags)
        block keepOthers:
          var idx = 0
          var it = n
          it.loopInto:
            if idx != matchIdx:
              if it.substructureKind == ElifU: inc kept
              emitSpec(c, emitted, it, cond, condTrue, rename)
            inc idx
            skip it
        if kept > 0:
          let li = rawLineInfo(n)
          dest.openTag n.cursorTagId
          if li.isValid: dest.appendLineInfo li
          dest.addBufferSamePool emitted
          dest.closeTag()
        else:
          # no guards left: splice the else bodies (if an else exists)
          var it = n
          it.loopInto:
            if it.substructureKind == ElseU:
              var b = it
              b.into:
                while b.hasMore:
                  emitSpec(c, dest, b, cond, condTrue, rename)
                  skip b
            skip it
      return
    let li = rawLineInfo(n)
    dest.openTag n.cursorTagId
    if li.isValid: dest.appendLineInfo li
    emitSpecChildren(c, dest, n, cond, condTrue, rename)
    dest.closeTag()
  of Symbol:
    if rename.hasKey(n.symId): dest.addSymUse rename.getOrDefault(n.symId)
    else: dest.addSubtree n
  of SymbolDef:
    if rename.hasKey(n.symId): dest.addSymDef rename.getOrDefault(n.symId)
    else: dest.addSubtree n
  else:
    dest.addSubtree n                          # any literal/dot leaf

proc applyCandidate(c: var Context; cand: Candidate): TokenBuf =
  let whileCur = cursorAt(c.orig[], cand.whilePos)
  let cond = cursorAt(c.orig[], cand.condPos)
  var repl = createTokenBuf(256, c.orig[].pool, c.orig[].tags)
  repl.openTag TagId(ord(IfTagId))
  block trueArm:
    repl.openTag TagId(ord(ElifTagId))
    repl.addSubtree cond
    repl.openTag TagId(ord(StmtsTagId))
    let empty = initTable[SymId, string]()
    emitSpec(c, repl, whileCur, cond, condTrue = true, empty)
    repl.closeTag()
    repl.closeTag()
  block falseArm:
    repl.openTag TagId(ord(ElseTagId))
    repl.openTag TagId(ord(StmtsTagId))
    var rename = initTable[SymId, string]()
    collectDefs(whileCur, c, rename)
    emitSpec(c, repl, whileCur, cond, condTrue = false, rename)
    repl.closeTag()
    repl.closeTag()
  repl.closeTag()
  var ps = initPatchset(c.orig)
  ps.addSubst(cand.whilePos, cursorAt(repl, 0))
  result = ps.apply()

# ── public entry ─────────────────────────────────────────────────────────────

proc runUnswitch*(buf: var TokenBuf; suffix = "us"): int {.discardable.} =
  ## Fixpoint: unswitch one loop per round (the innermost candidate), rebuild,
  ## rescan. Returns the number of loops unswitched.
  result = 0
  var rounds = 0
  var minted = 0
  while rounds < MaxRounds:
    inc rounds
    var c = Context(orig: addr buf, suffix: suffix, counter: minted,
                    procAddrTaken: initHashSet[SymId]())
    block:
      let root = beginRead(buf)
      collectAddrTaken(root, c.procAddrTaken)
    var cands: seq[Candidate] = @[]
    block:
      let root = beginRead(buf)
      scanWhiles(c, root, cands)
    if cands.len == 0:
      break
    var newBuf = applyCandidate(c, cands[0])
    minted = c.counter
    buf = ensureMove(newBuf)
    inc result

# ── self-tests ───────────────────────────────────────────────────────────────

when isMainModule:
  import std / strutils

  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  proc unswitched(src: string): string =
    var b = parse(src)
    runUnswitch(b, "t")
    toString(b)

  proc canon(src: string): string =
    var b = parse(src)
    toString(b)

  block invariant_if_hoisted:
    # `flag.0.M` is never assigned in the loop and tested twice: the if must be
    # hoisted and the test must appear exactly once afterwards (both copies fold
    # every matching inner if away).
    let got = unswitched(
      "(stmts (while (lt i.0.M n.0.M) (stmts " &
      "(if (elif flag.0.M (stmts (asgn a.0.M 1))) (else (stmts (asgn a.0.M 2)))) " &
      "(if (elif flag.0.M (stmts (asgn b.0.M 1))) (else (stmts (asgn b.0.M 2)))) " &
      "(asgn i.0.M (add (i 64) i.0.M 1)))))")
    doAssert got.count("flag.0.M") == 1, got
    doAssert got.count("(while") == 2, got

  block single_occurrence_left_alone:
    # invariant but tested only once: doubling the loop deletes no repeated
    # test, so the profitability gate declines
    let src =
      "(stmts (while (lt i.0.M n.0.M) (stmts " &
      "(if (elif flag.0.M (stmts (asgn a.0.M 1))) (else (stmts (asgn a.0.M 2)))) " &
      "(asgn i.0.M (add (i 64) i.0.M 1)))))"
    doAssert unswitched(src) == canon(src)

  block variant_condition_left_alone:
    # the condition's symbol is assigned inside the loop
    let src =
      "(stmts (while (lt i.0.M n.0.M) (stmts " &
      "(if (elif flag.0.M (stmts (asgn flag.0.M (false))))) " &
      "(asgn i.0.M (add (i 64) i.0.M 1)))))"
    doAssert unswitched(src) == canon(src)

  block call_in_loop_blocks_addr_taken_sym:
    # `flag` is address-taken in the proc and the loop contains a call: the
    # callee could rewrite it through the captured pointer.
    let src =
      "(stmts (var :p.0.M . (ptr (bool)) (haddr flag.0.M)) " &
      "(while (lt i.0.M n.0.M) (stmts " &
      "(if (elif flag.0.M (stmts (asgn a.0.M 1))) (else (stmts (asgn a.0.M 2)))) " &
      "(if (elif flag.0.M (stmts (asgn b.0.M 1))) (else (stmts (asgn b.0.M 2)))) " &
      "(call f.0.M) " &
      "(asgn i.0.M (add (i 64) i.0.M 1)))))"
    doAssert unswitched(src) == canon(src)

  block call_in_loop_ok_for_plain_local:
    # same loop, but the condition symbol is a LOCAL (single-dot name) whose
    # address is never taken: a call cannot touch it, so the unswitch is legal.
    let got = unswitched(
      "(stmts (while (lt i.0 n.0) (stmts " &
      "(if (elif flag.0 (stmts (asgn a.0 1))) (else (stmts (asgn a.0 2)))) " &
      "(if (elif flag.0 (stmts (asgn b.0 1))) (else (stmts (asgn b.0 2)))) " &
      "(call f.0.M) " &
      "(asgn i.0 (add (i 64) i.0 1)))))")
    doAssert got.count("flag.0 ") + got.count("flag.0\n") >= 1
    doAssert got.count("(while") == 2, got

  block call_in_loop_blocks_global_cond:
    # a GLOBAL condition symbol (module-suffixed name) with a call in the loop:
    # the callee may assign the global directly, so decline.
    let src =
      "(stmts (while (lt i.0.M n.0.M) (stmts " &
      "(if (elif flag.0.M (stmts (asgn a.0.M 1))) (else (stmts (asgn a.0.M 2)))) " &
      "(if (elif flag.0.M (stmts (asgn b.0.M 1))) (else (stmts (asgn b.0.M 2)))) " &
      "(call f.0.M) " &
      "(asgn i.0.M (add (i 64) i.0.M 1)))))"
    doAssert unswitched(src) == canon(src)

  block duplicate_defs_renamed:
    # a local declared inside the loop must not collide between the two copies
    let got = unswitched(
      "(stmts (while (lt i.0.M n.0.M) (stmts " &
      "(var :t.1.M . (i 64) 0) " &
      "(if (elif flag.0.M (stmts (asgn t.1.M 1))) (else (stmts (asgn t.1.M 2)))) " &
      "(if (elif flag.0.M (stmts (asgn u.1.M 1))) (else (stmts (asgn u.1.M 2)))) " &
      "(asgn i.0.M (add (i 64) i.0.M t.1.M)))))")
    doAssert got.count(":t.1.M") == 1, got
    doAssert "`us" in got, got

  block loop_local_condition_left_alone:
    # the condition reads a symbol DECLARED inside the loop (per-iteration
    # existence): never invariant, even though nothing `asgn`s it
    let src =
      "(stmts (while (lt i.0 n.0) (stmts " &
      "(var :t.1 . (i 64) (add (i 64) i.0 1)) " &
      "(if (elif (le t.1 14) (stmts (asgn a.0 1))) (else (stmts (asgn a.0 2)))) " &
      "(if (elif (le t.1 14) (stmts (asgn b.0 1))) (else (stmts (asgn b.0 2)))) " &
      "(asgn i.0 (add (i 64) i.0 1)))))"
    doAssert unswitched(src) == canon(src)

  block repeated_condition_deduped:
    # the SAME test twice in the body: unswitching once removes both
    let got = unswitched(
      "(stmts (while (lt i.0.M n.0.M) (stmts " &
      "(if (elif (le k.0.M 14) (stmts (asgn a.0.M 1))) (else (stmts (asgn a.0.M 2)))) " &
      "(if (elif (le k.0.M 14) (stmts (asgn b.0.M 1))) (else (stmts (asgn b.0.M 2)))) " &
      "(asgn i.0.M (add (i 64) i.0.M 1)))))")
    doAssert got.count("(le k.0.M 14)") == 1, got

  echo "unswitch.nim: all self-tests passed"

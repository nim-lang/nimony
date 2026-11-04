#
#
#           NIFC Constant Folding (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Folds what is known at compile time and deletes the code that folding
## proves unreachable:
##
## * **Loads from constant aggregates.** `(dot (at things 1) kind 0)`, where
##   `things` is a module-level `(const :things . T (aconstr …))`, becomes the
##   literal the initializer has there. The access path is any chain of `at`
##   (literal index) and `dot` (inheritance depth 0) rooted at the const. A
##   scalar result folds in any read position; an aggregate one (`(oconstr Vec3
##   …)`) only as a `var` initializer or an `asgn` source, where SROA takes it
##   apart. A path under `addr`/`haddr` or on the left of an `asgn` is left
##   alone, and so is one whose result is not made of literals only.
## * **Literal comparisons and boolean operators**: `(le 0 1)` → `(true)`,
##   `(not (true))` → `(false)`, `(and (true) (false))` → `(false)`.
## * **`if` with literal conditions.** A `(false)` arm is deleted; the first
##   `(true)` arm becomes the `else` and the arms after it are deleted; an `if`
##   left with nothing but its unconditional arm becomes `(scope ARM)`.
## * **Dead statements after `jmp`/`ret`.** In a statement list, everything
##   after an unconditional jump up to the next label that some remaining jump
##   still targets is unreachable and is deleted (a `var` declaration stays, as
##   a declaration without initializer, because later statements may name it).
##
## Why this pass exists: unrolling and inlining hand the back end literal
## indices — `objectIntersect(i, ray)` unrolled over `i` reads `things[0]`,
## `things[1]`, `things[2]` — but the inlined bounds check
## `(if (elif (not (le 0 i)) (stmts (jmp Lfail))))` stays a test, keeps the
## failure path alive, and the index stays a variable. Folding the check, then
## the dead failure path, lets copy propagation make the index literal; then the
## loads fold and the per-object `kind` test with them.
##
## Jumps are forward and scoped (multi-join IR): a jump's target is a label
## AFTER it in an enclosing list. So one forward walk that subtracts every jump
## it deletes from its target's count knows, when it reaches a label, whether
## anything still jumps there.

import std / [assertions, tables]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore
import ".." / ".." / "lib" / nifcdecl        # stmtKind/exprKind/substructureKind
import ".." / ".." / "models" / tags         # tag ids for synthesis

type
  ConstTable* = Table[SymId, Cursor]   ## const symbol -> its initializer

  ValKind = enum vNone, vInt, vBool, vConst
  Val = object
    kind: ValKind
    i: int64
    b: bool
    c: Cursor                ## vConst (and vInt read from a const): the subtree

  Context = object
    consts: ptr ConstTable
    jumps: Table[SymId, int] ## remaining jumps per label
    noDce: bool              ## flag-based control flow present: no dead code elim
    changed: bool

proc child0(c: Cursor): Cursor {.inline.} =
  result = c
  inc result

# ── the module's constant aggregates ─────────────────────────────────────────

proc collectConsts*(module: Cursor): ConstTable =
  ## The top-level `(const :C pragmas T INIT)` whose INIT is an array or object
  ## constructor. The cursors point into the module buffer, which must outlive
  ## every `runConstFold` that uses the table.
  result = initTable[SymId, Cursor]()
  if module.kind != TagLit: return
  var n = module
  n.loopInto:
    if n.kind == TagLit and n.stmtKind == ConstS:
      var it = child0(n)
      if it.kind == SymbolDef:
        let s = it.symId
        skip it                                  # name
        skip it                                  # pragmas
        skip it                                  # type
        if it.kind == TagLit and it.exprKind in {AconstrC, OconstrC}:
          result[s] = it
    skip n

# ── evaluation ───────────────────────────────────────────────────────────────

proc isScalarLit(n: Cursor): bool =
  case n.kind
  of IntLit, UIntLit, FloatLit, CharLit: result = true
  of TagLit:
    case n.exprKind
    of TrueC, FalseC, NilC, InfC, NeginfC, NanC: result = true
    of SufC: result = child0(n).kind in {IntLit, UIntLit, FloatLit, CharLit}
    else: result = false
  else: result = false

proc isLiteralTree(n: Cursor): bool =
  ## Made of literals only (a constructor's type and a `kv`'s field name aside):
  ## copying it into a body copies no reference to anything.
  if isScalarLit(n): return true
  if n.kind != TagLit: return false
  case n.exprKind
  of AconstrC, OconstrC:
    var first = true
    var it = n
    it.loopInto:
      if first:
        first = false                            # the type
      elif it.kind == TagLit and it.substructureKind == KvU:
        var v = child0(it)
        skip v                                   # the field
        if not isLiteralTree(v): return false
      elif not isLiteralTree(it):
        return false
      skip it
    result = true
  else: result = false

proc intOf(n: Cursor; v: var int64): bool =
  ## An integer literal, possibly `(suf LIT "…")`.
  case n.kind
  of IntLit:
    v = intVal(n)
    result = true
  of TagLit:
    if n.exprKind == SufC and child0(n).kind == IntLit:
      v = intVal(child0(n))
      result = true
    else: result = false
  else: result = false

proc constVal(n: Cursor): Val =
  ## A subtree of a const's initializer, read as a value.
  var i = 0'i64
  if intOf(n, i): result = Val(kind: vInt, i: i, c: n)
  elif n.kind == TagLit and n.exprKind in {TrueC, FalseC}:
    result = Val(kind: vBool, b: n.exprKind == TrueC, c: n)
  else: result = Val(kind: vConst, c: n)

proc eval(c: Context; n: Cursor): Val

proc indexOf(c: Context; n: Cursor; v: var int64): bool =
  if n.kind == TagLit and n.exprKind == ConvC:
    var it = child0(n)
    skip it                                      # the type
    return indexOf(c, it, v)
  let x = eval(c, n)
  result = x.kind == vInt
  if result: v = x.i

proc constBase(c: Context; n: Cursor; base: var Cursor): bool =
  ## Is `n` a const symbol or a path into one that ends at a constructor?
  if n.kind == Symbol:
    if c.consts[].hasKey(n.symId):
      base = c.consts[].getOrDefault(n.symId)
      return true
    return false
  let b = eval(c, n)
  result = b.kind == vConst and b.c.kind == TagLit
  if result: base = b.c

proc evalAt(c: Context; n: Cursor): Val =
  result = Val(kind: vNone)
  var it = child0(n)
  var base = default(Cursor)
  if not constBase(c, it, base): return
  if base.exprKind != AconstrC: return
  skip it
  var idx = 0'i64
  if not indexOf(c, it, idx) or idx < 0: return
  var count = 0'i64
  var b2 = base
  b2.loopInto:
    if count == idx + 1:                         # child 0 is the type
      return constVal(b2)
    inc count
    skip b2

proc evalDot(c: Context; n: Cursor): Val =
  result = Val(kind: vNone)
  var it = child0(n)
  var base = default(Cursor)
  if not constBase(c, it, base): return
  if base.exprKind != OconstrC: return
  skip it
  if it.kind != Symbol: return
  let field = it.symId
  skip it
  var depth = 0'i64
  if not intOf(it, depth) or depth != 0: return
  var first = true
  var kv = base
  kv.loopInto:
    if first:
      first = false                              # the type
    elif kv.kind == TagLit and kv.substructureKind == KvU:
      let f = child0(kv)
      if f.kind == Symbol and f.symId == field:
        var v = f
        skip v
        return constVal(v)
    skip kv

proc eval(c: Context; n: Cursor): Val =
  result = Val(kind: vNone)
  case n.kind
  of IntLit:
    result = Val(kind: vInt, i: intVal(n))
  of TagLit:
    case n.exprKind
    of TrueC: result = Val(kind: vBool, b: true)
    of FalseC: result = Val(kind: vBool, b: false)
    of SufC:
      var i = 0'i64
      if intOf(n, i): result = Val(kind: vInt, i: i)
    of AtC:
      if c.consts[].len > 0: result = evalAt(c, n)
    of DotC:
      if c.consts[].len > 0: result = evalDot(c, n)
    of EqC, NeqC, LeC, LtC:
      let a = child0(n)
      var b = a
      skip b
      let x = eval(c, a)
      if x.kind notin {vInt, vBool}: return
      let y = eval(c, b)
      if y.kind != x.kind: return
      if x.kind == vInt:
        let r = case n.exprKind
                of EqC: x.i == y.i
                of NeqC: x.i != y.i
                of LeC: x.i <= y.i
                else: x.i < y.i
        result = Val(kind: vBool, b: r)
      elif n.exprKind in {EqC, NeqC}:
        result = Val(kind: vBool, b: (x.b == y.b) == (n.exprKind == EqC))
    of NotC:
      let x = eval(c, child0(n))
      if x.kind == vBool: result = Val(kind: vBool, b: not x.b)
    of AndC, OrC:
      let a = child0(n)
      var b = a
      skip b
      let x = eval(c, a)
      if x.kind != vBool: return
      let y = eval(c, b)
      if y.kind != vBool: return
      result = Val(kind: vBool, b: (if n.exprKind == AndC: x.b and y.b else: x.b or y.b))
    else: discard
  else: discard

# ── jumps ────────────────────────────────────────────────────────────────────

proc countJumps(c: var Context; n: Cursor) =
  if n.kind != TagLit: return
  case n.stmtKind
  of JmpS:
    let t = child0(n)
    if t.kind == Symbol: c.jumps.mgetOrPut(t.symId, 0) += 1
    return
  of JtrueS, MflagS, VflagS, IteS, ItecS: c.noDce = true
  else: discard
  var it = n
  it.loopInto:
    countJumps(c, it)
    skip it

proc dropJumps(c: var Context; n: Cursor) =
  ## `n` is deleted: its jumps no longer reach their labels.
  if n.kind != TagLit: return
  if n.stmtKind == JmpS:
    let t = child0(n)
    if t.kind == Symbol: c.jumps.mgetOrPut(t.symId, 0) -= 1
    return
  var it = n
  it.loopInto:
    dropJumps(c, it)
    skip it

proc hasLiveLabel(c: Context; n: Cursor): bool =
  if n.kind != TagLit: return false
  if n.stmtKind == LabS:
    let d = child0(n)
    return d.kind == SymbolDef and c.jumps.getOrDefault(d.symId, 0) > 0
  result = false
  var it = n
  it.loopInto:
    if hasLiveLabel(c, it): return true
    skip it

# ── emission ─────────────────────────────────────────────────────────────────

proc openLike(dest: var TokenBuf; n: Cursor) =
  let li = rawLineInfo(n)
  dest.openTag n.cursorTagId
  if li.isValid: dest.appendLineInfo li

proc addBool(dest: var TokenBuf; b: bool) =
  dest.openTag TagId(ord(if b: TrueTagId else: FalseTagId))
  dest.closeTag()

proc emitAny(c: var Context; dest: var TokenBuf; n: Cursor)
proc emitChildren(c: var Context; dest: var TokenBuf; n: Cursor)

proc emitE(c: var Context; dest: var TokenBuf; n: Cursor; aggOk: bool) =
  ## An expression; `aggOk` when an aggregate value may replace it.
  if n.kind == TagLit:
    case n.exprKind
    of AddrC, HaddrC:
      dest.addSubtree n                          # an address of a const stays one
      return
    of EqC, NeqC, LeC, LtC, NotC, AndC, OrC:
      let v = eval(c, n)
      if v.kind == vBool:
        dest.addBool v.b
        c.changed = true
        return
    of AtC, DotC:
      let v = eval(c, n)
      if v.kind != vNone and
          (isScalarLit(v.c) or (aggOk and isLiteralTree(v.c))):
        dest.addSubtree v.c
        c.changed = true
        return
    else: discard
    emitChildren(c, dest, n)
  else:
    dest.addSubtree n

proc emitChildren(c: var Context; dest: var TokenBuf; n: Cursor) =
  dest.openLike n
  var it = n
  it.loopInto:
    emitAny(c, dest, it)
    skip it
  dest.closeTag()

proc emitList(c: var Context; dest: var TokenBuf; n: Cursor) =
  ## A `stmts`/`scope` list: dead statements after a jump are deleted.
  dest.openLike n
  var dead = false
  var it = n
  it.loopInto:
    if dead:
      if it.kind == TagLit and it.stmtKind == LabS:
        let d = child0(it)
        if d.kind == SymbolDef and c.jumps.getOrDefault(d.symId, 0) > 0:
          dead = false
      elif hasLiveLabel(c, it):
        dead = false
    if dead:
      c.changed = true
      if it.kind == TagLit and it.stmtKind == VarS:
        # keep the declaration: a later (reachable) statement may name it
        var d = child0(it)
        dest.openLike it
        for _ in 0 ..< 3:                        # name, pragmas, type
          dest.addSubtree d
          skip d
        dest.addDotToken()
        dest.closeTag()
        dropJumps(c, d)
      else:
        dropJumps(c, it)
    else:
      emitAny(c, dest, it)
      if not c.noDce and it.kind == TagLit and it.stmtKind in {JmpS, RetS}:
        dead = true
    skip it
  dest.closeTag()

proc emitIf(c: var Context; dest: var TokenBuf; n: Cursor) =
  var arms: seq[(Cursor, bool)] = @[]            # (arm, unconditional)
  var folded = false
  var done = false
  var arm = n
  arm.loopInto:
    if done:
      dropJumps(c, arm)
      folded = true
    elif arm.kind == TagLit and arm.substructureKind == ElifU:
      let v = eval(c, child0(arm))
      if v.kind == vBool:
        folded = true
        if v.b:
          arms.add (arm, true)
          done = true
        else:
          dropJumps(c, arm)
      else:
        arms.add (arm, false)
    else:
      arms.add (arm, true)                       # else
      done = true
    skip arm
  if not folded:
    emitChildren(c, dest, n)
    return
  c.changed = true
  if arms.len == 0: return
  proc body(a: Cursor; uncond: bool): Cursor =
    result = child0(a)
    if a.substructureKind == ElifU: skip result  # the condition
  if arms[0][1]:
    dest.openTag TagId(ord(ScopeTagId))
    emitAny(c, dest, body(arms[0][0], true))
    dest.closeTag()
    return
  dest.openLike n
  for (a, uncond) in arms:
    if uncond:
      dest.openTag TagId(ord(ElseTagId))
      emitAny(c, dest, body(a, true))
      dest.closeTag()
    else:
      emitChildren(c, dest, a)
  dest.closeTag()

proc emitAny(c: var Context; dest: var TokenBuf; n: Cursor) =
  if n.kind != TagLit:
    dest.addSubtree n
    return
  case n.stmtKind
  of StmtsS, ScopeS:
    emitList(c, dest, n)
  of IfS:
    emitIf(c, dest, n)
  of VarS, GvarS, TvarS, ConstS:
    # (var :x pragmas T INIT)
    dest.openLike n
    var it = child0(n)
    for _ in 0 ..< 3:
      dest.addSubtree it
      skip it
    emitE(c, dest, it, aggOk = true)
    dest.closeTag()
  of AsgnS:
    dest.openLike n
    var it = child0(n)
    dest.addSubtree it                           # the target is not a read
    skip it
    emitE(c, dest, it, aggOk = true)
    dest.closeTag()
  else:
    if n.stmtKind == NoStmt and n.exprKind != NoExpr:
      emitE(c, dest, n, aggOk = false)
    else:
      emitChildren(c, dest, n)

# ── public entry ─────────────────────────────────────────────────────────────

proc runConstFold*(buf: var TokenBuf; consts: ptr ConstTable): bool =
  ## Folds the body in `buf` in place. Returns whether anything changed.
  var c = Context(consts: consts, jumps: initTable[SymId, int]())
  countJumps(c, beginRead(buf))
  var dest = createTokenBuf(buf.len, buf.pool, buf.tags)
  emitAny(c, dest, beginRead(buf))
  result = c.changed
  if result: buf = ensureMove(dest)

# ── self-tests ───────────────────────────────────────────────────────────────

when isMainModule:
  import std / strutils

  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100, sharedTags = createLengTagPool())

  const Things =
    "(const :things.0 . T.0 (aconstr T.0 " &
    "(oconstr Thing.0 (kv kind.0 (suf 0 \"i64\")) (kv center.0 (oconstr V.0 (kv x.0 0.0) (kv y.0 1.0))) (kv name.0 nm.0)) " &
    "(oconstr Thing.0 (kv kind.0 (suf 1 \"i64\")) (kv center.0 (oconstr V.0 (kv x.0 -1.0) (kv y.0 0.5))) (kv name.0 nm.1))))"

  proc folded(body: string): string =
    var m = parse("(stmts " & Things & ")")
    let consts = collectConsts(beginRead(m))
    var b = createTokenBuf(64, m.pool, m.tags)
    var src = parseFromBuffer(body, "M", 100, sharedTags = m.tags, sharedPool = m.pool)
    var r = beginRead(src)
    b.addSubtree r
    discard runConstFold(b, addr consts)
    result = toString(b).multiReplace(("\n", ""), (" ", ""))

  proc same(body: string): bool =
    var b = parse(body)
    result = folded(body) == toString(b).multiReplace(("\n", ""), (" ", ""))

  block scalar_loads:
    let got = folded("(stmts (var :k.0 . (i 64) (dot (at things.0 1) kind.0 0)) " &
                     "(asgn f.0 (dot (dot (at things.0 0) center.0 0) y.0 0)))")
    doAssert "(suf 1 \"i64\")".replace(" ", "") in got, got
    doAssert "(asgn f.0 1.0)".replace(" ", "") in got, got
    doAssert "things.0".replace(" ", "") notin got, got

  block aggregate_only_as_initializer:
    let got = folded("(stmts (var :v.0 . V.0 (dot (at things.0 1) center.0 0)) " &
                     "(call g.0 (dot (at things.0 1) center.0 0)))")
    doAssert "(var :v.0 . V.0 (oconstr V.0".replace(" ", "") in got, got
    doAssert "(call g.0 (dot (at things.0 1) center.0 0))".replace(" ", "") in got, got

  block symbols_are_not_literals:
    doAssert same("(stmts (var :n.0 . N.0 (dot (at things.0 1) name.0 0)))")

  block non_literal_index_and_addr:
    doAssert same("(stmts (var :k.0 . (i 64) (dot (at things.0 i.0) kind.0 0)))")
    doAssert same("(stmts (var :p.0 . P.0 (haddr (at things.0 1))))")
    doAssert same("(stmts (var :k.0 . (i 64) (dot (at things.0 2) kind.0 0)))")

  block compare_through_const:
    let got = folded("(stmts (if (elif (eq (dot (at things.0 1) kind.0 0) 1) " &
                     "(stmts (call a.0))) (else (stmts (call b.0)))))")
    doAssert "(scope (stmts (call a.0)))".replace(" ", "") in got and "b.0".replace(" ", "") notin got, got

  block bounds_check_and_dead_failure_path:
    # the inliner's shape of `things[1]`'s check
    let got = folded(
      "(stmts (var :x.0 . (i 64) .) (scope " &
      "(if (elif (not (le 0 1)) (stmts (jmp L.0)))) (lab :A.0) " &
      "(if (elif (not (le 1 2)) (stmts (jmp L.0)))) " &
      "(scope (asgn x.0 1)) (jmp E.0) (lab :L.0) " &
      "(scope (stmts (asgn x.0 0) (call raise.0 1 0 2))) (lab :E.0)) " &
      "(call use.0 x.0))")
    doAssert "raise.0".replace(" ", "") notin got and "(if".replace(" ", "") notin got, got
    doAssert "(jmp L.0)".replace(" ", "") notin got and ":L.0".replace(" ", "") notin got, got
    doAssert "(asgn x.0 1)".replace(" ", "") in got and ":E.0".replace(" ", "") in got, got

  block live_label_ends_dead_region:
    let got = folded("(stmts (if (elif c.0 (stmts (jmp L.0)))) (ret 1) " &
                     "(call dead.0) (var :v.0 . (i 64) 5) (lab :L.0) (call live.0))")
    doAssert "dead.0".replace(" ", "") notin got and "live.0".replace(" ", "") in got, got
    doAssert "(var :v.0 . (i 64) .)".replace(" ", "") in got, got

  block nested_jump_out_of_dead_code:
    # the deleted region's own jump to M.0 must not keep M.0 alive
    let got = folded("(stmts (ret 0) (scope (jmp M.0)) (call d1.0) (lab :M.0) (call d2.0))")
    doAssert "d1.0".replace(" ", "") notin got and "d2.0".replace(" ", "") notin got, got

  block false_arm_dropped_true_arm_becomes_else:
    let got = folded("(stmts (if (elif c.0 (stmts (call a.0))) (elif (lt 2 1) (stmts (call b.0))) " &
                     "(elif (lt 1 2) (stmts (call d.0))) (else (stmts (call e.0)))))")
    doAssert "b.0".replace(" ", "") notin got and "e.0".replace(" ", "") notin got, got
    doAssert "(else (stmts (call d.0)))".replace(" ", "") in got, got

  block unknown_condition_untouched:
    doAssert same("(stmts (if (elif (lt i.0 2) (stmts (call a.0)))) (call b.0))")

  echo "constfold.nim: all self-tests passed"

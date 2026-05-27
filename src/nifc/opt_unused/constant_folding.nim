#
#
#           NIFC Constant Folding
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Evaluate pure arithmetic / comparison / boolean expressions on
## literal operands at compile time and substitute the result.
##
## Complementary to `copy_propagation` — once copy-prop has inlined
## literal bindings into expression operands, constant folding reduces
## the now-fully-literal expressions to single tokens.
##
## ## What this pass folds
##
## - `(add T x y)`, `(sub T x y)`, `(mul T x y)` on `IntLit` operands
##   → an `IntLit` of the result.
## - `(neg T x)` on an `IntLit` operand → `IntLit -x`.
## - `(bitand|bitor|bitxor T x y)`, `(bitnot T x)`,
##   `(shl|shr T x y)` on `IntLit` operands → `IntLit`.
## - `(eq|neq|le|lt T x y)` on `IntLit` operands → `(true)` / `(false)`.
## - `(and x y)`, `(or x y)`, `(not x)` on bool nullary operands
##   → `(true)` / `(false)`.
##
## ## Bottom-up by construction
##
## `fold` recurses into operands first and returns a `FoldResult` value
## describing what (if anything) the subtree reduces to. The recursive
## call records patches for the operands, and on the way back up the
## parent uses those operands' results to potentially fold itself —
## so `(add T 1 (mul T 2 3))` reduces to `IntLit 7` in a single pass.
##
## Two-phase via the shared [patchsets](patchsets.nim) module, same
## structural shape as the other rewriting passes.

import std / [tables, sets, hashes, assertions]
include "../../lib" / nifprelude
import nifstreams, nifcursors
import ".." / nifc_model
import ".." / ".." / models / tags
import patchsets

type
  FoldKind = enum NoFold, FoldInt, FoldBool
  FoldResult = object
    case kind: FoldKind
    of NoFold: discard
    of FoldInt: i: int64
    of FoldBool: b: bool

  Context = object
    orig: ptr TokenBuf
    patchset: Patchset
    synth: seq[TokenBuf]

proc createContext(orig: ptr TokenBuf): Context =
  Context(orig: orig, patchset: initPatchset(orig), synth: @[])

# ---- synthesis -----------------------------------------------------------

proc addIntSynth(c: var Context; value: int64; info: PackedLineInfo): int =
  result = c.synth.len
  var buf = createTokenBuf(2)
  buf.addIntLit value, info
  c.synth.add buf

proc addBoolSynth(c: var Context; value: bool; info: PackedLineInfo): int =
  result = c.synth.len
  var buf = createTokenBuf(2)
  let tag = if value: TrueTagId else: FalseTagId
  buf.addParLe(TagId(ord(tag)), info)
  buf.addParRi()
  c.synth.add buf

proc recordInt(c: var Context; pos: int; value: int64; info: PackedLineInfo) =
  let idx = addIntSynth(c, value, info)
  c.patchset.addSubst(pos, cursorAt(c.synth[idx], 0))

proc recordBool(c: var Context; pos: int; value: bool; info: PackedLineInfo) =
  let idx = addBoolSynth(c, value, info)
  c.patchset.addSubst(pos, cursorAt(c.synth[idx], 0))

# ---- folding --------------------------------------------------------------

proc fold(c: var Context; n: var Cursor): FoldResult

proc foldBinaryTyped(c: var Context; n: var Cursor; op: NifcExpr;
                     pos: int; info: PackedLineInfo): FoldResult =
  result = FoldResult(kind: NoFold)
  var l, r: FoldResult
  n.into:
    if n.hasMore: skip n                       # type
    if n.hasMore: l = fold(c, n)
    if n.hasMore: r = fold(c, n)
    while n.hasMore: skip n
  if l.kind != FoldInt or r.kind != FoldInt: return
  let v = case op
    of AddC:    l.i + r.i
    of SubC:    l.i - r.i
    of MulC:    l.i * r.i
    of ShlC:    l.i shl r.i
    of ShrC:    l.i shr r.i
    of BitandC: l.i and r.i
    of BitorC:  l.i or r.i
    of BitxorC: l.i xor r.i
    else: 0
  recordInt(c, pos, v, info)
  result = FoldResult(kind: FoldInt, i: v)

proc foldUnaryTyped(c: var Context; n: var Cursor; op: NifcExpr;
                    pos: int; info: PackedLineInfo): FoldResult =
  result = FoldResult(kind: NoFold)
  var inner: FoldResult
  n.into:
    if n.hasMore: skip n                       # type
    if n.hasMore: inner = fold(c, n)
    while n.hasMore: skip n
  if inner.kind != FoldInt: return
  let v = case op
    of NegC:    -inner.i
    of BitnotC: not inner.i
    else: 0
  recordInt(c, pos, v, info)
  result = FoldResult(kind: FoldInt, i: v)

proc foldCmpTyped(c: var Context; n: var Cursor; op: NifcExpr;
                  pos: int; info: PackedLineInfo): FoldResult =
  result = FoldResult(kind: NoFold)
  var l, r: FoldResult
  n.into:
    if n.hasMore: skip n                       # type
    if n.hasMore: l = fold(c, n)
    if n.hasMore: r = fold(c, n)
    while n.hasMore: skip n
  if l.kind != FoldInt or r.kind != FoldInt: return
  let v = case op
    of EqC:  l.i == r.i
    of NeqC: l.i != r.i
    of LeC:  l.i <= r.i
    of LtC:  l.i <  r.i
    else: false
  recordBool(c, pos, v, info)
  result = FoldResult(kind: FoldBool, b: v)

proc foldBoolBin(c: var Context; n: var Cursor; op: NifcExpr;
                 pos: int; info: PackedLineInfo): FoldResult =
  result = FoldResult(kind: NoFold)
  var l, r: FoldResult
  n.into:
    if n.hasMore: l = fold(c, n)
    if n.hasMore: r = fold(c, n)
    while n.hasMore: skip n
  if l.kind != FoldBool or r.kind != FoldBool: return
  let v = case op
    of AndC: l.b and r.b
    of OrC:  l.b or r.b
    else: false
  recordBool(c, pos, v, info)
  result = FoldResult(kind: FoldBool, b: v)

proc foldNot(c: var Context; n: var Cursor;
             pos: int; info: PackedLineInfo): FoldResult =
  result = FoldResult(kind: NoFold)
  var inner: FoldResult
  n.into:
    if n.hasMore: inner = fold(c, n)
    while n.hasMore: skip n
  if inner.kind != FoldBool: return
  let v = not inner.b
  recordBool(c, pos, v, info)
  result = FoldResult(kind: FoldBool, b: v)

proc fold(c: var Context; n: var Cursor): FoldResult =
  result = FoldResult(kind: NoFold)
  case n.kind
  of IntLit:
    result = FoldResult(kind: FoldInt, i: pool.integers[n.intId])
    inc n
  of ParLe:
    let pos = cursorToPosition(c.orig[], n)
    let info = n.info
    let ek = n.exprKind
    case ek
    of TrueC:
      result = FoldResult(kind: FoldBool, b: true)
      skip n
    of FalseC:
      result = FoldResult(kind: FoldBool, b: false)
      skip n
    of AddC, SubC, MulC, ShlC, ShrC, BitandC, BitorC, BitxorC:
      result = foldBinaryTyped(c, n, ek, pos, info)
    of NegC, BitnotC:
      result = foldUnaryTyped(c, n, ek, pos, info)
    of EqC, NeqC, LeC, LtC:
      result = foldCmpTyped(c, n, ek, pos, info)
    of AndC, OrC:
      result = foldBoolBin(c, n, ek, pos, info)
    of NotC:
      result = foldNot(c, n, pos, info)
    else:
      # Not a foldable expression. Descend into children so any
      # nested foldable subtrees still get a chance.
      n.loopInto:
        discard fold(c, n)
  else:
    inc n

# ---- public entry --------------------------------------------------------

proc runConstantFolding*(buf: var TokenBuf) =
  ## Two-phase constant folding: walk `buf` bottom-up evaluating pure
  ## expressions on literal operands, recording substitute patches, then
  ## rebuild `buf` with each foldable expression replaced by its result.
  var ctx = createContext(addr buf)
  var n = beginRead(buf)
  while n.hasMore:
    discard fold(ctx, n)
  if not ctx.patchset.isEmpty:
    var newBuf = ctx.patchset.apply()
    buf = ensureMove(newBuf)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  import nifrender

  proc parse(src: string): TokenBuf =
    var stream = nifstreams.openFromBuffer(src, "M")
    result = fromStream(stream)

  template assertUnchanged(input: string) =
    var buf = parse(input)
    let before = render(buf)
    runConstantFolding buf
    assertRender(buf, before)

  discard pool.syms.getOrIncl("x.0.M")
  discard pool.syms.getOrIncl("y.0.M")
  discard pool.syms.getOrIncl("use.0.M")

  block add_two_literals:
    var buf = parse("(stmts (asgn x.0.M (add (i 32) 2 3)))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M 5))""")

  block nested_arithmetic:
    # (add T 1 (mul T 2 3)) → 7.
    var buf = parse("(stmts (asgn x.0.M (add (i 32) 1 (mul (i 32) 2 3))))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M 7))""")

  block sub_and_neg:
    # (sub T 10 (neg T 5)) → (sub T 10 -5) → 15.
    var buf = parse("(stmts (asgn x.0.M (sub (i 32) 10 (neg (i 32) 5))))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M 15))""")

  block bitwise:
    # (bitand T (bitor T 5 2) (bitxor T 7 3)) — fold step by step.
    # bitor 5 2 = 7; bitxor 7 3 = 4; bitand 7 4 = 4.
    var buf = parse("(stmts (asgn x.0.M (bitand (i 32) (bitor (i 32) 5 2) (bitxor (i 32) 7 3))))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M 4))""")

  block cmp_true:
    # (eq T 5 5) → (true).
    var buf = parse("(stmts (asgn x.0.M (eq (i 32) 5 5)))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M
(true)))""")

  block cmp_false:
    # (lt T 7 3) → (false).
    var buf = parse("(stmts (asgn x.0.M (lt (i 32) 7 3)))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M
(false)))""")

  block bool_ops:
    # (and (true) (or (false) (not (false)))) → (and (true) (or (false) (true))) →
    # (and (true) (true)) → (true).
    var buf = parse("(stmts (asgn x.0.M (and (true) (or (false) (not (false))))))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M
(true)))""")

  block partial_no_fold:
    # One operand is a symbol → no fold.
    assertUnchanged("(stmts (asgn x.0.M (add (i 32) y.0.M 3)))")

  block fold_inside_call:
    # Nested fold inside a call argument.
    var buf = parse("(stmts (call use.0.M (add (i 32) 4 6)))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(call use.0.M 10))""")

  block fold_in_cond:
    # Fold the if condition itself.
    var buf = parse("(stmts (if (elif (lt (i 32) 1 2) (asgn x.0.M 1))))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(if
(elif
(true)
(asgn x.0.M 1))))""")

  block chained_with_cmp:
    # (eq T (add T 1 2) 3) → (eq T 3 3) → (true).
    var buf = parse("(stmts (asgn x.0.M (eq (i 32) (add (i 32) 1 2) 3)))")
    runConstantFolding buf
    assertRender(buf, """
(stmts
(asgn x.0.M
(true)))""")

  echo "constant_folding.nim: all self-tests passed"

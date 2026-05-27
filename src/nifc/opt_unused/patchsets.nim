#
#
#           NIFC Patchset
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## A `Patchset` records position-keyed token rewrites against a source
## `TokenBuf` and applies them in one pass by rebuilding the buffer.
##
## Two patch kinds are supported:
##
## - `pkInsert` — splice the source subtree *before* the original token
##   at the position. Multiple inserts at the same position are emitted
##   in registration order.
## - `pkSubst` — replace the entire subtree at the position with the
##   source subtree. At most one substitute per position is honored
##   (later substitutes at the same position are silently dropped).
##
## A position may carry any combination: only inserts (emitted before
## the original); only a substitute (replaces the original); or inserts
## followed by a substitute (emitted in order, then the substitute
## replaces the original).
##
## Source subtrees are referenced by `Cursor`. The caller owns whatever
## buffer the cursor points into and is responsible for keeping it
## alive until `apply` returns. A cursor at a single-token leaf (a
## `Symbol`, a literal, or a bare `DotToken`) substitutes that one
## token; a cursor at a `ParLe` substitutes the whole subtree (via
## `addSubtree`, so jump fields are recomputed correctly in the
## rebuilt buffer).
##
## The apply pass does **not** recurse into substituted subtrees to
## look for further patches. Callers that need chained expansion
## should resolve chains at registration time and store a terminal
## cursor.

import std / [tables, assertions]
include "../../lib" / nifprelude
import nifstreams, nifcursors

type
  PatchKind* = enum pkInsert, pkSubst

  Patch = object
    kind: PatchKind
    src: Cursor

  Patchset* = object
    orig: ptr TokenBuf
    patches: Table[int, seq[Patch]]

proc initPatchset*(orig: ptr TokenBuf): Patchset =
  ## Create a patchset bound to `orig`. Positions passed to
  ## `addInsert` / `addSubst` are interpreted as token indices into
  ## `orig[]`.
  Patchset(orig: orig, patches: initTable[int, seq[Patch]]())

proc isEmpty*(p: Patchset): bool {.inline.} = p.patches.len == 0

proc addInsert*(p: var Patchset; pos: int; src: Cursor) =
  p.patches.mgetOrPut(pos, @[]).add Patch(kind: pkInsert, src: src)

proc addSubst*(p: var Patchset; pos: int; src: Cursor) =
  p.patches.mgetOrPut(pos, @[]).add Patch(kind: pkSubst, src: src)

proc applyOne(p: var Patchset; dest: var TokenBuf; n: var Cursor) =
  let pos = cursorToPosition(p.orig[], n)
  var substituted = false
  if pos in p.patches:
    for patch in p.patches[pos]:
      case patch.kind
      of pkInsert:
        dest.addSubtree patch.src
      of pkSubst:
        if not substituted:
          dest.addSubtree patch.src
          substituted = true
    if substituted:
      skip n
      return
  case n.kind
  of ParLe:
    dest.add n
    n.loopInto:
      applyOne(p, dest, n)
    dest.addParRi()
  of Symbol:
    dest.add n
    inc n
  else:
    dest.add n
    inc n

proc apply*(p: var Patchset; sizeHint = -1): TokenBuf =
  ## Rebuild `orig` with every patch applied. The result is a fresh
  ## `TokenBuf` that the caller may move into a `var TokenBuf`
  ## parameter.
  let cap = if sizeHint > 0: sizeHint else: p.orig[].len + p.patches.len * 4
  result = createTokenBuf(cap)
  var n = beginRead(p.orig[])
  while n.hasMore:
    applyOne(p, result, n)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    var stream = nifstreams.openFromBuffer(src, "M")
    result = fromStream(stream)

  proc dumpKinds(buf: var TokenBuf): string =
    result = ""
    var c = beginRead(buf)
    while c.hasMore:
      if result.len > 0: result.add ' '
      result.add $c.kind
      inc c

  proc countKind(buf: TokenBuf; k: NifKind): int =
    result = 0
    for i in 0 ..< buf.len:
      if buf[i].kind == k: inc result

  discard pool.syms.getOrIncl("x.0.M")
  discard pool.syms.getOrIncl("y.0.M")

  block subst_call_with_dot:
    # Replace the inner (call ...) with a single DotToken — same shape
    # arcopt would produce for an elided wasMoved.
    var buf = parse("(stmts (call x.0.M) (call y.0.M))")
    var dotBuf = createTokenBuf(2)
    dotBuf.addDotToken()
    var ps = initPatchset(addr buf)
    # Position 1 is the (call ParLe for the first call.
    ps.addSubst(1, cursorAt(dotBuf, 0))
    var newBuf = ps.apply()
    doAssert countKind(newBuf, DotToken) == 1, dumpKinds(newBuf)
    # First call's tokens are gone; second call survives intact.
    doAssert countKind(newBuf, ParLe) == 2, dumpKinds(newBuf)  # outer stmts + second call

  block insert_var_before_call:
    # Insert a synthesized (var :v . . 5) before the first call.
    var buf = parse("(stmts (call x.0.M))")
    var synth = createTokenBuf(8)
    synth.addParLe(TagId(0), NoLineInfo)   # any tag — just a structure
    synth.addDotToken()
    synth.addParRi()
    var ps = initPatchset(addr buf)
    # Position 1 is the (call ParLe.
    ps.addInsert(1, cursorAt(synth, 0))
    var newBuf = ps.apply()
    # Outer stmts now contains two children: the inserted subtree and
    # the original call.
    doAssert countKind(newBuf, ParLe) == 3, dumpKinds(newBuf)  # stmts + synth + call

  block multiple_inserts_at_same_pos:
    var buf = parse("(stmts (call x.0.M))")
    var s1 = createTokenBuf(2); s1.addDotToken()
    var s2 = createTokenBuf(2); s2.addDotToken()
    var ps = initPatchset(addr buf)
    ps.addInsert(1, cursorAt(s1, 0))
    ps.addInsert(1, cursorAt(s2, 0))
    var newBuf = ps.apply()
    # Two dots inserted before the call, plus the call itself.
    doAssert countKind(newBuf, DotToken) == 2, dumpKinds(newBuf)
    doAssert countKind(newBuf, ParLe) == 2, dumpKinds(newBuf)  # stmts + call

  block insert_then_subst:
    # Insert a dot, then substitute the call with a different dot.
    var buf = parse("(stmts (call x.0.M))")
    var s1 = createTokenBuf(2); s1.addDotToken()
    var s2 = createTokenBuf(2); s2.addDotToken()
    var ps = initPatchset(addr buf)
    ps.addInsert(1, cursorAt(s1, 0))
    ps.addSubst(1, cursorAt(s2, 0))
    var newBuf = ps.apply()
    doAssert countKind(newBuf, DotToken) == 2, dumpKinds(newBuf)
    # The original call is gone.
    doAssert countKind(newBuf, ParLe) == 1, dumpKinds(newBuf)  # just the outer stmts

  echo "patchsets.nim: all self-tests passed"

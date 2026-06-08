#
#
#           NIFC Patchset (nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## nifcore port of `patchsets.nim`. A `Patchset` records position-keyed token
## rewrites against a source `TokenBuf` and applies them in one rebuild pass.
## See the original `patchsets.nim` for the full semantics; the only structural
## difference here is that nifcore has no ParRi token — a substituted/rebuilt
## subtree is reopened with `openTag`/`closeTag` so jump fields are recomputed.

import std / [tables, assertions]
import ".." / ".." / "lib" / nifcoreparse   # re-exports nifcore

type
  PatchKind* = enum pkInsert, pkSubst

  Patch = object
    kind: PatchKind
    src: Cursor

  Patchset* = object
    orig: ptr TokenBuf
    patches: Table[int, seq[Patch]]

proc initPatchset*(orig: ptr TokenBuf): Patchset =
  ## Create a patchset bound to `orig`. Positions are token indices into `orig[]`.
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
    for patch in p.patches.getOrDefault(pos):
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
  of TagLit:
    # Reopen the tag and recurse so `closeTag` recomputes the jump after any
    # inserts/substitutions changed the child count.
    let tag = n.cursorTagId
    let li = rawLineInfo(n)
    dest.openTag tag
    if li.isValid: dest.appendLineInfo li
    n.loopInto:
      applyOne(p, dest, n)
    dest.closeTag()
  else:
    dest.addSubtree n
    inc n

proc apply*(p: var Patchset; sizeHint = -1): TokenBuf =
  ## Rebuild `orig` with every patch applied. Result shares `orig`'s pool/tags
  ## so spliced subtrees (which also share them) copy in bulk.
  let cap = if sizeHint > 0: sizeHint else: p.orig[].len + p.patches.len * 4
  result = createTokenBuf(cap, p.orig[].pool, p.orig[].tags)
  var n = beginRead(p.orig[])
  # Single outermost block (module / proc-body `(stmts …)`); rebuild that one
  # root tree — `applyOne` descends into its children with the bounded loop.
  applyOne(p, result, n)

# ---- self-tests ----------------------------------------------------------

when isMainModule:
  proc parse(src: string): TokenBuf =
    parseFromBuffer(src, "M", 100)

  proc countKind(buf: var TokenBuf; k: NifKind): int =
    result = 0
    var c = beginRead(buf)
    while c.hasMore:
      if c.kind == k: inc result
      inc c

  block subst_call_with_dot:
    var buf = parse("(stmts (call x.0.M) (call y.0.M))")
    var dotBuf = createTokenBuf(2, buf.pool, buf.tags)
    dotBuf.addDotToken()
    var ps = initPatchset(addr buf)
    ps.addSubst(1, cursorAt(dotBuf, 0))        # pos 1 = first (call
    var newBuf = ps.apply()
    doAssert countKind(newBuf, DotToken) == 1
    doAssert countKind(newBuf, TagLit) == 2    # outer stmts + second call

  block insert_var_before_call:
    var buf = parse("(stmts (call x.0.M))")
    var synth = createTokenBuf(8, buf.pool, buf.tags)
    synth.openTag TagId(0)
    synth.addDotToken()
    synth.closeTag()
    var ps = initPatchset(addr buf)
    ps.addInsert(1, cursorAt(synth, 0))
    var newBuf = ps.apply()
    doAssert countKind(newBuf, TagLit) == 3    # stmts + synth + call

  block multiple_inserts_at_same_pos:
    var buf = parse("(stmts (call x.0.M))")
    var s1 = createTokenBuf(2, buf.pool, buf.tags); s1.addDotToken()
    var s2 = createTokenBuf(2, buf.pool, buf.tags); s2.addDotToken()
    var ps = initPatchset(addr buf)
    ps.addInsert(1, cursorAt(s1, 0))
    ps.addInsert(1, cursorAt(s2, 0))
    var newBuf = ps.apply()
    doAssert countKind(newBuf, DotToken) == 2
    doAssert countKind(newBuf, TagLit) == 2    # stmts + call

  block insert_then_subst:
    var buf = parse("(stmts (call x.0.M))")
    var s1 = createTokenBuf(2, buf.pool, buf.tags); s1.addDotToken()
    var s2 = createTokenBuf(2, buf.pool, buf.tags); s2.addDotToken()
    var ps = initPatchset(addr buf)
    ps.addInsert(1, cursorAt(s1, 0))
    ps.addSubst(1, cursorAt(s2, 0))
    var newBuf = ps.apply()
    doAssert countKind(newBuf, DotToken) == 2
    doAssert countKind(newBuf, TagLit) == 1    # just the outer stmts

  echo "patchsets_nc.nim: all self-tests passed"

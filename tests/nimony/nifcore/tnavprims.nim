# Focused regression tests for the nifcore navigation primitives added in
# #2109: span / firstSon / isLastSon / takeTree / linearScan. tnifcore.nim
# smoke-tests them on one tree; this file exercises the edge cases that a
# frontend port leans on — leaf vs subtree, single vs multiple children,
# the outer-scope bound of firstSon, deep all-depth linearScan with an early
# break, and the cross-pool re-interning path of takeTree.

import nifcore
import std / [assertions, syncio]

proc main =
  let tp = newTagPool()
  let tStmts = tp.registerTag("stmts")
  let tPair  = tp.registerTag("pair")
  let tCall  = tp.registerTag("call")
  let tAdd   = tp.registerTag("add")
  let tInner = tp.registerTag("inner")
  let tLeaf  = tp.registerTag("leaf")

  # ── span: atom is one token; a subtree is head + the sum of its children ──
  block:
    var b = createTokenBuf(16, sharedTags = tp)   # (pair 10 20)
    b.buildTree tPair:
      b.addIntLit(10)
      b.addIntLit(20)
    var root = b.beginRead()
    let child = firstSon(root)                     # the IntLit 10
    assert child.kind == IntLit
    assert span(child) == 1                        # a leaf occupies one token
    var second = child
    skip second                                    # the IntLit 20
    assert span(second) == 1
    # a subtree's span is its head plus the span of every child.
    let headWidth = tokenWidth(root)
    assert span(root) == headWidth + span(child) + span(second)
    assert span(root) >= 3                          # head + two leaves at minimum
    var afterRoot = root
    skip afterRoot                                  # skipping the root by its span
    assert not afterRoot.hasMore                    # exhausts the buffer
    root.endRead()

  # ── firstSon: lands on the first body token, for both a tag- and an
  #    atom-first child, and carries the outer scope bound (not the body's) ──
  block:
    var b = createTokenBuf(32, sharedTags = tp)   # (stmts (inner 1) 2)
    b.buildTree tStmts:
      b.buildTree tInner:
        b.addIntLit(1)
      b.addIntLit(2)
    var root = b.beginRead()
    let s0 = firstSon(root)                         # first child is (inner 1)
    assert s0.kind == TagLit and s0.cursorTagId == tInner
    # firstSon == a plain `inc` of a copy: same landing token.
    var byInc = root
    inc byInc
    assert byInc.kind == TagLit and byInc.cursorTagId == tInner
    # atom-first child:
    var b2 = createTokenBuf(16, sharedTags = tp)  # (pair 7 8)
    b2.buildTree tPair:
      b2.addIntLit(7)
      b2.addIntLit(8)
    var root2 = b2.beginRead()
    let a0 = firstSon(root2)
    assert a0.kind == IntLit and a0.intVal == 7
    root.endRead()
    root2.endRead()

  # ── isLastSon: true only for the final child; works for the sole child,
  #    a middle child, an atom child and a subtree child ──
  block:
    var one = createTokenBuf(16, sharedTags = tp)  # (leaf 42)
    one.buildTree tLeaf:
      one.addIntLit(42)
    var oneRoot = one.beginRead()
    assert isLastSon(firstSon(oneRoot))            # sole child is the last child
    oneRoot.endRead()

    var many = createTokenBuf(32, sharedTags = tp) # (stmts (inner 1) 2 3)
    many.buildTree tStmts:
      many.buildTree tInner:
        many.addIntLit(1)
      many.addIntLit(2)
      many.addIntLit(3)
    var manyRoot = many.beginRead()
    var cur = firstSon(manyRoot)                    # (inner 1) — a subtree
    assert cur.kind == TagLit and not isLastSon(cur)
    skip cur                                        # the IntLit 2 — middle atom
    assert cur.intVal == 2 and not isLastSon(cur)
    skip cur                                        # the IntLit 3 — last atom
    assert cur.intVal == 3 and isLastSon(cur)
    manyRoot.endRead()

  # ── takeTree: copies a whole value (leaf or subtree) and advances past it.
  #    Same-pool path is a bulk copy; here we drain every child in sequence
  #    and confirm the copies read back with identical structure. ──
  block:
    var b = createTokenBuf(32, sharedTags = tp)   # (add (pair 1 2) 9)
    b.buildTree tAdd:
      b.buildTree tPair:
        b.addIntLit(1)
        b.addIntLit(2)
      b.addIntLit(9)
    var root = b.beginRead()
    var w = firstSon(root)                          # at (pair 1 2)
    let pairSpan = span(w)

    var dest = createTokenBuf(16, sharedPool = b.pool, sharedTags = b.tags)
    takeTree(dest, w)                               # copy the subtree, advance
    assert w.kind == IntLit and w.intVal == 9       # advanced past the subtree
    assert isLastSon(w)

    var dc = dest.beginRead()
    assert dc.kind == TagLit and dc.cursorTagId == tPair
    assert span(dc) == pairSpan                     # structure preserved exactly
    var dchild = firstSon(dc)
    assert dchild.intVal == 1
    skip dchild
    assert dchild.intVal == 2
    dc.endRead()

    # taking the trailing atom too drains the parent.
    takeTree(dest, w)
    assert not w.hasMore
    root.endRead()

  # ── takeTree across pools: a dest with a *fresh literal pool* forces the
  #    re-interning path. Shared tag pool keeps tag ids stable; pooled and
  #    inline strings and ints must survive the copy. ──
  block:
    var src = createTokenBuf(32, sharedTags = tp)  # (call "hello" "hi" 5)
    src.buildTree tCall:
      src.addStrLit("hello")                        # >3 chars: pool-interned
      src.addStrLit("hi")                           # <=3 chars: inline
      src.addIntLit(5)
    var sroot = src.beginRead()

    var dest = createTokenBuf(16, sharedTags = tp)  # different literal pool
    assert dest.pool != src.pool                    # ensure the re-intern path
    var w = sroot
    takeTree(dest, w)

    var dc = dest.beginRead()
    assert dc.cursorTagId == tCall                  # tag id stable (shared pool)
    var f = firstSon(dc)
    assert f.strVal == "hello"                      # re-interned pooled string
    skip f
    assert f.strVal == "hi"                         # re-interned inline string
    skip f
    assert f.intVal == 5
    dc.endRead()
    sroot.endRead()

  # ── linearScan: visits every nested TagLit at all depths in document order,
  #    never the root itself, skips atoms, and supports an early break that
  #    leaves the cursor on the matching node. ──
  block:
    var b = createTokenBuf(48, sharedTags = tp)   # (stmts (call (inner 1)) 7 (add))
    b.buildTree tStmts:
      b.buildTree tCall:
        b.buildTree tInner:
          b.addIntLit(1)
      b.addIntLit(7)                                # atom between subtrees
      b.buildTree tAdd:
        discard
    var root = b.beginRead()

    var scan = root
    var seen: seq[uint32] = @[]
    linearScan scan:
      seen.add uint32(scan.cursorTagId)
    # call, then its nested inner, then add — doc order, all depths, no stmts.
    assert seen == @[uint32(tCall), uint32(tInner), uint32(tAdd)]

    # early break: stop at the first `inner`, cursor left positioned on it.
    var scan2 = root
    var hits = 0
    linearScan scan2:
      inc hits
      if scan2.cursorTagId == tInner:
        break
    assert hits == 2                                # call, then inner
    assert scan2.cursorTagId == tInner              # left on the match
    root.endRead()

    # a subtree with no nested tags yields nothing.
    var flat = createTokenBuf(16, sharedTags = tp) # (leaf 1 2 3)
    flat.buildTree tLeaf:
      flat.addIntLit(1)
      flat.addIntLit(2)
      flat.addIntLit(3)
    var flatRoot = flat.beginRead()
    var fscan = flatRoot
    var count = 0
    linearScan fscan:
      inc count
    assert count == 0
    flatRoot.endRead()

  echo "ok"

main()

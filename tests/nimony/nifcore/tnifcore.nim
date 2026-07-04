# Regression test: the nifcore in-memory NIF library must compile *and run*
# under Nimony (it backs the plugin system). Exercises the builder, the
# cursor reader, and — critically — the manual GC_ref/GC_unref ref-counting
# of the shared pools plus the TokenBuf/Cursor destructors at scope exit.

import nifcore
import std / [assertions, syncio]

proc main =
  let tp = newTagPool()
  let tStmts = tp.registerTag("stmts")

  var b = createTokenBuf(16, sharedTags = tp)
  b.buildTree tStmts:
    b.addStrLit("hello")     # pool-interned (>3 chars)
    b.addIntLit(42)
    b.addStrLit("hi")        # inline (<=3 chars)

  # Read it back.
  var c = b.beginRead()
  assert c.kind == TagLit
  assert c.cursorTagId == tStmts
  c.inc                      # descend into the body

  assert c.kind == StrLit
  assert c.strVal == "hello"
  c.inc

  assert c.kind == IntLit
  assert c.intVal == 42
  c.inc

  assert c.kind == StrLit
  assert c.strVal == "hi"
  c.inc

  c.endRead()

  var syms = createTokenBuf()
  let sym = syms.pool.syms.getOrIncl("already.interned")
  syms.addSymUse(sym)
  var symCursor = syms.beginRead()
  assert symCursor.symId == sym
  assert symCursor.symName == "already.interned"

  var source = createTokenBuf(sharedPool = syms.pool,
                              sharedTags = syms.tags)
  source.addIdent("first")
  source.addIdent("second")
  var appended = createTokenBuf(sharedPool = syms.pool,
                                sharedTags = syms.tags)
  appended.addIdent("prefix")
  appended.addBuffer(source)
  var sourceCursor = source.beginRead()
  assert sourceCursor.strVal == "first"
  sourceCursor.skip()
  assert sourceCursor.strVal == "second"
  var appendedCursor = appended.beginRead()
  assert appendedCursor.strVal == "prefix"
  appendedCursor.skip()
  assert appendedCursor.strVal == "first"
  appendedCursor.skip()
  assert appendedCursor.strVal == "second"
  appendedCursor.skip()
  assert not appendedCursor.hasMore

  # Assignment must copy the cursor's bound even when both cursors point at
  # the same token in the same owner.
  var boundedSource = createTokenBuf(sharedPool = syms.pool,
                                     sharedTags = syms.tags)
  boundedSource.buildTree tStmts:
    boundedSource.addIdent("inside")
  boundedSource.addIdent("outside")
  var boundedRoot = boundedSource.beginRead()
  var bounded = boundedRoot.childCursor()
  var unbounded = boundedSource.cursorAt(1)
  unbounded = bounded
  unbounded.skip()
  assert not unbounded.hasMore

  var foreign = createTokenBuf()
  foreign.addIdent("foreign")
  appended.addBuffer(foreign)
  appendedCursor = appended.beginRead()
  appendedCursor.skip()
  appendedCursor.skip()
  appendedCursor.skip()
  assert appendedCursor.strVal == "foreign"

  # Navigation primitives: span / firstSon / isLastSon / takeTree / linearScan.
  let tCall = tp.registerTag("call")
  let tAdd = tp.registerTag("add")
  var nb = createTokenBuf(32, sharedTags = tp)   # (stmts (call "foo" 1) (add 2 3))
  nb.buildTree tStmts:
    nb.buildTree tCall:
      nb.addStrLit("foo")
      nb.addIntLit(1)
    nb.buildTree tAdd:
      nb.addIntLit(2)
      nb.addIntLit(3)
  var nroot = nb.beginRead()

  let call0 = firstSon(nroot)                    # first child is (call ...)
  assert call0.kind == TagLit and call0.cursorTagId == tCall
  assert not isLastSon(call0)                    # (call ...) is not the last child
  var add0 = call0
  skip add0                                      # advance to (add ...)
  assert add0.cursorTagId == tAdd and isLastSon(add0)

  var taken = createTokenBuf(16, sharedPool = nb.pool, sharedTags = nb.tags)
  var w = firstSon(nroot)
  takeTree(taken, w)                             # copy (call ...), advance past it
  assert w.cursorTagId == tAdd
  var tc = taken.beginRead()
  assert tc.cursorTagId == tCall and span(tc) == span(call0)

  var scan = nroot
  var seen: seq[uint32] = @[]
  linearScan scan:                               # all nested tags, doc order
    seen.add uint32(scan.cursorTagId)
  assert seen == @[uint32(tCall), uint32(tAdd)]
  nroot.endRead()

  echo "ok"

main()

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
  echo "ok"

main()

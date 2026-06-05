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
  echo "ok"

main()

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
  let sym = syms.pool.symId("already.interned")
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

  # A symbol is an object, not a string (#2457): `sym` takes one apart and the
  # parts are ids, so asking "same module?" is an integer comparison.
  var symbols = createTokenBuf()
  let p = symbols.pool
  let inst = p.symId("gen.12.Ikey.mymod")
  let glob = p.symId("other.3.mymod")
  let loc = p.symId("tmp.14")
  let dotted = p.symId("Pool.Obj.0")

  assert p.sym(inst).disamb == 12
  assert p.strings[p.sym(inst).name] == "gen"
  assert p.strings[p.sym(inst).dedup] == "Ikey"
  assert p.sym(inst).module == p.sym(glob).module
  assert not p.sym(inst).isLocal

  assert p.sym(loc).isLocal
  assert p.sym(loc).dedup == StrId(0)
  assert p.strings[p.sym(loc).name] == "tmp"

  # A name may contain dots; only the disambiguator's dot ends it.
  assert p.sym(dotted).isLocal
  assert p.strings[p.sym(dotted).name] == "Pool.Obj"

  # The parts and the spelling are the same symbol.
  assert p.symId("gen", 12, "mymod", "Ikey") == inst
  assert p.symId("tmp", 14) == loc
  assert p.symString(inst) == "gen.12.Ikey.mymod"
  assert p.symString(p.sym(inst)) == "gen.12.Ikey.mymod"
  assert p.symId(p.sym(glob)) == glob

  # The three questions the Nim compiler's IC modules ask of `pool.syms`,
  # spelled the way its own copy of `nifstreams` spells them. The pool stores
  # `NifSymbol` records; this surface is what keeps that copy compiling.
  let classicA = p.syms.getOrIncl("abc.12.Ikey.mymod")
  let classicB = p.syms.getOrIncl("tmp.7")
  assert p.syms[classicA] == "abc.12.Ikey.mymod"
  assert p.syms[classicB] == "tmp.7"
  assert p.syms.getOrIncl("abc.12.Ikey.mymod") == classicA
  assert p.syms.getKeyId("abc.12.Ikey.mymod") == classicA
  assert p.syms.getKeyId("never.9.seen") == SymId(0)
  # a miss must intern nothing, or the next one would be a hit
  let stringsBefore = p.strings.len
  assert p.syms.getKeyId("nothing.0.here") == SymId(0)
  assert p.strings.len == stringsBefore

  echo "ok"

main()

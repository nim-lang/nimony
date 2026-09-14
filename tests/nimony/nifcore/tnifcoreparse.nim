# Regression test: nifcoreparse (the NIF text parser producing a nifcore
# TokenBuf) must compile *and run* under Nimony — it sits on top of nifcore
# and the nifreader, and is part of the plugin pipeline.
#
# Verifies the parse -> toString -> parse round-trip is byte-identical, which
# exercises the parser, the builder, the cursor, and all the TokenBuf / Cursor
# destructors (incl. the shared-pool GC_ref / GC_unref) at scope exit.

import nifcoreparse
import nifbuilder
import std / [assertions, syncio]

proc sameTokens(a, b: var TokenBuf): bool =
  if a.len != b.len: return false
  for i in 0 ..< a.len:
    if not (a[i] == b[i]): return false
  true

proc roundTrips(b1: var TokenBuf): bool =
  let txt = toString(b1)
  var b2 = parseFromBuffer(txt, "t")
  result = sameTokens(b1, b2)
  if not result: echo "round-trip MISMATCH:\n", txt

proc main =
  var empty = createTokenBuf()
  assert toString(empty) == ""

  for s in [
      "(stmts (call foo 42 \"hi\") (asgn x 3.14) (ret -7))",
      "(proc :myproc.0 . . (params (param x.1 (i +32))) (i +32) (stmts (ret 0)))",
      "(x \"longer than three bytes\" 'a' +18446744073709551615u -9223372036854775808)",
      "(nested (a (b (c (d .)))))",
      # Every shape the parser's split-symbol path has to get right: the pool
      # stores symbols taken apart, so each of these must come back out of it
      # spelled exactly as it went in (#2457).
      "(stmts glob.12.mymod gen.12.Ikey.mymod loc.7 a.0 :def.3.mymod)",
      # a disambiguator that is not a number, so the whole thing is the name
      "(stmts p.0h107 _exit.sys.mymod d.00)",
      # two keys is not one instantiation; and a name with dots in it
      "(stmts foo.0.Ia.Ib.mymod Pool.Obj.0 ..<.3.mymod)",
      # a trailing dot is this module, which the reader expands before the pool
      # ever sees it
      "(stmts self.4. :selfdef.5.)",
      # escapes inside the name, and an operator definition with none at all
      "(stmts \\5B\\5D=.0.mymod :\\5B\\5D=)"]:
    var b1 = parseFromBuffer(s, "t")
    assert roundTrips(b1)

  var positioned = createTokenBuf()
  positioned.addIdent("hello")
  let file = positioned.pool.filenames.getOrIncl("source.nim")
  positioned.appendLineInfo(file, 12, 3)
  assert toString(positioned, includeLineInfo = false) == "hello"
  assert toString(positioned).len > "hello".len
  let positionedCursor = positioned.beginRead()
  assert toString(positionedCursor, includeLineInfo = false) == "hello"

  var sparse = createTokenBuf()
  let tag = sparse.tags.registerTag("pair")
  sparse.openTag(tag)
  let sparseFile = sparse.pool.filenames.getOrIncl("dense.nim")
  sparse.appendLineInfo(sparseFile, 7, 2)
  sparse.addIdent("left")
  sparse.addIdent("right")
  sparse.closeTag()
  var dense = parseFromBuffer(toString(sparse), "dense",
                              denseLineInfo = true)
  var child = dense.beginRead()
  child = child.childCursor
  while child.hasMore:
    assert child.rawLineInfo.isValid
    child.skip()

  # A non-finite float is written as a compound, `(inf)`, and its line info
  # has to go on the tag name. Attached after the `)` the text was not NIF, and
  # the reader stopped there, silently dropping everything after the literal.
  var specials = createTokenBuf()
  let specialsFile = specials.pool.filenames.getOrIncl("floats.nim")
  specials.openTag(specials.tags.registerTag("consts"))
  specials.appendLineInfo(specialsFile, 1, 0)
  for v in [Inf, NaN, -Inf, 1.5]:
    specials.addFloatLit(v)
    specials.appendLineInfo(specialsFile, 2, 12)
  specials.addIdent("after")
  specials.appendLineInfo(specialsFile, 3, 4)
  specials.closeTag()
  let specialsText = toString(specials)
  var reread = parseFromBuffer(specialsText, "floats")
  assert toString(reread, includeLineInfo = false) ==
    "(consts\n (inf)\n (nan)\n (neginf)1.5 after)"

  var unusedName = ""
  var hinted = parseFromBuffer(
    "(.unusedname tmp.14)\n(stmts)", "hinted", unusedName)
  assert unusedName == "tmp.14"
  assert toString(hinted, includeLineInfo = false) == "(stmts)"

  var symbols = createTokenBuf()
  let fresh = symbols.pool.symId("tmp.14")
  symbols.addSymDef(fresh)
  assert toString(symbols, includeLineInfo = false) == ":tmp.14"
  var rendered = nifbuilder.open(32)
  rendered.addRaw "prefix "
  symbols.appendTo(rendered, includeLineInfo = false)
  assert rendered.extract() == "prefix :tmp.14"
  echo "ok"

main()

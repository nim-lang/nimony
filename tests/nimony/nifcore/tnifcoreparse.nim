# Regression test: nifcoreparse (the NIF text parser producing a nifcore
# TokenBuf) must compile *and run* under Nimony — it sits on top of nifcore
# and the nifreader, and is part of the plugin pipeline.
#
# Verifies the parse -> toString -> parse round-trip is byte-identical, which
# exercises the parser, the builder, the cursor, and all the TokenBuf / Cursor
# destructors (incl. the shared-pool GC_ref / GC_unref) at scope exit.

import nifcoreparse
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
  for s in [
      "(stmts (call foo 42 \"hi\") (asgn x 3.14) (ret -7))",
      "(proc :myproc.0 . . (params (param x.1 (i +32))) (i +32) (stmts (ret 0)))",
      "(x \"longer than three bytes\" 'a' +18446744073709551615u -9223372036854775808)",
      "(nested (a (b (c (d .)))))"]:
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
  echo "ok"

main()

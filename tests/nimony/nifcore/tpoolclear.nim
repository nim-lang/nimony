# Regression test for the three additions the HTTP message layer needs
# (doc/internals/http.md §1): `seal`/`tagId` on TagPool, and `clear` on Pool.

import nifcore
import std / [assertions, syncio]

proc testSeal =
  let tp = newTagPool()
  let host = tp.registerTag("host")
  let accept = tp.registerTag("accept")
  assert host.uint32 == 1'u32
  assert accept.uint32 == 2'u32

  # `tagId` answers without interning — what a parser handed untrusted bytes
  # is supposed to ask.
  assert tp.tagId("host") == host
  assert tp.tagId("accept") == accept
  assert tp.tagId("x-unregistered").uint32 == 0'u32
  assert tp.tags.len == 2

  tp.seal()
  assert tp.tagId("host") == host              # lookups still work
  assert tp.tagId("x-unregistered").uint32 == 0'u32
  assert tp.tags.len == 2                      # and still never grow

proc testClear =
  let tp = newTagPool()
  let host = tp.registerTag("host")
  tp.seal()

  var buf = createTokenBuf(16, sharedTags = tp)
  buf.buildTree host:
    buf.addStrLit("example.com")
    buf.addIntLit(42)

  var c = buf.beginRead()
  assert c.kind == TagLit
  assert c.cursorTagId == host
  c.inc
  assert c.strVal == "example.com"
  c.inc
  assert c.intVal == 42
  c.inc
  c.endRead()

  let firstId = buf.pool.strings.getOrIncl("example.com")

  # Recycle: drop the tokens, drop the literals, keep both allocations.
  buf.shrink 0
  buf.pool.clear()
  assert buf.pool.strings.len == 0
  assert buf.len == 0

  # Ids restart, and the blanked hash index still resolves.
  let again = buf.pool.strings.getOrIncl("something-else")
  assert again == firstId
  assert buf.pool.strings[again] == "something-else"
  let other = buf.pool.strings.getOrIncl("example.com")
  assert other != again
  assert buf.pool.strings[other] == "example.com"

  # The buffer is usable again, against the same (sealed) tag pool.
  buf.pool.clear()
  buf.buildTree host:
    buf.addStrLit("recycled.example")
  var c2 = buf.beginRead()
  assert c2.cursorTagId == host
  c2.inc
  assert c2.strVal == "recycled.example"
  c2.inc
  c2.endRead()

proc testClearManyRounds =
  # A pool refilled far past its initial index size: `clear` must leave the
  # table growable, not just survive one round.
  var buf = createTokenBuf(16)
  for round in 0..<8:
    for i in 0..<40:
      discard buf.pool.strings.getOrIncl("header-value-" & $round & "-" & $i)
    assert buf.pool.strings.len == 40
    buf.pool.clear()
    assert buf.pool.strings.len == 0

testSeal()
testClear()
testClearManyRounds()
echo "nifcore pool: ok"

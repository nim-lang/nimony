# tclosure_xmod_destroy for an object that is exactly 24 bytes: the lowered
# closure pair is two words (its fn slot keeps the `closure` pragma but is a
# bare pointer), so the defining module and an importer that sees the
# still-unlowered type must both pass it by value. sizeof counted the lowered
# pair as three words: the hook took a pointer, the importer passed the value.
import std/syncio
import deps/mclosuresmall

proc plain() =
  var q: seq[Entry] = @[]
  q.add Entry(a: 1'u64, fn: proc (): int {.closure.} = 41)
  q.add mkEntry(2'u64, 42)
  echo q.len, " ", q[0].fn(), " ", q[1].fn()

proc resizing() =
  var q: seq[Entry] = @[]
  for i in 0 ..< 5:
    q.add mkEntry(uint64(i), i * 10)
  q.shrink(2)
  echo q.len, " ", q[1].fn()
  q.del(0)
  echo q.len, " ", q[0].fn()
  q = @[]
  echo q.len

proc withRef() =
  var q: seq[WithRef] = @[]
  q.add mkWithRef(7)
  q.add WithRef(node: Node(val: 8), fn: proc (): int {.closure.} = 9)
  echo q.len, " ", q[0].node.val, " ", q[0].fn(), " ", q[1].node.val, " ", q[1].fn()
  q.del(0)
  echo q.len, " ", q[0].fn()

proc nested() =
  var q: seq[Outer] = @[]
  q.add mkOuter(3'u64, 5)
  q.add Outer(inner: Entry(a: 4'u64, fn: proc (): int {.closure.} = 6), tag: 1)
  echo q.len, " ", q[0].inner.fn(), " ", q[1].inner.fn(), " ", q[1].tag
  q.shrink(1)
  echo q.len

plain()
resizing()
withRef()
nested()
echo "ok"

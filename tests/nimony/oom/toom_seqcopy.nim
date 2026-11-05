# Seq paths that allocate a fresh buffer and then fill it. A refused
# allocation must leave every container valid, with `oomHandler` called: a
# new seq is empty, a grown one keeps its old buffer and elements. Nothing
# may be written through a buffer that was not allocated, nor past the end of
# one that did not grow.
#
# Each source below takes 880 KB of the 1 MiB budget (see `nimony.args`), so
# any second buffer of half its size or more is refused.
# That arithmetic needs a heap of its own, hence the `.nojoin` sidecar.
import std / [syncio, algorithm, deques, sequtils]

const N = 110_000

proc dupCopy() =
  var a = newSeq[int](N)
  a[N-1] = 7
  var b = a          # `=dup`: `a` is used again below
  echo "dup: ", b.len, " src ", a.len, " ", a[N-1]

proc sliceCopy() =
  var a = newSeq[int](N)
  let b = a[0 .. N-1]
  echo "slice: ", b.len, " src ", a.len

proc insertGrow() =
  var a = newSeq[int](N)
  a[0] = 1
  a[N-1] = 2
  let more = newSeqOf[int](1_000, 9)  # past the allocator's rounding slack
  a.insert(more, 0)  # needs a bigger buffer: refused
  echo "insert: ", a.len, " ", a[0], " ", a[N-1]

proc sortTemp() =
  var a = newSeq[int](N)
  for i in 0 ..< N: a[i] = N - i
  a.sort()           # its merge buffer is `N div 2` elements: refused
  echo "sort: ", a.len, " ", a[0], " ", a[N-1]

proc dequeGrow() =
  var d = initDeque[int](65_536)   # 512 KB
  for i in 0 ..< 65_536: d.addLast i
  d.addLast 65_536   # full: doubling to 1 MB is refused, the item is dropped
  d.addFirst -1
  echo "deque: ", d.len, " ", d.peekFirst, " ", d.peekLast

proc main() =
  dupCopy()
  sliceCopy()
  insertGrow()
  sortTemp()
  dequeGrow()
  echo "still running"

main()

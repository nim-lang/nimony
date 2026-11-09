## #2741: the `*It`/fold templates of std/sequtils evaluate their collection
## argument once, and before they inject `it`/`a`/`b`, so a caller's own `a`
## is not captured by the fold's accumulator.
import std/[assertions, sequtils]

proc getSeq(c: var int): seq[int] =
  inc c
  result = @[1, 2, 3, 4]

proc main =
  let a = @[1, 2, 3, 4]
  assert a.foldl(a + b) == 10
  assert a.foldr(a + b) == 10
  let it = @[1, 2, 3]
  assert it.mapIt(it * 2) == @[2, 4, 6]
  assert it.filterIt(it > 1) == @[2, 3]
  assert it.anyIt(it == 3)
  assert it.allIt(it > 0)
  assert it.countIt(it > 1) == 2

  var calls = 0
  assert getSeq(calls).foldl(a + b) == 10
  assert calls == 1
  calls = 0
  assert getSeq(calls).foldr(a + b) == 10
  assert calls == 1
  calls = 0
  assert getSeq(calls).mapIt(it + 1) == @[2, 3, 4, 5]
  assert calls == 1
  calls = 0
  assert getSeq(calls).filterIt(it mod 2 == 0) == @[2, 4]
  assert calls == 1
  calls = 0
  assert getSeq(calls).anyIt(it == 4)
  assert calls == 1
  calls = 0
  assert getSeq(calls).allIt(it > 0)
  assert calls == 1
  calls = 0
  assert getSeq(calls).countIt(it > 2) == 2
  assert calls == 1

  # In-place forms still work on the caller's own seq.
  var b = @[1, 2, 3, 4, 5]
  b.keepItIf(it mod 2 == 1)
  assert b == @[1, 3, 5]
  b.applyIt(it * 10)
  assert b == @[10, 30, 50]

main()

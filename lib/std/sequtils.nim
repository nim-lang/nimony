#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Operations on sequences (and `openArray`s), in the spirit of functional
## programming: `map`, `filter`, `zip`, folds and friends.
##
## This is the Nimony port. It currently covers the callback- and
## value-oriented core; the `mapIt`/`filterIt` template family is not ported
## yet.

import std/[assertions]

func repeat*[T](x: T; n: Natural): seq[T] =
  ## Returns a sequence with `x` repeated `n` times.
  runnableExamples:
    assert repeat(5, 3) == @[5, 5, 5]
    assert repeat("a", 2) == @["a", "a"]
  result = @[]
  for i in 0 ..< n:
    result.add x

# NOTE: `concat` is intentionally not provided yet — `system` exposes a greedy
# `concat*()` varargs string template that wins/!breaks overload resolution for
# a `seq` overload. Resolving that cleanly is left to a follow-up.

func count*[T: Equatable](s: openArray[T]; x: T): int =
  ## Returns the number of occurrences of `x` in `s`.
  runnableExamples:
    assert count(@[1, 2, 2, 3, 2], 2) == 3
  result = 0
  for i in 0 ..< s.len:
    if s[i] == x: inc result

# `find` and `contains` for `openArray[T: Equatable]` already live in
# `system` (openarrays); they are intentionally not redefined here.

func deduplicate*[T: Equatable](s: openArray[T]): seq[T] =
  ## Returns `s` with consecutive and non-consecutive duplicates removed,
  ## preserving first-seen order.
  runnableExamples:
    assert deduplicate(@[1, 2, 2, 3, 1]) == @[1, 2, 3]
  result = @[]
  for i in 0 ..< s.len:
    var seen = false
    for j in 0 ..< result.len:
      if result[j] == s[i]: seen = true
    if not seen: result.add s[i]

func minIndex*[T: Comparable](s: openArray[T]): int =
  ## Returns the index of the minimum element of `s` (0 for an empty `s`).
  runnableExamples:
    assert minIndex(@[3, 1, 2]) == 1
  result = 0
  for i in 1 ..< s.len:
    if s[i] < s[result]: result = i

func maxIndex*[T: Comparable](s: openArray[T]): int =
  ## Returns the index of the maximum element of `s` (0 for an empty `s`).
  runnableExamples:
    assert maxIndex(@[3, 1, 2]) == 0
  result = 0
  for i in 1 ..< s.len:
    if s[result] < s[i]: result = i

proc map*[T, S](s: openArray[T]; op: proc (x: T): S): seq[S] =
  ## Returns a new sequence with `op` applied to every element of `s`.
  runnableExamples:
    proc dbl(x: int): int = x * 2
    assert map(@[1, 2, 3], dbl) == @[2, 4, 6]
  result = @[]
  for i in 0 ..< s.len:
    result.add op(s[i])

proc filter*[T](s: openArray[T]; pred: proc (x: T): bool): seq[T] =
  ## Returns the elements of `s` for which `pred` returns true.
  runnableExamples:
    proc isEven(x: int): bool = x mod 2 == 0
    assert filter(@[1, 2, 3, 4], isEven) == @[2, 4]
  result = @[]
  for i in 0 ..< s.len:
    if pred(s[i]): result.add s[i]

proc keepIf*[T](s: var seq[T]; pred: proc (x: T): bool) =
  ## In-place variant of `filter`: keeps only the elements for which `pred`
  ## returns true.
  runnableExamples:
    proc isEven(x: int): bool = x mod 2 == 0
    var data = @[1, 2, 3, 4, 5]
    keepIf(data, isEven)
    assert data == @[2, 4]
  var kept: seq[T] = @[]
  for i in 0 ..< s.len:
    if pred(s[i]): kept.add s[i]
  s = kept

proc all*[T](s: openArray[T]; pred: proc (x: T): bool): bool =
  ## Whether `pred` is true for every element (true for an empty `s`).
  runnableExamples:
    proc isEven(x: int): bool = x mod 2 == 0
    assert all(@[2, 4, 6], isEven)
    assert not all(@[2, 3, 6], isEven)
  for i in 0 ..< s.len:
    if not pred(s[i]): return false
  return true

proc any*[T](s: openArray[T]; pred: proc (x: T): bool): bool =
  ## Whether `pred` is true for at least one element (false for an empty `s`).
  runnableExamples:
    proc isEven(x: int): bool = x mod 2 == 0
    assert any(@[1, 3, 4], isEven)
    assert not any(@[1, 3, 5], isEven)
  for i in 0 ..< s.len:
    if pred(s[i]): return true
  return false

proc apply*[T](s: var seq[T]; op: proc (x: T): T) =
  ## Applies `op` to every element of `s` in place.
  runnableExamples:
    proc inc1(x: int): int = x + 1
    var data = @[1, 2, 3]
    apply(data, inc1)
    assert data == @[2, 3, 4]
  for i in 0 ..< s.len:
    let v = op(s[i])
    s[i] = v

func cycle*[T](s: openArray[T]; n: Natural): seq[T] =
  ## Returns a sequence with the elements of `s` repeated `n` times.
  runnableExamples:
    assert cycle(@[1, 2], 3) == @[1, 2, 1, 2, 1, 2]
  result = @[]
  for _ in 0 ..< n:
    for i in 0 ..< s.len:
      result.add s[i]

func zip*[S, T](a: openArray[S]; b: openArray[T]): seq[(S, T)] =
  ## Pairs up the elements of `a` and `b`, truncating to the shorter length.
  runnableExamples:
    assert zip(@[1, 2, 3], @["a", "b"]) == @[(1, "a"), (2, "b")]
  result = @[]
  var n = a.len
  if b.len < n: n = b.len
  for i in 0 ..< n:
    result.add (a[i], b[i])

func unzip*[S, T](s: openArray[(S, T)]): (seq[S], seq[T]) =
  ## Splits a sequence of pairs into a pair of sequences.
  runnableExamples:
    let (a, b) = unzip(@[(1, "a"), (2, "b")])
    assert a == @[1, 2]
    assert b == @["a", "b"]
  var first: seq[S] = @[]
  var second: seq[T] = @[]
  for i in 0 ..< s.len:
    first.add s[i][0]
    second.add s[i][1]
  result = (first, second)

# ── `it` / fold templates ────────────────────────────────────────────────────
#
# These mirror the classic `*It` family. They are `{.untyped.}` templates and
# inject their loop bindings with `{.inject.}` (`it` for the element; `a`/`b`
# for the fold accumulator and current element).
#
# NOTE: `mapIt`/`filterIt`/`toSeq` are deliberately *not* provided yet. They must
# build a result `seq` whose element type is inferred via `typeof` inside the
# template, and Nimony currently leaves that as a bare `seq` ("got seq but
# wanted seq"). Worth a follow-up once template `typeof` element inference lands.

template foldl*(s, operation: untyped): untyped {.untyped.} =
  ## Left-associative fold. `a` is the accumulator (seeded with the first
  ## element), `b` the current element.
  runnableExamples:
    assert foldl(@[1, 2, 3, 4], a + b) == 10
  var acc = s[0]
  for i in 1 ..< s.len:
    let a {.inject.} = acc
    let b {.inject.} = s[i]
    acc = operation
  acc

template foldr*(s, operation: untyped): untyped {.untyped.} =
  ## Right-associative fold. `a` is the current element, `b` the accumulator
  ## (seeded with the last element).
  runnableExamples:
    assert foldr(@[1, 2, 3, 4], a + b) == 10
  var acc = s[s.len - 1]
  for k in 0 ..< s.len - 1:
    let a {.inject.} = s[s.len - 2 - k]
    let b {.inject.} = acc
    acc = operation
  acc

template anyIt*(s, pred: untyped): bool {.untyped.} =
  ## Whether `pred` (referencing the injected `it`) holds for any element.
  runnableExamples:
    assert anyIt(@[1, 3, 4], it mod 2 == 0)
  var res = false
  for i in 0 ..< s.len:
    let it {.inject.} = s[i]
    if pred:
      res = true
      break
  res

template allIt*(s, pred: untyped): bool {.untyped.} =
  ## Whether `pred` (referencing the injected `it`) holds for every element.
  runnableExamples:
    assert allIt(@[2, 4, 6], it mod 2 == 0)
  var res = true
  for i in 0 ..< s.len:
    let it {.inject.} = s[i]
    if not (pred):
      res = false
      break
  res

template countIt*(s, pred: untyped): int {.untyped.} =
  ## Number of elements for which `pred` (referencing the injected `it`) holds.
  runnableExamples:
    assert countIt(@[1, 2, 3, 4], it mod 2 == 0) == 2
  var res = 0
  for i in 0 ..< s.len:
    let it {.inject.} = s[i]
    if pred: inc res
  res

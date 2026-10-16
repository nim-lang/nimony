# Test: borrow checking must work for openArray. See issue #1858.
#
# An `openArray` is a `.view` type: `toOpenArray` stores a raw pointer into the
# source container. While such a view is alive the source must not be mutated,
# and it must not be passed as a `var` argument next to the view.
#
# The converters in `lib/std/system/openarrays.nim` build the view out of a raw
# pointer, which is where any path the borrow checker could follow ends, so the
# `establishesBorrow` pragma on the converter is what makes these errors fire.
# See issue #1858 and `topenarray_borrow.nim` for the cases that must still pass.

proc grow(s: var seq[int]) =
  s.add 99

proc mutate(a: var array[3, int]) =
  a[0] = 42

# 1. `add` can reallocate the seq's payload; `view` dangles afterwards.
proc testSeqRealloc =
  var s = @[1, 2, 3]
  let view = toOpenArray(s)
  grow(s)
  let x = view[0]
  discard x

testSeqRealloc()

# 2. Mutating the array through a `var` parameter while a view is alive.
proc testArrayMutate =
  var a = [1, 2, 3]
  let view = toOpenArray(a)
  mutate(a)
  let x = view[0]
  discard x

testArrayMutate()

# 3. The same container passed as a view and as a `var` argument in one call.
proc appendFirst(v: openArray[int]; s: var seq[int]) =
  s.add v[0]

proc testAliasInCall =
  var s = @[1, 2, 3]
  appendFirst(s, s)

testAliasInCall()

# 4. A `var` parameter forwarded to another `var` parameter is a location
# already, so it takes no `(haddr …)`; the mutation must be caught all the same.
proc testForwardedVar(s: var string) =
  let p = toOpenArray(s)
  s.setLen(1)
  if p.len > 0: discard p[0]

var fwd = "abc"
testForwardedVar(fwd)

# 5. Iterating a field of a `var` parameter while passing the parameter on as
# `var`: the callee may change the container being iterated.
type
  Holder = object
    items: seq[int]

proc touch(h: var Holder; x: int) =
  h.items.add x

proc testLoopOverForwarded(h: var Holder) =
  for x in h.items:
    touch(h, x)

var holder = Holder(items: @[1, 2])
testLoopOverForwarded(holder)

# 6. A `var T` result is derived from the call's first argument, so iterating
# it borrows that argument.
proc mitemsOf(h: var Holder): var seq[int] = h.items

proc testVarResult(h: var Holder) =
  for x in mitems(mitemsOf(h)):
    touch(h, x)

testVarResult(holder)

# 7. A fresh call result lives in a temporary that dies with the scope that
# evaluated it: a view of it must not be returned ...
proc freshSeq(n: int): seq[int] =
  result = @[]
  for i in 0 ..< n: result.add i

proc viewOfTemp(): openArray[int] =
  result = toOpenArray(freshSeq(3))

discard viewOfTemp()

# 8. ... nor outlive the scope it was created in.
proc tempEscapesScope(b: bool) =
  var g = toOpenArray(freshSeq(1))
  if b:
    g = toOpenArray(freshSeq(3))
  if g.len > 0: discard g[0]

tempEscapesScope(true)

# 9. `result` is never killed, but the borrow it holds lasts until the proc
# returns: mutating the source afterwards hands the caller a dangling view.
proc resultThenMutate(s: var seq[int]): openArray[int] =
  result = toOpenArray(s)
  grow(s)

var src = @[1, 2, 3]
discard resultThenMutate(src)

# 10. `let a = b` for a view `b` takes over `b`'s borrow; rebinding `b` must not
# release the only borrow on `s` while `a` still points into it.
proc copiedViewRebind =
  var s = @[1, 2, 3]
  var r = @[4, 5]
  var b = toOpenArray(s)
  let a = b
  b = toOpenArray(r)
  grow(s)
  discard a[0]

copiedViewRebind()

# 11. A reslice of a view assigned back to itself keeps the original borrow:
# `a` must not end up borrowing from `a`.
proc selfReslice =
  var s = @[1, 2, 3]
  var a = toOpenArray(s)
  a = toOpenArray(a, 1, 2)
  grow(s)
  discard a[0]

selfReslice()

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

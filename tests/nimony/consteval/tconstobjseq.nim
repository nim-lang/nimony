import std/assertions

# `const obj = Obj(weights: @[])` must pass the field's `seq[T]` type into
# const-eval so `@[]` can use the same sub-compile path as top-level
# `const empty: seq[int] = @[]` (see tconstseq.nim).

type
  Obj = object
    weights: seq[int]

const x = Obj(weights: @[])
assert x.weights.len == 0

# A user-written `(proc () {.closure.}, ref RootObj)` has exactly the shape
# lambda lifting produces for a lifted closure. Before the `closureTuple` tag
# existed, `isLiftedClosureTuple` probed for that shape structurally, so this
# type was mistaken for an already-lifted tuple: `tre` took the type
# declaration verbatim instead of routing it through `treType`, leaving the
# inner closure proctype unlifted while every closure value stored in the
# field was a `{fn, env}` pair.

import std/assertions

type
  Payload = ref object of RootObj
    tag: int
  Lookalike = (proc () {.closure.}, ref RootObj)

var trace = 0

proc makeLookalike(step: int): Lookalike =
  result = (proc () {.closure.} = trace += step, Payload(tag: step))

proc runIt(x: Lookalike) =
  x[0]()

let a = makeLookalike(3)
runIt a
assert trace == 3

let b = makeLookalike(10)
runIt b
runIt a
assert trace == 16
assert Payload(b[1]).tag == 10

# `inc` through a `var T`-returning call: the call that builds the destination
# is not a right-hand side that may mutate it (#3133).
import std/syncio

type
  Model = object
    counter: int
  Handle = object
    p: ptr Model

proc get(x: var Handle): var Model = x.p[]

proc main() =
  var m = Model(counter: 0)
  var h = Handle(p: addr m)
  inc h.get.counter
  inc h.get.counter, 2
  dec h.get.counter
  echo m.counter

main()

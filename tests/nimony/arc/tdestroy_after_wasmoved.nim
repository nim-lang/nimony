## A `=destroy` of a location still in its `=wasMoved` state has nothing to
## destroy, and arcopt drops it — including a destroy in a branch below the
## `=wasMoved`, such as the destroy of `result`'s default value before the
## branch's first assignment to it. Counted through a `=destroy` hook; a
## destroy that frees a live value must stay.

import std/syncio

type Tracked = object
  id: int

var destroyed = 0
proc `=destroy`(x: Tracked) =
  inc destroyed

proc branchy(n: int): Tracked {.noinline.} =
  if n > 0:
    result = Tracked(id: n)
  else:
    result = Tracked(id: -n)

proc threeWay(n: int): Tracked {.noinline.} =
  case n
  of 0: result = Tracked(id: 10)
  of 1: result = Tracked(id: 11)
  else: result = Tracked(id: 12)

proc twice(n: int): Tracked {.noinline.} =
  if n > 0:
    result = Tracked(id: 1)
    result = Tracked(id: 2)   # destroys the first value
  else:
    result = Tracked(id: 3)

proc looped(n: int): Tracked {.noinline.} =
  result = Tracked(id: -1)
  var i = 0
  while i < n:
    result = Tracked(id: i)   # each pass destroys the value before it
    inc i

proc defaulted(n: int) {.noinline.} =
  var r = default(Tracked)
  if n > 0:
    r = Tracked(id: n)       # r still holds its default: nothing to destroy
  echo r.id

proc main =
  block:
    let a = branchy(1)
    let b = branchy(-1)
    echo a.id, " ", b.id
  echo "branchy: ", destroyed       # a and b
  destroyed = 0
  block:
    let a = threeWay(0)
    let b = threeWay(5)
    echo a.id, " ", b.id
  echo "threeWay: ", destroyed      # a and b
  destroyed = 0
  block:
    let a = twice(1)
    echo a.id
  echo "twice: ", destroyed         # the first value, then a
  destroyed = 0
  block:
    let a = looped(3)
    echo a.id
  echo "looped: ", destroyed        # the start value, passes 0 and 1, then a
  destroyed = 0
  defaulted(4)
  echo "defaulted: ", destroyed     # r's value, once

main()

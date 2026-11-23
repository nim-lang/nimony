# seq `&` moves the elements of its `sink` operands bitwise: no `move`, so no
# `HasDefault`, each element destroyed exactly once.

import std/syncio

var destroyed = 0
type
  R = object
    id: int     # 0 = owns nothing
  Node = ref object
    v: int

proc `=destroy`(x: R) =
  if x.id != 0:
    {.cast(assumeSync).}:
      inc destroyed
proc `=wasMoved`(x: var R) = x.id = 0
proc `=copy`(d: var R; s: R) {.error.}

proc dcount(): int =
  {.cast(assumeSync).}:
    result = destroyed

proc main =
  block:
    var a: seq[R] = @[]
    a.add R(id: 1)
    a.add R(id: 2)
    var b: seq[R] = @[]
    b.add R(id: 3)
    let c = a & b
    echo c.len, " ", c[0].id, c[1].id, c[2].id, " destroyed=", dcount()
    let d = c & R(id: 4)
    echo d.len, " ", d[3].id, " destroyed=", dcount()
    let e = R(id: 5) & d
    echo e.len, " ", e[0].id, e[4].id, " destroyed=", dcount()
  echo "after scope destroyed=", dcount()
  # `Node` (a `ref`, not `nil ref`) has no default: `&` must still work
  let n = @[Node(v: 1)] & @[Node(v: 2)]
  echo n[0].v + n[1].v
  let s = @["a", "b"] & "c"
  echo s.len, s[2]

main()

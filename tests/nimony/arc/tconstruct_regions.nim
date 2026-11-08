# `construct` regions (`doc/internals/multi_join_ir.md`): an object
# constructor with a call among its values is split into one store per field.
# Fields keep source order, a field that may raise is evaluated before the
# region, destructible fields are owned exactly once, and a large `ref` object
# is built in its heap block, not on the stack first (#2846).

{.feature: "assumeSync".}

import std/[syncio, assertions]

var log = ""
var destroyed = 0

type
  Tracked = object
    id: int

proc `=destroy`(x: Tracked) =
  if x.id != 0:
    inc destroyed

proc `=wasMoved`(x: var Tracked) =
  x.id = 0

proc `=copy`(dest: var Tracked; src: Tracked) {.error.}

proc `=dup`(x: Tracked): Tracked {.error.}

proc mk(id: int; tag: string): Tracked =
  log.add tag
  Tracked(id: id)

proc num(n: int; tag: string): int =
  log.add tag
  n

type
  Pair = object
    a: Tracked
    n: int
    b: Tracked

  Outer = object
    s: string
    p: Pair

  Big = ref object
    data: array[2_000_000, int]
    tail: int

proc order() =
  log = ""
  let p = Pair(a: mk(1, "a"), n: num(2, "n"), b: mk(3, "b"))
  assert p.a.id == 1 and p.n == 2 and p.b.id == 3
  echo log

proc nested() =
  log = ""
  let o = Outer(s: $num(7, "s"), p: Pair(a: mk(4, "a"), n: 5, b: mk(6, "b")))
  assert o.s == "7" and o.p.a.id == 4 and o.p.n == 5 and o.p.b.id == 6
  echo log

proc mayFail(fail: bool): int {.raises.} =
  log.add "r"
  if fail: raise Failure
  result = 9

proc raising(fail: bool) {.raises.} =
  log = ""
  let p = Pair(a: mk(10, "a"), n: mayFail(fail), b: mk(11, "b"))
  assert p.n == 9
  echo log

proc asArgument(p: Pair): int = p.a.id + p.b.id

type
  Named = ref object
    name: string
    kids: seq[Tracked]

proc longName(i: int): string = "node-" & $i & "-with-a-name-long-enough-for-the-heap"

proc refFields() =
  # the payload is uninitialized heap memory: its stores must not destroy what
  # was there before
  var total = 0
  for i in 0 ..< 100:
    let n = Named(name: longName(i), kids: @[mk(i + 1, "")])
    total += n.name.len + n.kids[0].id
  echo total

proc big() =
  let n = Big(tail: num(3, ""))
  echo n.data[0], " ", n.data[1_999_999], " ", n.tail

proc main() =
  order()
  nested()
  try:
    raising(false)
    raising(true)
  except:
    echo "raised after ", log
  echo asArgument(Pair(a: mk(20, ""), n: 0, b: mk(22, "")))
  refFields()
  big()

destroyed = 0
main()
echo destroyed

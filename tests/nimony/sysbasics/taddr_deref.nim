# `addr` of a location reached through a dereference: planetis-m/nimony-bugs-fuzzers#80
import std/syncio
type
  Obj = object
    x: int
  Node = ref Obj
proc viaDeref(n: Node): ptr int = addr n[].x
proc viaPtr(p: ptr Obj): ptr int = addr p[].x
proc whole(p: ptr Obj): ptr Obj = addr p[]
proc id(p: ptr Obj): ptr Obj = p
let n = Node(x: 1)
viaDeref(n)[] = 5
var o = Obj(x: 1)
viaPtr(addr o)[] = 6
whole(addr o).x += 1
(addr id(addr o).x)[] += 1    # the pointer comes from a call
(addr id(addr o)[])[].x += 1
echo n.x, " ", o.x

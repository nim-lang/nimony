import std/syncio

# Regression: the eraiser's `canRaise` temp for a raising call in expression
# position must OWN the returned value. As a CursorS the duplifier could only
# `=dup` out of it (a cursor is a borrow) and the destroyer never released it,
# so every `dest = raisingCall()` leaked the entire returned value — found as
# a JSON parser leaking its full DOM once per parse (~3.6MB/call at scale).
# `let t = raisingCall(); dest = t` was always balanced; only the direct
# assignment form leaked.

type
  Payload = object
    id: int
  Node = ref object
    p: Payload
  Holder = object
    p: Payload

var destroyed = 0

proc `=wasMoved`(x: var Payload) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`(v: Payload) =
  destroyed = destroyed + 1

proc mkNode(id: int): Node {.raises.} =
  result = Node(p: Payload(id: id))

proc wrap(h: Holder): Node {.raises.} =
  result = Node(p: Payload(id: h.p.id))

proc consumeDirect(): Node {.raises.} =
  # raising call directly in `result =` position
  result = mkNode(1)

proc run() {.raises.} =
  block:
    var n = consumeDirect()
    var m = Node(p: Payload(id: 2))
    # raising call re-assigned into a pre-existing location:
    m = mkNode(3)
    discard n
    discard m
  # block end: n's payload (1), m's old (2) and new (3) payloads must all
  # have been destroyed exactly once.
  echo "destroyed=", destroyed
  block:
    # #285: the argument is a constructor that moves a local (its last use),
    # so the duplifier binds the call to a temp before the deferred
    # `=wasMoved`; that temp must have the success tuple's type.
    let p = Payload(id: 4)
    let k = wrap(Holder(p: p))
    echo "wrapped=", k.p.id
  # +3: `p` (moved-from), the `Holder` and k's own payload
  echo "destroyed=", destroyed

try:
  run()
except:
  echo "unexpected raise"

{.feature: "assumeSync".}  # test program: globals shared freely

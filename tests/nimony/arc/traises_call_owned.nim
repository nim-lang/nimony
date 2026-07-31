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

var destroyed = 0

proc `=destroy`(v: Payload) =
  destroyed = destroyed + 1

proc mkNode(id: int): Node {.raises.} =
  result = Node(p: Payload(id: id))

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

try:
  run()
except:
  echo "unexpected raise"

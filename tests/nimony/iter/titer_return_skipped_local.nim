# An inline iterator's `return` jumps past locals the iterator declares after
# it. Those locals were never initialized on that path, so they must not be
# destroyed there. The return label used to sit inside the expansion's scope,
# and the destroyer then destroyed every local of that scope after it:
# `std/ioring`'s `slotsForFd` crashed `closeFd` on the native backend that way.

import std/[syncio, assertions]

type
  Probe = object
    id: int

var destroyed = 0

proc `=destroy`(p: Probe) =
  if p.id != 0: inc destroyed
  else: inc destroyed, 1000 # never constructed: an uninitialized local

proc `=wasMoved`(p: var Probe) = p.id = 0
proc `=copy`(dest: var Probe; src: Probe) {.error.}

iterator items(stop: bool): int =
  if stop: return
  var p = Probe(id: 1)
  yield p.id
  var q = Probe(id: 2)
  yield q.id

proc collect(stop: bool): int =
  result = 0
  for x in items(stop):
    result += x

assert collect(true) == 0
assert destroyed == 0, "early return destroyed locals it never reached"
assert collect(false) == 3
assert destroyed == 2
echo "iterator return skipped locals: ", destroyed

{.feature: "assumeSync".}  # test program: single-threaded counter

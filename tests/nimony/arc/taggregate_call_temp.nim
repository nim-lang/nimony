## A call in an aggregate whose sibling is complex is bound to a temp by
## `xelim`. That temp used to be a `cursor`, so the duplifier could not move out
## of it: the aggregate got a `=dup` and the call's own result leaked (and a
## type whose `=dup` is `.error` failed to compile). Checked by valgrind and by
## counting.

import std / [syncio, assertions]

type
  Payload = object
    items: seq[string]

  Holder = object
    p: Payload
    note: int

proc mk(s: string): Payload = Payload(items: @[s, s & s])

var created = 0
var destroyed = 0

type
  Counted = object
    x: int

proc `=destroy`(c: Counted) =
  if c.x != 0: inc destroyed
proc `=wasMoved`(c: var Counted) = c.x = 0
proc `=dup`(c: Counted): Counted {.error.}
proc `=copy`(a: var Counted; b: Counted) {.error.}

proc mkCounted(): Counted =
  inc created
  Counted(x: 1)

type
  CountedHolder = object
    c: Counted
    note: int

proc main(flag: bool) =
  for i in 0 ..< 3:
    let h = Holder(p: mk("ab"), note: (if flag: i else: -i))
    echo h.p.items[1], " ", h.note
  block:
    let ch = CountedHolder(c: mkCounted(), note: (if flag: 1 else: 2))
    echo ch.note

main(true)
assert created == 1
assert destroyed == 1
echo "OK"

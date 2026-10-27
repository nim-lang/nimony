# A call that mutates a pointer's target leaves the pointer non-nil (#2809).

import std/syncio

type
  Obj = object
    x: int

proc touch(value: var int) = inc value

proc receive(input: nil ptr int) =
  if input == nil: return
  let receiver = input
  touch(receiver[])
  touch(receiver[])

proc receiveObj(o: nil ref Obj) =
  if o == nil: return
  touch(o.x)
  touch(o.x)
  touch(o[].x)

var value = 1
receive(addr value)
echo value

var o = (ref Obj)(x: 1)
receiveObj(o)
echo o.x

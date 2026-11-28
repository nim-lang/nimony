# `return expr` is `result = expr; return result`: the old `result` is destroyed.
{.feature: "assumeSync".}
import std/syncio

type
  Obj = object
    id: int

var destroyed = 0
proc `=destroy`(o: Obj) =
  if o.id != 0: inc destroyed
proc `=wasMoved`(o: var Obj) = o.id = 0

proc mk(i: int): Obj = Obj(id: i)

proc retValue(): Obj =
  result = mk(1)
  return mk(2)

proc retLocal(): Obj =
  result = mk(1)
  let x = mk(2)
  return x

proc retUnassigned(c: bool): Obj =
  if c:
    return mk(2) # `result` holds nothing yet
  result = mk(1)

template check(call: untyped) =
  destroyed = 0
  block:
    let r: Obj = call
    echo r.id
  echo destroyed

check retValue()
check retLocal()
check retUnassigned(true)

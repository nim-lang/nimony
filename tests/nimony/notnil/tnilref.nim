
import std / syncio

type
  Myref = ref object
    x: int

proc p(): Myref not nil {.raises.} = Myref(x: 5)

proc hah2 {.raises.} =
  var r: Myref not nil
  r = p()
  echo r[].x

proc other =
  var r: nil Myref = nil
  if r != nil:
    echo r[].x
  else:
    echo "is nil"

proc touch(value: var int) = inc value

proc receive(input: nil ptr int) =
  # a call mutating the target keeps the pointer non-nil (#2809)
  if input == nil: return
  let receiver = input
  touch(receiver[])
  touch(receiver[])

proc receiveRef(o: nil Myref) =
  if o == nil: return
  touch(o.x)
  touch(o.x)
  touch(o[].x)

try:
  hah2()
  other()
  var value = 1
  receive(addr value)
  echo value
  var o = Myref(x: 1)
  receiveRef(o)
  echo o.x
except:
  echo "god riddance"

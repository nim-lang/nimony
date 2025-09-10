
import std / syncio

type
  Myref = ref object
    x: int

proc p(): Myref not nil {.raises.} = Myref(x: 5)

proc hah2 {.raises.} =
  var r: nil Myref
  r = p()
  echo r[].x

proc other =
  var r: nil Myref = nil
  if r != nil:
    echo r[].x
  else:
    echo "is nil"

try:
  hah2()
  other()
except:
  echo "god riddance"


import std / syncio

type
  Myref = ref object
    x: int

proc p(): Myref not nil {.raises.} = Myref(x: 5)

proc hah {.raises.} =
  var r: Myref not nil
  r = p()
  echo r[].x

try:
  hah()
except:
  echo "god riddance"

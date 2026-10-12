# The value of an imported `.raises` call lives in the coroutine frame across a
# passive call: the frame field and its temp both have the success-tuple type,
# which the caller only knows once the callee's signature is lowered (#2642).
import std/syncio
import deps / mraisingvalue

proc step() {.passive.} = discard

proc caller(x: int) {.passive.} =
  try:
    let n = value(x)
    step()
    echo n
  except ErrorCode:
    echo "failed"

complete delay(caller(21))
complete delay(caller(-1))

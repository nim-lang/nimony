# under `ownedRefs` the constructed exception is `owned`; `raise` takes it
{.feature: "ownedRefs".}
import std/syncio

type
  IOError = ref object of Exception
    code: int

proc inner() {.raises: IOError.} =
  raise IOError(msg: "inner", code: 1)

proc outer() {.raises: IOError.} =
  try:
    inner()
  except IOError as e:
    echo "caught: ", e.msg, " code=", e.code
    raise IOError(msg: "wrapped: " & e.msg, code: e.code + 100)

try:
  outer()
except IOError as e2:
  echo "top: ", e2.msg, " code=", e2.code

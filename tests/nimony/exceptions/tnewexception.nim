import std/syncio

# Nim 2 interop: `raise newException(...)` with a ref-based hierarchy.
type
  CatchableError = object of Exception
  ValueError = object of CatchableError

proc p() {.raises: ref Exception.} =
  raise newException(ValueError, "wrong value")

try:
  p()
except ref ValueError as e:
  echo "caught: ", e.msg

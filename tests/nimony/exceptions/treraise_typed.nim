import std/syncio

# A typed `raise X(...)` fired from inside an except handler must propagate
# PAST the enclosing try, not loop back into the same handler. nifcgen pops
# the try's exception label for the duration of each except-arm body so a
# nested raise targets the next-outer handler.

type
  IOError = ref object of Exception
    code: int

proc inner() {.raises: IOError.} =
  raise IOError(msg: "inner", code: 1)

proc outer() {.raises: IOError.} =
  try:
    inner()
  except IOError as e:
    echo "outer-caught: ", e.msg, " code=", e.code
    raise IOError(msg: "wrapped: " & e.msg, code: e.code + 100)

try:
  outer()
except IOError as e2:
  echo "top-caught: ", e2.msg, " code=", e2.code

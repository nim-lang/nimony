import std/syncio

# A bare `raise` inside an except handler re-raises the in-flight exception.
# The exception itself lives in the threadvar `exc`; nifcgen's pop trick
# routes the goto past this handler's own label so the outer try receives it.

type
  IOError = ref object of Exception
    code: int

proc inner() {.raises: IOError.} =
  raise IOError(msg: "inner", code: 7)

proc outer() {.raises: IOError.} =
  try:
    inner()
  except IOError as e:
    echo "outer-saw: ", e.msg, " code=", e.code
    raise  # re-raise the same exception

try:
  outer()
except IOError as e2:
  echo "top-caught: ", e2.msg, " code=", e2.code

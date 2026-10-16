# An `except` arm is a type filter: an `ErrorA` is not caught by `except
# ErrorB`, and in a routine that does not raise it has nowhere to go (#2680).

type
  ErrorA = ref object of Exception
  ErrorB = ref object of Exception

proc foo(): int {.raises: ErrorA.} =
  raise ErrorA(msg: "a")

proc test() =
  try:
    discard foo()
  except ErrorB:
    discard

test()

# Nothing translates an `ErrorCode` into a `ref` exception or back (#2679):
proc code() {.raises.} = raise IOError

proc refFromCode() {.raises: ErrorA.} =
  code()

proc codeFromRef() {.raises.} =
  discard foo()

# `ErrorCode` arms catch only `ErrorCode`:
proc test2() =
  try:
    discard foo()
  except ErrorCode:
    discard

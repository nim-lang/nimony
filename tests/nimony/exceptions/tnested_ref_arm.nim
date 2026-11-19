# A `try` nested in a typed `ref` arm of a non-raising routine (#3044 item 9).
import std/syncio

type
  Failed = ref object of Exception

proc step(n: int) {.raises: Failed.} =
  if n > 1: raise Failed(msg: "step")

proc stop(n: int): int =
  var failures = 0
  try:
    step(n)
  except Failed:
    inc failures
    try:
      step(n - 1)
    except Failed:
      inc failures
  result = failures

echo stop(5)

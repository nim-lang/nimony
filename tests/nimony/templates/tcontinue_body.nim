# An untyped argument is checked before template expansion introduces its loop
import std/assertions

template repeat(body: untyped) =
  var remaining = 3
  while remaining > 0:
    dec remaining
    body
    assert false

template repeatUntyped(body: untyped) {.untyped.} =
  repeat:
    body

type RetryError = ref object of Exception

template retryOn(attempts: int; errorKind, body: untyped) {.untyped.} =
  var remaining = attempts
  while remaining > 0:
    dec remaining
    try:
      body
      break
    except errorKind:
      if remaining == 0: raise

template retry(attempts: int; body: untyped) =
  retryOn(attempts, RetryError):
    body

proc test() {.raises: ref Exception.} =
  var visits = 0
  repeat:
    inc visits
    continue
  assert visits == 3

  repeatUntyped:
    inc visits
    continue
  assert visits == 6

  retry 3:
    inc visits
    continue
  assert visits == 9

try:
  test()
except:
  assert false

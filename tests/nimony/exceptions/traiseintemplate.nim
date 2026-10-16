# A `raise` inside a template body. The body is checked where the template is
# declared, but whether a `raise` is allowed depends on the routine it is
# EXPANDED into; the declaration-time check used to write an error node into
# the stored body, and every expansion then crashed sem.
import std/syncio

type Conn = object
  failure: int

template fail(c: var Conn; why: int; e: ErrorCode) =
  if c.failure == 0: c.failure = why
  raise e

template fail(c: var Conn; why: int) = fail(c, why, ProtocolError)

proc handshake(c: var Conn; step: int) {.raises.} =
  if step == 2: c.fail(7)
  if step == 3: c.fail(8, TimeoutError)

proc main =
  var c = Conn(failure: 0)
  for step in 1 .. 3:
    try:
      handshake(c, step)
      echo "step ", step, " ok"
    except ErrorCode as e:
      echo "step ", step, ": ", e, " failure=", c.failure

main()

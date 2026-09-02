# `.passive` and `.raises` together: a coroutine that fails.
#
# The two halves of the raise lowering sit on either side of the coroutine cut.
# The control-flow half is ahead of it — `eraiser` has already turned every
# raising call into a temp plus a `(failed t)` check — which is why a try inside
# a passive proc has worked all along (see `ttry.nim`). The TYPE half is behind
# it: `lengcgen` is what turns a `.raises` signature into `ErrorCode` or
# `(ErrorCode, T)`, and by then `cps` has long since built the frame and the
# state procs from the types the routine was written with.
#
# So nothing here is about control flow. Everything tested below is a VALUE
# crossing that seam: the temp holding a failed call's result, which outlives
# the state that made the call and therefore has to be a frame field of the
# right type; and the coroutine's own error, which travels in the frame's
# result slot rather than in a state proc's return — that slot belongs to the
# `Continuation` the trampoline runs next.

import std / syncio

proc step() {.passive.} = discard

# --- a void raising coroutine: the frame's result slot is a bare ErrorCode ---

proc failVoid(x: int) {.passive, raises.} =
  step()                      # suspend BEFORE the raise: the cut is here
  if x < 0:
    raise SyntaxError
  echo "failVoid ok ", x

proc catchVoid() {.passive.} =
  try:
    failVoid(1)
    failVoid(-1)
    echo "unreachable"
  except:
    echo "catchVoid: caught"

catchVoid()

# --- a value-returning one: the slot is (ErrorCode, T) and `result` is its
#     second half ---

proc failInt(x: int): int {.passive, raises.} =
  step()
  if x < 0:
    raise SyntaxError
  result = x * 10

proc catchInt() {.passive.} =
  try:
    let a = failInt(4)
    echo "failInt gave ", a
    let b = failInt(-1)
    echo "unreachable ", b
  except:
    echo "catchInt: caught"

catchInt()

# --- two coroutine boundaries: the error is re-raised by a passive proc that
#     is itself passive-called ---

proc middle(x: int): int {.passive, raises.} =
  let v = failInt(x)          # a raising PASSIVE call inside a raising one
  step()
  result = v + 1

proc catchNested() {.passive.} =
  try:
    let m1 = middle(2)
    echo "middle gave ", m1
    let m2 = middle(-1)
    echo "unreachable ", m2
  except:
    echo "catchNested: caught"

catchNested()

# --- cleanup still runs on the way out ---

proc catchWithFinally() {.passive.} =
  try:
    failVoid(-1)
    echo "unreachable"
  except:
    echo "catchWithFinally: caught"

  var done = false
  try:
    step()
    echo "catchWithFinally: body"
  finally:
    done = true
  echo "catchWithFinally: finally ran = ", done

catchWithFinally()

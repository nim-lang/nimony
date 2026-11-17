
import std / [syncio]

proc foo(r: bool) {.raises.} =
  echo "hi"
  if r:
    raise SyntaxError

try:
  foo(true)
except:
  echo "caught"
finally:
  echo "A finally"

try:
  foo(false)
except:
  echo "you are not supposed to see this"
finally:
  echo "B finally"


type
  MyObj = object
    value: int

# Custom destructor
proc `=wasMoved`(x: var MyObj) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`(x: var MyObj) =
  echo "Destructor called for MyObj(value=", x.value, ")"

proc main() =
  try:
    var o = MyObj(value: 123)
    echo "Object initialized"
  finally:
    echo "Inside finally block"

main()

# A raise in the `finally` a `return` runs leaves the `try`: its own `except`
# does not catch it.
proc finRaisesOnReturn() {.raises.} =
  try:
    return
  except:
    echo "own except: must not run"
  finally:
    echo "C finally"
    raise SyntaxError

try:
  finRaisesOnReturn()
except:
  echo "caught outside"

# A `return` in an outer `finally` does not rerun the inner `finally` that the
# first `return` already ran.
proc returnInOuterFin() =
  try:
    try:
      return
    finally:
      echo "D inner finally"
  finally:
    return

returnInOuterFin()

# `return x` stores `x` in `result` before the `finally` runs.
proc returnThroughFin(): int =
  try:
    return 1
  finally:
    echo "E finally sees ", result
    result = result + 10

echo returnThroughFin()
echo "Program end"

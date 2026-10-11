# `result = passiveCall()` inside a raising coroutine.
#
# `cps` ends a state where it finds a suspension point at the ROOT of a
# statement. The eraiser used to rewrite `result = v` into
# `result = (Success, v)`, which pushed the passive call one level down: the
# state boundary then fell after the whole assignment, and the routine either
# returned 0 or did not compile at all (`cps.nim: state != -1`).

import std / syncio

proc step() {.passive.} = discard

proc get(x: int): int {.passive.} =
  step()
  result = x * 10

proc viaResult(x: int): int {.passive, raises.} =
  if x < 0: raise SyntaxError
  result = get(x)

proc tail(x: int): int {.passive, raises.} =
  if x < 0: raise SyntaxError
  get(x)

proc viaReturn(x: int): int {.passive, raises.} =
  if x < 0: raise SyntaxError
  return get(x)

proc reassign(x: int): int {.passive, raises.} =
  var y = viaResult(x)  # `y` holds the success tuple
  y = get(y)            # a passive, non-raising call into it
  result = y

proc raiseAfter(x: int): int {.passive, raises.} =
  result = get(x)
  if result > 30: raise SyntaxError

proc main() {.passive.} =
  for i in [3, -1]:
    try:
      echo viaResult(i)
      echo tail(i)
      echo viaReturn(i)
      echo reassign(i)
      echo raiseAfter(i)
    except:
      echo "caught ", i
  try:
    echo raiseAfter(5)
  except:
    echo "caught 5"

main()

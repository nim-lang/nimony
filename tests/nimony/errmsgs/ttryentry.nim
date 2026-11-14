# What is known inside an `except` or `finally` is what holds on *every* way
# into it. A handler can be entered from any point of the `try` body that can
# raise, a call to a `.raises` routine included, and a `finally` on every way
# out of the `try`. Every assertion here is false on some run. The proven side
# is `tests/nimony/contracts/texitjoins_ok.nim`.

proc risky(x: int) {.raises.} =
  if x > 3: raise Failure

proc inExcept(x: int): int =
  var y = 0
  result = 0
  try:
    y = x
    risky(x)                    # raises with `y == x`
    y = 0
  except:
    {.assert: y == 0.}

proc inFinally(x: int): int {.raises.} =
  var y = 0
  result = 0
  try:
    y = x
    risky(x)
    y = 0
  finally:
    {.assert: y == 0.}

proc raiseInIf(x: int): int {.raises.} =
  var y = 0
  result = 0
  try:
    y = 5
    if x > 3: raise Failure
    y = 0
  except:
    {.assert: y == 0.}

proc finallyAfterReturn(x: int): int =
  var y = 0
  try:
    y = x
    return 1
  finally:
    {.assert: y == 0.}          # the `finally` runs on the `return` too

proc raiseInHandler(x: int): int {.raises.} =
  var y = 0
  result = 0
  try:
    risky(x)
  except:
    y = x
    risky(x)                    # leaves the handler for the `finally`
    y = 0
  finally:
    {.assert: y == 0.}

discard inExcept(9)
discard finallyAfterReturn(9)

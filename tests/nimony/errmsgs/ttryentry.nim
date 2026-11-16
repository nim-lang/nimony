# What is known inside an `except` or `finally` is what holds on *every* way
# into it. A handler can be entered from any point of the `try` body that can
# raise, a call to a `.raises` routine included, and a `finally` on every way
# out of the `try`. Every claim here is false, and every read of `y` unset,
# on some run. The proven side is `contracts/texitjoins_ok.nim`.

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

proc finallyReadsBeforeInit(x: int): int =
  var y: int
  result = 0
  try:
    if x > 3: return 1
    y = x
  finally:
    result = y                  # `y` is not initialized on the `return`

discard inExcept(9)
discard finallyAfterReturn(9)

# An exit through a `finally` runs it on the way out: what the `finally` may
# write is not known on a `break` or `return` through it, and a `return` is
# judged against the `.ensures` without it. Each claim below is false.

proc breakThroughFinally(): int =
  var i = 0
  block b:
    try:
      i = 0
      break b
    finally:
      i = 100
  {.assert: i == 0.}
  result = i

proc returnThroughFinally(): int {.ensures: result == 0.} =
  try:
    result = 0
    return
  finally:
    result = 100

proc deferAfterReturn(): int {.ensures: result >= 0 and result < 8.} =
  defer: result = 1000000000
  result = 1

proc callerTrustsEnsures() =
  let k = deferAfterReturn()
  {.assert: k < 8.}               # rests on the `.ensures` above
  discard k

type Box = object
  f: int

proc returnObjectThroughFinally(): Box {.ensures: result.f == 0.} =
  try:
    return Box(f: 0)
  finally:
    result.f = 100

discard breakThroughFinally()
discard returnThroughFinally()
callerTrustsEnsures()
discard returnObjectThroughFinally()

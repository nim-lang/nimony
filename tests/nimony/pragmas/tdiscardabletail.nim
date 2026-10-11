import std/syncio

# Regression: `{.discardable.}` was not honoured when the call is the TAIL
# expression of a void proc. `processBodyStatements` semchecks a body's last
# statement with `semExpr`, so `handleProcReturnType` fell through to
# `commonType` against `void` and reported
#   type mismatch: got: int but wanted: void
# without ever consulting the pragma. The same call followed by any other
# statement always compiled, because `semStmt` applies the discard rule — an
# asymmetry with no stated reason. A one-line registration proc
# (`proc registerFoo() = addHandler(foo)`) is exactly this shape.

proc f(x: int): int {.discardable.} =
  echo "f ", x
  result = x + 1

proc tailOnly() =
  f(1)                  # the tail of a void proc — the regression

proc tailAfterOther() =
  f(2)
  echo "after"          # never broke: not the tail

proc tailInIf(cond: bool) =
  if cond:
    f(3)                # still the proc's tail, nested in control flow

proc tailInWhile() =
  var i = 0
  while i < 1:
    f(4)
    i = i + 1

proc usesResult(): int =
  f(5)                  # a NON-void proc's tail must still become `result`

tailOnly()
tailAfterOther()
tailInIf(true)
tailInWhile()
echo usesResult()

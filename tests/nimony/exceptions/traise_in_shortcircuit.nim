import std/syncio

# A raising call inside the body guarded by a short-circuit `and`/`or`:
# `xelim` turns the condition into a `(scope ...)` with `jmp` guards, and the
# eraiser used to hoist its `canRaise` temp out of that scope, in front of the
# locals the call reads. https://github.com/nim-lang/nimony/issues/2345

proc f(x: int): int {.raises.} =
  if x > 100:
    raise ValueError
  result = x * 2

proc small(x: int): bool = x < 10

proc runAnd(x: int) {.raises.} =
  if x > 0 and small(x):
    var y = 21
    echo f(y)

proc runOr(x: int) {.raises.} =
  if x < 0 or small(x):
    var y = 200
    echo f(y)

proc nested(x: int) {.raises.} =
  if x > 0 and small(x):
    var y = 3
    if y > 0 and small(y):
      var z = y + 4
      echo f(z)

try:
  runAnd(5)
  nested(5)
  runOr(5)
  echo "unreachable"
except ErrorCode:
  echo "raised"

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

# A branch that leaves through `raise`, `break` or `continue` has no value;
# the other branches type the if/case expression or the try's handler.

proc pick(v: int): int {.raises.} =
  let a = if v == 0: raise ValueError else: 4
  let b = case v
    of 1: raise ValueError
    else: a
  result = try: b except: raise ValueError

proc loop() =
  for i in 0..5:
    let a = if i < 3: i else: break
    let b = case i
      of 1: continue
      else: i * 10
    echo a, " ", b

try: echo pick(2) except ErrorCode as e: echo "unexpected ", e
try: discard pick(0) except ErrorCode as e: echo "if ", e
try: discard pick(1) except ErrorCode as e: echo "case ", e
loop()

# Loop rotation (nimony#2984): every `while` becomes a guarded bottom-tested
# loop, `(ite C (stmts (loop (stmts B (continue C))))`, and the guard is the
# pre-header CSE hoists loop-invariant loads into. Two ways that hoisting could
# introduce a trap the program does not have:
#
# * the loop runs zero times — the guard is false, nothing hoisted runs;
# * the load sits after a way out of the body (`if p == nil: break`) — it does
#   not run on every iteration, so CSE must not hoist it at all.
#
# The bottom test of a float condition reads NaN-aware flags: `<`, `<=`, `==`
# repeat only on an ordered pair, `!=` also on an unordered one. Each loop below
# also runs with a NaN, which is where a wrong condition code shows.
#
# And one way the rotation itself could go wrong: the statements that compute
# the condition (a `let` in it, a call bound to a temp) run again at the bottom
# of the loop, and they must write the SAME variables the body reads.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_rotate.nim
import std/[syncio, cmdline]

type Obj = object
  f: int

proc zeroTrip(p: ptr Obj; n: int): int {.noinline.} =
  result = 0
  var i = 0
  while i < n:
    result += p.f
    inc i

proc exitFirst(p: ptr Obj; n: int): int {.noinline.} =
  result = 0
  var i = 0
  while i < n:
    if p == nil: break
    result += p.f
    inc i

proc withContinue(n: int): int {.noinline.} =
  result = 0
  var i = 0
  while i < n:
    inc i
    if i == 3: continue
    if i == 7: break
    result += i

proc letInCond(n: int): int {.noinline.} =
  result = 0
  var x = n
  while (let s = x; s > 0):
    result += s
    dec x

proc below(i, n: int): bool {.noinline.} = i < n

proc callInCond(n: int): int {.noinline.} =
  result = 0
  var i = 0
  while below(i, n):
    result += i
    inc i

proc floatLt(n: float): int {.noinline.} =
  result = 0
  var x = 0.0
  while x < n:
    x += 1.0
    inc result

proc floatLe(n: float): int {.noinline.} =
  result = 0
  var x = 0.0
  while x <= n:
    x += 1.0
    inc result

proc floatEq(x: float): int {.noinline.} =
  result = 0
  var y = x
  while y == x:
    inc result
    if result == 3: y = x + 1.0

proc floatNeq(start: float): int {.noinline.} =
  result = 0
  var x = start
  while x != 3.0:
    x += 1.0
    inc result
    if result == 10: break

let none = cast[ptr Obj](paramCount() * 0)
let nan = 0.0 / float(paramCount())
var o = Obj(f: 5)
echo zeroTrip(none, paramCount())
echo zeroTrip(addr o, paramCount() + 3)
echo exitFirst(none, paramCount() + 3)
echo exitFirst(addr o, paramCount() + 3)
echo withContinue(paramCount() + 10)
echo letInCond(paramCount() + 3)
echo callInCond(paramCount() + 5)
echo floatLt(3.0), " ", floatLt(nan)
echo floatLe(3.0), " ", floatLe(nan)
echo floatEq(2.0), " ", floatEq(nan)
echo floatNeq(0.0), " ", floatNeq(nan)

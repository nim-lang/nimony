# Loop rotation (nimony#2984): every `while` becomes a guarded bottom-tested
# loop, `(ite C (stmts (loop (stmts B (continue C))))`, and the guard is the
# pre-header CSE hoists loop-invariant loads into. Two ways that hoisting could
# introduce a trap the program does not have:
#
# * the loop runs zero times — the guard is false, nothing hoisted runs;
# * the load sits after a way out of the body (`if p == nil: break`) — it does
#   not run on every iteration, so CSE must not hoist it at all.
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

let none = cast[ptr Obj](paramCount() * 0)
var o = Obj(f: 5)
echo zeroTrip(none, paramCount())
echo zeroTrip(addr o, paramCount() + 3)
echo exitFirst(none, paramCount() + 3)
echo exitFirst(addr o, paramCount() + 3)
echo withContinue(paramCount() + 10)
echo letInCond(paramCount() + 3)
echo callInCond(paramCount() + 5)

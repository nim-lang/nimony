import std/syncio

# A checked operation whose operand is itself a checked operation. Each one
# becomes its own `keepovf` statement, and `overflowFlag()` reports the last of
# them: the outermost operation, which is evaluated after its operands.

proc madd(a, b, c: int): int =
  {.keepOverflowFlag.}:
    result = a + b * c
    if overflowFlag(): result = -1

proc msub(a, b, c: int): int =
  {.keepOverflowFlag.}:
    let r = a * b - c
    if overflowFlag(): result = -1
    else: result = r

proc mboth(a, b, c, d: int): int =
  {.keepOverflowFlag.}:
    var r = 0
    r = a * b + c * d
    if overflowFlag(): result = -1
    else: result = r

proc deep(a, b, c, d: int): int =
  {.keepOverflowFlag.}:
    result = (a + b) * (c - d) - a * (b + c * d)

proc id(x: int): int = x

proc viaCall(a, b, c: int): int =
  {.keepOverflowFlag.}:
    result = a + id(b * c)

proc inCond(a, b, c: int): string =
  {.keepOverflowFlag.}:
    if a * b + c > 10: result = "big"
    else: result = "small"

proc inLoop(n: int): int =
  var i = 0
  result = 0
  {.keepOverflowFlag.}:
    while i * 2 + 1 < n:
      result = result + i * i
      i = i + 1

proc retDirect(a, b, c: int): int =
  {.keepOverflowFlag.}:
    return a * b + c

proc gen[T](a, b, c: T): T {.untyped.} =
  {.keepOverflowFlag.}:
    result = a + b * c

proc innerOnly(a, b, c: int): (int, bool) =
  {.keepOverflowFlag.}:
    let r = a - b * c
    var f = false
    if overflowFlag(): f = true
    result = (r, f)

proc condOperand(c: bool; a, b, d: int): (int, bool) =
  {.keepOverflowFlag.}:
    let r = (if c: a * b else: 0) + d
    var f = false
    if overflowFlag(): f = true
    result = (r, f)

proc show(t: (int, bool)) = echo t[0], " ", t[1]

template tmpl(a, b, c: int): int =
  var r = 0
  {.keepOverflowFlag.}:
    r = a - b * c
  r

echo madd(1, 2, 3)
# The inner `*` wraps, the outer `+` does not overflow: the flag is clear.
echo madd(0, high(int), 2)
# The inner `*` fits, the outer `+` overflows.
echo madd(high(int), 1, 1)
echo msub(3, 4, 5)
echo msub(low(int), 1, 1)
echo mboth(2, 3, 4, 5)
echo mboth(high(int), 1, 1, 1)
echo deep(3, 4, 10, 2)
echo viaCall(1, 2, 3)
echo inCond(3, 4, 0), " ", inCond(1, 1, 1)
echo inLoop(10)
echo retDirect(2, 3, 4)
echo gen(1, 2, 3)
echo tmpl(10, 2, 3)
# The flag itself: only the inner `*` wraps, so it is clear ...
show innerOnly(0, high(int), 2)
# ... and set when the outer `-` overflows.
show innerOnly(low(int), 1, 1)
# An operand that is a conditional holding a checked operation.
show condOperand(true, high(int), 2, 1)
show condOperand(false, high(int), 2, 1)
show condOperand(true, 1, 2, high(int))

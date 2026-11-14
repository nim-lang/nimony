# Induction variables, by target (`--cpu`, `Target.scaledIndex`):
#
# * x86/ARM address `a[i]` in one operand, so a loop whose counter only indexes
#   and tests COUNTS TO ZERO: `j = i - n`, `a[i]` becomes `(pat q j)` with
#   `q = addr a[n]`, the test `(op j 0)`. Any other loop is left alone.
# * Everything else (RISC-V, ...) walks a pointer per array instead.
#
# Each loop below hits one side of the rule on x86/ARM:
#
# * the counter only indexes (`<`, `<=`, `!=` tests, two arrays, start ≠ 0);
# * the counter is read after the loop, or in the body as a value — untouched;
# * a zero-trip loop — the guard is false, nothing hoisted runs.

import std/syncio

proc ltSum(k: int): int {.noinline.} =
  var arr = default(array[100, int])
  for i in 0..<100:
    arr[i] = i * k                   # counter read as a value: stays
  result = 0
  for i in 0..<100:
    result += arr[i]                 # only indexes: counter removed

proc leSum(k: int): int {.noinline.} =
  var arr = default(array[50, int])
  for i in 0..49:
    arr[i] = k
  result = 0
  for i in 0..49:                    # `le` test
    result += arr[i]

proc neqSum(k: int): int {.noinline.} =
  var arr = default(array[20, int])
  var i = 0
  while i != 20:
    arr[i] = k
    inc i
  result = 0
  var j = 5
  while j != 20:                     # `neq` test, start at 5
    result += arr[j]
    inc j

proc twoArrays(k: int): int {.noinline.} =
  var a = default(array[30, int])
  var b = default(array[30, int])
  for i in 0..<30:
    a[i] = k + i
  for i in 0..<30:
    b[i] = a[i] * 2                  # both accesses through pointers
  result = 0
  for i in 0..<30:
    result += b[i]

proc usedAfter(k: int): int {.noinline.} =
  var arr = default(array[10, int])
  var i = 0
  while i < 10:
    arr[i] = k
    inc i
  result = i + arr[9]                # the counter's final value is read

proc zeroTrip(k: int): int {.noinline.} =
  var arr = default(array[10, int])
  result = k
  var i = 10
  while i < 10:
    result += arr[i]
    inc i

echo ltSum(3), " ", leSum(2), " ", neqSum(4), " ", twoArrays(1), " ",
  usedAfter(7), " ", zeroTrip(5)

## Procs: parameters, return values, recursion, and default results.
import std/syncio

proc add(a, b: int): int = a + b

proc fac(n: int): int =
  if n <= 1: 1
  else: n * fac(n - 1)

proc fib(n: int): int =
  if n < 2: n
  else: fib(n - 1) + fib(n - 2)

echo add(3, 4)
echo fac(5)               # 120
echo fib(10)              # 55

import std / syncio

proc fib(a: int): int =
  if a <= 2:
    result = 1
  else:
    result = fib(a-1) + fib(a//2)

echo fib(8)

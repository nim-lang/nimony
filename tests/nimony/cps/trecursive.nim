
import std / syncio

proc fib(n: int): int {.passive.} =
  if n <= 1:
    return n
  return fib(n - 1) + fib(n - 2)

proc main() {.passive.} =
  echo fib(10)

main()

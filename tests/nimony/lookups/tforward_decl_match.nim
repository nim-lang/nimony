import std / [assertions, syncio]

# Forward declarations matched with their implementations by tokens:
# generic params, param names and defaults, return type. Pragmas need not
# be repeated.

proc g[T](x: T): T
proc h(x: int; y = 3): int {.inline.}

proc useBoth(): int = g(4) + h(1)

proc g[T](x: T): T = x
proc h(x: int; y = 3): int = x + y

proc outer(): int =
  proc inner(x: int): int
  proc inner(x: int): int = x * 2
  result = inner(5)

proc other(): int =
  # same name and signature as `outer`'s forward decl, different scope
  proc inner(x: int): int = x * 3
  result = inner(5)

assert useBoth() == 8
assert outer() == 10
assert other() == 15
echo "OK"

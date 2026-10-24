import std/syncio
import deps/mstaticterms

# Constant parameters (`x: static[T]`, doc/internals/static-params.md): the
# argument must be a tier-1 constant expression; inside the routine `x` is an
# ordinary value of the plain element type `T`.

proc twice(x: static[int]): int = x * 2

const c = 4
echo twice(3)              # literal
echo twice(c)              # `const` reference
echo twice(c + 1)          # primitive arithmetic over constants
echo twice(-2)
echo twice(2 * c - 1)

# a `staticParam` reference is itself a constant, so it may be forwarded
proc twicePlusOne(y: static[int]): int = twice(y) + 1
echo twicePlusOne(10)

# enum, string and a constant default value
type Color = enum red, green
proc tag(c: static[Color]; s: static[string] = "d"): string = $ord(c) & s
echo tag(green)
echo tag(red, "x")

# general CTFE is never implicit: it is requested with `static(expr)`
proc sq(x: int): int = x * x
echo twice(static(sq(3)))
echo twice(static sq(2))

# generic routines and imported signatures
echo rep(3, 4).len
echo rep("a").len
echo pick(0)
const one = 1
echo pick(one)

proc fwd[T](a: T; k: static[int]): int = rep(a, k).len
echo fwd(7, 3)

# templates take `static` parameters too, including plugin templates: the
# plugin sees the constant argument
template twiceT(x: static[int]): int = x * 2
echo twiceT(c + 1)

template show(x: static[int]) {.plugin: "deps/mstaticarg".}
show(3)

# a closure captures a `static` parameter like any other parameter
proc captured(x: static[int]): int =
  proc next(): int {.closure.} = x + 1
  next()
echo captured(9)

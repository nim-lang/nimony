{.feature: "staticOverloads".}

import std/syncio

# With `staticOverloads` the more-constant candidate wins an otherwise exact
# tie (Nim 2 behavior), as the very last tiebreaker.

proc f(x: static[int]): string = "static"
proc f(x: int): string = "int"

var v = 3
echo f(3)
echo f(v)               # a runtime argument never matches `static[int]`
const k = 5
echo f(k)

proc sq(x: int): int = x * x
echo f(static(sq(3)))

# genuine specificity still decides first: `int8` beats both `int` candidates
# for an `int8` argument, the static preference is never consulted
proc g(x: static[int]): string = "static int"
proc g(x: int8): string = "int8"
let small = 1'i8
echo g(small)
echo g(4)

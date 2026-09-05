import std/assertions

template bump(n: int): int = n + 1
template mix(a, b: int): int = a * 10 + b

type
  Sized[N: static[int]; T] = object
    data: array[bump(N), T]

var s: Sized[5, int]
assert sizeof(s.data) == 48, "bump(5) == 6 -> sizeof(array[6, int]) == 48"

type
  Grid[R, C: static[int]; T] = object
    data: array[bump(R * C), T]

var g: Grid[2, 3, int]
assert sizeof(g.data) == 56, "bump(2 * 3) == 7 -> sizeof(array[7, int]) == 56"

type
  Mixed[R, C: static[int]; T] = object
    data: array[mix(R + 1, C * 2), T]

var m: Mixed[2, 3, int]
assert sizeof(m.data) == 288, "mix(2 + 1, 3 * 2) == mix(3, 6) == 36 -> sizeof(array[36, int]) == 288"

proc takesBump[R, C: static[int]; T](x: array[bump(R * C), T]): int = x.len

var flat: array[7, int]
assert takesBump[2, 3, int](flat) == 7, "takesBump[2, 3, int] should match array[7, int]"

proc takesMix[R, C: static[int]; T](x: array[mix(R + 1, C * 2), T]): int = x.len

var wide: array[36, int]
assert takesMix[2, 3, int](wide) == 36, "takesMix[2, 3, int] should match array[36, int]"

type
  Span[N: static[int]] = range[0 .. bump(N) - 1]

proc lastIndex[N: static[int]](): int = high(Span[N])

assert lastIndex[5]() == 5, "lastIndex[5]() == bump(5) - 1 == 5"

type
  Composite[A, B: static[int]] = object
    s: Span[bump(A)]

func foo[A, B: static[int]](): Span[bump(A)] = B
assert foo[2, 3]() == 3

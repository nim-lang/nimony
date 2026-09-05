import std/syncio

func bump(n: int): int = n + 1
func mix(a, b: int): int = a * 10 + b

type
  Sized[N: static[int]; T] = object
    data: array[bump(N), T]

var s: Sized[5, int]
echo sizeof(s.data)                 # bump(5) == 6  -> 48

type
  Grid[R, C: static[int]; T] = object
    data: array[bump(R * C), T]

var g: Grid[2, 3, int]
echo sizeof(g.data)                 # bump(2 * 3) == 7 -> 56

type
  Mixed[R, C: static[int]; T] = object
    data: array[mix(R + 1, C * 2), T]

var m: Mixed[2, 3, int]
echo sizeof(m.data)                 # mix(2 + 1, 3 * 2) == mix(3, 6) == 36 -> 288

# overload resolution folds already-bound `static[int]` params when matching an
# array length built from a user-defined compile-time function.
proc takesBump[R, C: static[int]; T](x: array[bump(R * C), T]): int = x.len

var flat: array[7, int]
echo takesBump[2, 3, int](flat)     # bump(2 * 3) == 7

proc takesMix[R, C: static[int]; T](x: array[mix(R + 1, C * 2), T]): int = x.len

var wide: array[36, int]
echo takesMix[2, 3, int](wide)      # mix(3, 6) == 36

# and the same in a range bound:
type
  Span[N: static[int]] = range[0 .. bump(N) - 1]

proc lastIndex[N: static[int]](): int = high(Span[N])

echo lastIndex[5]()                 # bump(5) - 1 == 5

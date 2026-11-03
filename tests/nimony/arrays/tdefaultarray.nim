## `default(array[N, T])` is one `(arepconstr T X)` in the IR, not N
## elements (and not a loop when `default(T)` is a constant). Each context a
## back end must lower it in: a local, a global, a field of a heap object, a
## closure environment, an argument, a result, an element of a bigger array,
## and a non-zero `X` (holey enum, field default).

import std/syncio

type
  Holey = enum
    hA = 3, hB = 7
  WithDefault = object
    x: int = 7
    c: char
  Inner = object
    a: int32
    b: bool
  Node = ref object
    data: array[2_000_000, int]
    tail: int

const Big = 300_000

var g = default(array[Big, int])
var gh = default(array[5, Holey])

proc sum[N: static int](a: array[N, int]): int =
  result = 0
  for x in a: result += x

proc mk(): array[1000, WithDefault] =
  result = default(array[1000, WithDefault])

proc main =
  var a = default(array[Big, int])
  a[7] = 1
  echo sum(a)
  echo sum(default(array[Big, int]))

  var h = default(array[4, Holey])
  echo h[0], " ", h[3]

  let w = mk()
  echo w[0].x, " ", w[999].x, " ", ord(w[500].c)

  var nested = default(array[3, array[100_000, Inner]])
  echo nested[2][99_999].a, " ", nested[1][5].b

  let n = Node()
  echo n.data[1_999_999], " ", n.tail

  var cap = default(array[Big, int])
  cap[Big - 1] = 5
  let p = proc (): int {.closure.} = cap[Big - 1] + cap[0]
  echo p()

main()

echo g[Big - 1], " ", gh[4]
g[3] = 9
g = default(array[Big, int])
echo g[3]

# nimony#2846: a `ref object` with a huge array field is constructed in its
# heap allocation; the zero `default(array)` no longer passes through a stack
# temp of the full size (16 MB here, past the default 8 MB stack).
import std/syncio

type
  Color = enum red, green
  Inner = object
    a: array[1_000_000, int]
    b: int
  Node = ref object
    data: array[2_000_000, int]
    tag: int
  Grid = ref object
    head: string
    cells: array[1000, array[1000, float]]
    colors: array[3_000_000, Color]
    inner: Inner
  Small = ref object
    x: array[4, int]
    y: int = 7
  Empty = ref object
    none: array[0, int]
    z: int

proc main =
  let n = Node(tag: 3)
  echo n.data[0], " ", n.data[1_999_999], " ", n.tag
  let g = Grid(head: "h")
  echo g.head, " ", g.cells[999][999], " ", ord(g.colors[5]), " ", g.inner.a[7], " ", g.inner.b
  let s = Small()
  echo s.x[3], " ", s.y
  var loc = default(array[8, int])
  loc[2] = 4
  echo loc[2]
  # a length-0 array: the empty `(aconstr T)` must not get a `0` element
  let e = Empty(z: 5)
  echo e.none.len, " ", e.z
  echo default(array[0, int]).len

main()

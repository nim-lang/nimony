# Issue #2846: constructing a `ref object` with a big array field must build
# it in the heap allocation, not in a stack temp (16 MB here, past the default
# 8 MB stack).

import std/syncio

type
  Node = ref object
    data: array[2_000_000, int]

  Mixed = ref object
    a: int
    s: string
    grid: array[1000, array[1000, float]]
    b: int = 7

  Base = ref object of RootObj
    id: int
    bits: array[2_000_000, uint8]
  Derived = ref object of Base
    more: array[2_000_000, int]
    tag: char

  Kind = enum kA, kB
  Variant = ref object
    case kind: Kind
    of kA: big: array[2_000_000, int]
    of kB: x: int

  Box[T] = ref object
    payload: array[2_000_000, T]
    n: int

method describe(b: Base): string {.base.} = "base"
method describe(d: Derived): string = "derived"

let n = Node()
echo n.data[0], " ", n.data[1_999_999]

let m = Mixed(a: 3, s: "hi")
echo m.a, " ", m.s, " ", m.grid[999][999], " ", m.b

let d = Derived(id: 5, tag: 'z')
echo d.id, " ", d.bits[0], " ", d.more[1_999_999], " ", d.tag, " ", describe(Base(d))

let v = Variant(kind: kA)
echo v.big[42]

let bx = Box[int](n: 9)
echo bx.payload[123], " ", bx.n

# A type may reach itself through an indirection: none of these is a
# recursion by value.
import std/syncio

type
  R = object
    next: nil ref R
    p: nil ptr R
    s: seq[R]
    str: string
    f: proc (x: R): R {.nimcall.}
    u: nil ptr UncheckedArray[R]
  N = ref object
    kids: seq[N]
    parent: nil N
  G[T] = object
    x: T
    next: nil ref G[T]
  H = object
    g: seq[G[H]]
  FNObj[T] = object
    x: T
    fn: nil FN[T]
  FN[T] = ref FNObj[T]
  M = object
    a: array[2, nil ref M]
    t: tuple[a: int, b: nil ptr M]
  K = object
    case b: bool
    of true: x: seq[K]
    else: discard
  DD = distinct (nil ref DD2)
  DD2 = object
    d: DD
  Base = object of RootObj
    kids: seq[Base]
  Derived = object of Base
    parent: nil ref Derived
  Inst = object
    a: G[int]
    b: G[G[int]]

proc idR(x: R): R = x

var h = H(g: @[G[H](next: nil)])
var r = R(next: nil, f: idR)
var k = K(b: true, x: @[])
var fnobj = FNObj[int](x: 3, fn: nil)
var d = Derived(parent: nil)
var i = Inst()
var n = N(parent: nil)
echo h.g.len, " ", fnobj.x, " ", k.x.len, " ", i.b.x.x, " ", n.kids.len

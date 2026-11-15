# Object field defaults that are expressions other than literals: calls,
# operators, conversions, macros. A default is checked once, in the body
# phase, after every type and layout of the section is known, and is
# evaluated at each construction.
import std/[syncio, assertions, options, macros]
import deps/mfielddefaultcalls

proc answer(): int = 41 + 1

proc early(): int = Late().k # constructs a type declared further down

macro mkInner(): untyped = result = newCall(ident"Inner")

type
  O = object
    s: seq[int] = @[1, 2]
    e: seq[int] = @[]
    n: seq[int] = newSeq[int](2)
    o: Option[int] = some(1)
    c: int = answer()
    t: string = "a" & "b"
    a: int = 2 + 3
    i8: int8 = int8(3)
    l: int = len("abc")
    lp: int = later() # declared further down
    lk: int = laterK + 1
  R = ref object
    s: seq[int] = @[7]
  G[T] = object
    size: int = sizeof(T)
    k: seq[int] = @[9]
    s: seq[T] = @[T(1), T(2)]
    t: (T, int) = (T(3), 4)
  GH[T: HasDefault] = object # checked like a generic routine body
    v: T = default(T)
    s: seq[T] = newSeq[T](2)

  # IBox[int] is first needed while IG[int]'s defaults are typed, for an
  # instance used only in a signature
  IBox[T] = object
    v: int = 1 + 1
  IG[T] = object
    n: int = IBox[T]().v + 1

  # names types declared later in the section
  Outer = object
    i: Inner = Inner()
    n: int = 1
    r: RInner = RInner()
    d: Inner = default(Inner)
    m: Mid = Mid(j: Inner(k: 5))
    x: Inner = mkInner()
    dd: D = D(3)
    e: E = high(E)
    f: E = e1
  Mid = object
    j: Inner = Inner()
  Inner = object
    s: seq[int] = @[1, 2, 3]
    k: int = 2 + 2
  RInner = ref object
    s: seq[string] = @["r"]
  D = distinct int
  E = enum e1, e2

  # a default's `sizeof` of a type whose layout names this one
  SA = object
    n: int = sizeof(SB)
  SB = object
    a: array[sizeof(SA), byte]

  Late = object
    k: int = 6 * 7

proc later(): int = 5
const laterK = 10

const fromConst = Inner(k: 8) # a `const` constructs it in the signature phase

proc withParam(x = Inner()): int = x.k

proc onlyInSignature(x: IG[int]) = discard

block: # object construction and `default` use the defaults
  let x = O()
  assert x.s == @[1, 2]
  assert x.e.len == 0
  assert x.n == @[0, 0]
  assert x.o.isSome and x.o.get == 1
  assert x.c == 42
  assert x.t == "ab"
  assert x.a == 5
  assert x.i8 == 3'i8
  assert x.l == 3
  assert x.lp == 5 and x.lk == 11
  let y = default(O)
  assert y.s == @[1, 2] and y.c == 42
  let z = O(s: @[3])
  assert z.s == @[3] and z.a == 5

block: # ref object
  assert R().s == @[7]

block: # generic object: defaults depending on the generic parameter
  assert G[int64]().size == 8
  assert G[int8]().size == 1
  assert G[int8]().k == @[9]
  let g = G[int8]()
  assert g.s == @[1'i8, 2'i8]
  assert g.t[0] == 3'i8 and g.t[1] == 4
  let f = G[float]()
  assert f.s == @[1.0, 2.0] and f.t[0] == 3.0
  assert GH[int]().v == 0 and GH[string]().s == @["", ""]

block: # defaults naming types declared later in the section
  let o = Outer()
  assert o.i.s == @[1, 2, 3] and o.i.k == 4 and o.n == 1
  assert o.r.s == @["r"]
  assert o.d.s == @[1, 2, 3] and o.d.k == 4
  assert o.m.j.k == 5 and o.m.j.s == @[1, 2, 3]
  assert o.x.k == 4
  assert int(o.dd) == 3 and o.e == e2 and o.f == e1
  assert Mid().j.k == 4
  assert SA().n == 8

block: # earlier constructions: a proc body, a `const`, a parameter default
  assert early() == 42
  assert fromConst.k == 8 and fromConst.s == @[1, 2, 3]
  assert withParam() == 4

block: # types declared in another module
  let p = P()
  assert p.s == @[4, 5] and p.w == 42
  assert p.q.s == @[6] and p.q.k == 7
  let gd = GD[int8]()
  assert gd.s == @[1'i8, 1'i8] and gd.n == 2

proc local() =
  type L = object
    s: seq[string] = @["x"]
    v: (int, char) = (3, 'c')
  assert L().s == @["x"] and L().v == (3, 'c')
local()

echo "ok"

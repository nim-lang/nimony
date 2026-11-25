# A type section's definitions may refer to later ones, a parent type included.
import std/syncio

type
  # object parent declared later; `x` shadows the inherited `x`
  D = object of Base
    x: string
  Base = object of RootObj
    x: int

  # `ref object` parent declared later, with a method override
  RD = ref object of RBase
    x: string
  RBase = ref object of RootObj
    x: int

  # parent reached through `ref`, its object declared later
  PD = ref object of PBase
    z: int
  PBase = ref PBaseObj
  PBaseObj = object of RootObj
    b: int

  # a chain declared child first
  C3 = object of C2
    c: int
  C2 = object of C1
    b: int
  C1 = object of RootObj
    a: int

var d = D(x: "a")
Base(d).x = 3
echo d.x, " ", Base(d).x

method show(a: RBase): string {.base.} = "base " & $a.x
method show(a: RD): string = "rd " & a.x & " " & $RBase(a).x
let rd = RD(x: "s")
RBase(rd).x = 7
let r: RBase = rd
echo r.show, " / ", RBase(x: 3).show

echo PD(z: 1, b: 2).b
let c = C3(a: 1, b: 2, c: 3)
echo c.a, c.b, c.c

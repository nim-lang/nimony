# issue #2724
import std/syncio
type
  Parent[T] = ref object of RootObj
    v: T
  Mid = ref object of Parent[int]
  Child {.final.} = ref object of Mid
  GChild[T] = ref object of Parent[T]

proc onlyInt(x: Parent[int]): int = x.v
proc anyP[T](x: Parent[T]): T = x.v
echo onlyInt(Child(v: 3)), " ", anyP(Child(v: 4)), " ", onlyInt(GChild[int](v: 5)), " ", anyP(GChild[string](v: "s"))

# a subtype satisfies an object constraint and `T` binds to it (nimony-bugs-llms#330)
type
  Base = ref object of RootObj
  Sub = ref object of Base
  SubSub = ref object of Sub
  OBase = object of RootObj
  OSub = object of OBase
  GBox[T] = object of RootObj
    x: T
  IntBox = object of GBox[int]

proc same[T: Base](v: T): T = v
proc kind[T: Base](v: T): string = (when T is Sub: "Sub" else: "Base")
proc p[T: Base](v: T): string = "T: Base"
proc p[T: Sub](v: T): string = "T: Sub"
proc q(v: Base): string = "concrete"
proc q[T: Sub](v: T): string = "generic"
proc obj[T: OBase](v: var T): int = 1
proc pobj[T: ptr OBase](v: T): int = 2
proc orc[T: Base or int](v: T): int = 3
proc inst[T: GBox[int]](v: T): int = 4
proc td[T: Base](t: typedesc[T]): int = 5
var o = OSub()
let s: Sub = same(Sub())
echo kind(Base()), " ", kind(SubSub()), " ", p(Base()), " ", p(SubSub()), " ", q(Base()), " ", q(Sub())
echo obj(o), " ", pobj(addr o), " ", orc(Sub()), " ", inst(IntBox()), " ", td(Sub)

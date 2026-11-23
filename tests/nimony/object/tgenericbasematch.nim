# A subtype satisfies an object constraint `[T: Base]`, as in Nim 2, and `T`
# binds to the argument's own type (planetis-m/nimony-bugs-llms#330).

import std/syncio

type
  Base = ref object of RootObj
  Sub = ref object of Base
  SubSub = ref object of Sub
  OBase = object of RootObj
    x: int
  OSub = object of OBase
  GBox[U] = object of RootObj
    val: U
  GSub = object of GBox[int]
  Other = object of RootObj

proc same[T: Base](v: T): T = v
proc osame[T: OBase](v: T): T = v

# (a) a generic match ranks above a subtype match
proc pa(v: Base): string = "Base"
proc pa[T: Base](v: T): string = "T: Base"

# (b) the constrained generic is more specific than the plain one
proc pb[T](v: T): string = "T"
proc pb[T: Base](v: T): string = "T: Base"

# (c) `not Base` rejects a subtype of `Base`
proc pc[T: not Base](v: T): string = "T: not Base"
proc pc(v: Base): string = "Base"

# a constraint naming another typevar binds it from the inherited base
proc gsame[U; T: GBox[U]](v: T): T = v
proc gelem[U; T: GBox[U]](v: T): seq[U] = @[]
proc psame[U; T: ptr GBox[U]](v: T): T = v
proc pelem[U; T: ptr GBox[U]](v: T): seq[U] = @[]
proc ptrsame[T: ptr OBase](v: T): T = v

# an unrelated object still does not match
proc po[T: OBase](v: T): string = "T: OBase"
proc po[T: Other](v: T): string = "T: Other"

let s: Sub = same(Sub())
let ss: SubSub = same(SubSub())
let b: Base = same(Base())
echo s != nil, " ", ss != nil, " ", b != nil
let os: OSub = osame(OSub(x: 3))
echo os.x
echo pa(Sub()), " | ", pb(Sub()), " | ", pc(Sub()), " | ", pc(3)
var g = GSub(val: 7)
let g2: GSub = gsame(g)
let e: seq[int] = gelem(g)
let pg: ptr GSub = psame(addr g)
let pe: seq[int] = pelem(addr g)
echo g2.val, " ", e.len, " ", pg.val, " ", pe.len
var o = OSub(x: 5)
let po2: ptr OSub = ptrsame(addr o)
echo po2.x
echo po(o), " | ", po(Other())

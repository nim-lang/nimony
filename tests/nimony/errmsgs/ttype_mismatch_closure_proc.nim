proc dontTakeClosure(p: proc ()) = discard

proc test =
  proc closureProc() {.closure.} = discard
  dontTakeClosure(closureProc)

test()

proc takesOpenArray(a: openArray[int]; b: int) = discard
takesOpenArray([1, 2], 3, 4)

# a nimcall routine converts to a closure only as a whole argument, not as a
# field of a tuple argument:
type Setup = proc (x: int): int {.closure.}
proc dbl(x: int): int = x * 2
proc put[T](s: var seq[T]; x: sink T) = discard
var cases: seq[(string, Setup)] = @[]
put(cases, ("b", dbl))

# nor as a field of a tuple constructor or variable, or through a pointer,
# whether the nimcall side is a routine or a proc value:
type Plain = proc (x: int): int
proc takeTup(t: (Setup, int)): int = t[0](t[1])
proc takePtr(p: ptr Setup): int = p[](3)
proc nested =
  var pv: Plain = dbl
  let t: (Plain, int) = (pv, 3)
  discard takeTup((dbl, 3))
  discard takeTup((pv, 3))
  discard takeTup(t)
  discard takePtr(addr pv)
nested()

# nor as an array element, or as a proc type in a parameter's or the result's
# type of a routine passed for a `.closure` proc type:
proc takeArr(a: array[1, Setup]): int = a[0](3)
proc ho(cb: Plain): int = cb(3)
proc takeHO(f: proc (cb: Setup): int {.closure.}): int = 0
proc mkPlain(): Plain = dbl
proc takeRet(f: proc (): Setup {.closure.}): int = f()(3)
let plainArr: array[1, Plain] = [dbl]
discard takeArr(plainArr)
discard takeHO(ho)
discard takeRet(mkPlain)

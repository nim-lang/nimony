# The prover keeps a fact about a variable only while nothing can write it
# without naming it (doc/language.md, "Aliasing"). Each routine below loses
# the fact `x < 10` its guard established, so in a `staticContracts` module
# the index is an error.

{.feature: "staticContracts".}
{.feature: "assumeSync".}

type
  Obj = object
    n: int
  Holder = object
    r: ref Obj
  D = object
    r: ref Obj
  Big = object
    n: int
    pad: array[16, int]

proc `=wasMoved`(d: var D) = d.r = nil
proc `=destroy`(d: D) =
  if d.r != nil: d.r.n = 1000000000

var g = 3
var arr: array[10, int]
proc clobber() = g = 1000000000
func touch(h: var Holder) = h.r.n = 1000000000

proc r3addr(a: int): int =          # R3: an address taken anywhere in the routine
  var x = a
  let p = addr x
  result = 0
  if x >= 0 and x < 10:
    p[] = 1000000000
    result = arr[x]

proc r4capture(a: int): int =       # R4: a closure names the local
  var x = a
  proc bump() {.closure.} = x = 1000000000
  result = 0
  if x >= 0 and x < 10:
    bump()
    result = arr[x]

proc r7proc(v: var Obj): int =      # R7: a `proc` call
  result = 0
  if v.n >= 0 and v.n < 10:
    clobber()
    result = arr[v.n]

proc r7ref(v: var Obj; r: ref Obj): int =   # R7: a write through a `ref`
  result = 0
  if v.n >= 0 and v.n < 10:
    r.n = 1000000000
    result = arr[v.n]

proc r7global(v: var int): int =    # R7: a write to a global
  result = 0
  if v >= 0 and v < 10:
    g = 1000000000
    result = arr[v]

proc r7byref(a: Big): int =         # a value parameter is read-only: no error
  result = 0
  if a.n >= 0 and a.n < 10:
    clobber()
    result = arr[a.n]

proc g1func(v: var Obj; h: var Holder): int =   # a `func` with a `var` argument
  result = 0
  if v.n >= 0 and v.n < 10:
    touch(h)
    result = arr[v.n]

proc r8call(): int =                # R8: any call
  result = 0
  if g >= 0 and g < 10:
    clobber()
    result = arr[g]

proc r8ptr(p: ptr int): int =       # R8: a write through a pointer
  result = 0
  if g >= 0 and g < 10:
    p[] = 1000000000
    result = arr[g]

proc r8borrowed(v: var int): int =  # R8: a write to a borrowed parameter
  result = 0
  if g >= 0 and g < 10:
    v = 1000000000
    result = arr[g]

proc g2dtor(v: var Obj; r: ref Obj): int =   # a destructor at scope exit
  result = 0
  if v.n >= 0 and v.n < 10:
    block:
      var d = D(r: r)
      discard d
    result = arr[v.n]

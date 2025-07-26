import std/[assertions]

{.push header: "pushpop.h".}
type
  TestStruct1 {.importc: "TestStruct1".} = object
    x: cint
{.pop.}

var tests1: TestStruct1

{.push header: "<math.h>".}
proc cos(x: cdouble): cdouble {.importc: "cos", cdecl.}
{.push cdecl.}
proc sin(x: cdouble): cdouble {.importc: "sin".}
{.pop.}
proc c_fpclassify[T: SomeFloat](x: T): int {.importc: "fpclassify".}
let c_fpNormal    {.importc: "FP_NORMAL".}: int
{.pop.}

assert c_fpclassify(1.0) == c_fpNormal

{.push discardable.}
proc foo(x: int): int = x
{.pop.}

foo(1)

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
{.pop.}

{.push discardable.}
proc foo(x: int): int = x
{.pop.}

foo(1)

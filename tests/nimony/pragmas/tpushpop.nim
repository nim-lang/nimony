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

{.push header: "<headerfile_doesnt_exists.h>", thiscall.}
# header pragma and thiscall calling convention in push pragma is ignored if declarations have both of them.
# This test uses C function without parameters as current implementation applies pushed header pragma to parameters.
proc getchar(): cint {.importc: "putchar", header: "<stdio.h>", noconv.}
{.pop.}

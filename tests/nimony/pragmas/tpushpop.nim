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

{.push callConv: cdecl.}
proc testCallConv(x: cint): cint {.importc: "testCallConv", header: "<stdio.h>".}
{.pop.}

# A proc type declares no symbol to import: the pushed `importc` skips the
# parameter's proc type
{.push importc, cdecl.}
proc qsort(base: pointer; n, size: csize_t; cmp: proc (a, b: pointer): cint {.cdecl.})
{.pop.}

proc cmpInt(a, b: pointer): cint {.cdecl.} =
  let x = cast[ptr cint](a)[]
  let y = cast[ptr cint](b)[]
  result = (if x < y: -1 elif x > y: 1 else: 0)

var arr = [cint 3, 1, 2]
qsort(addr arr[0], 3, csize_t sizeof(cint), cmpInt)
assert arr == [cint 1, 2, 3]

import std/assertions
import deps/mimportcvar

{.emit: "int cvar = 11;"}
{.emit: "int cvarInMainModule = 33;".}

var cvarInMainModule {.importc: "cvarInMainModule".}: int32

assert cvar == 11
assert cvarInModule == 22
assert cvarInMainModule == 33

{.emit: "int cvarNodecl = 44;".}
var cvarNodecl {.importc: "cvarNodecl", nodecl.}: int32
assert cvarNodecl == 44

{.build("C", "ctestvars.c").}
var cvarInDotC {.importc: "cvarInDotC", header: "ctestvars.h".}: int32
assert cvarInDotC == 55

assert cvarInDotCInModule == 66

{.emit:"""
const int TEST1 = 123;
#define TEST2 321
""".}

let
  TEST0 = 1
  TEST1 {.importc, nodecl.}: cint
  TEST2 {.importc: "TEST2", nodecl.}: cint

assert TEST0 == 1
assert TEST1 == 123
assert TEST2 == 321

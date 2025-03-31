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

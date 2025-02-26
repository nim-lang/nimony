import std/syncio
import deps/mimportcvar

{.emit: "int cvar = 11;"}
{.emit: "int cvarInMainModule = 33;".}

var cvarInMainModule {.importc: "cvarInMainModule".}: int32

assert cvar == 11
assert cvarInModule == 22
assert cvarInMainModule == 33

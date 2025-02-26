var cvar* {.importc: "cvar".}: int32

{.emit: "int cvarInModule = 22;".}
var cvarInModule* {.importc: "cvarInModule".}: int32

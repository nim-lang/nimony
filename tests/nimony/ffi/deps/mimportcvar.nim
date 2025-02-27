var cvar* {.importc: "cvar".}: int32

{.emit: "int cvarInModule = 22;".}
var cvarInModule* {.importc: "cvarInModule".}: int32

var cvarInDotCInModule* {.importc:"cvarInDotCInModule", header: "ctestvars.h".}: int32

# Private implementation included by std/posix/posix; not a standalone module.

proc errnoLocation(): ptr cint {.importc: "__errno_location", sideEffect.}

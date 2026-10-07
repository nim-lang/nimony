# Private implementation included by std/posix/posix; not a standalone module.

when defined(android):
  proc errnoLocation(): ptr cint {.importc: "__errno", sideEffect.}
else:
  proc errnoLocation(): ptr cint {.importc: "__errno_location", sideEffect.}

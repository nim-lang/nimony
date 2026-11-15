# errno is thread-local; the libc accessor has three leading underscores.
proc errnoLocation(): ptr cint {.importc: "___errno", sideEffect.}

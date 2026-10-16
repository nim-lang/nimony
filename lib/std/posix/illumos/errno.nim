# errno is thread-local; the libc accessor has THREE leading underscores.
proc errnoLocation(): ptr cint {.importc: "___errno", sideEffect.}

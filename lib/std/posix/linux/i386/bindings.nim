# Private implementation included by std/posix/posix; not a standalone module.

# Large-file ABI: Off is 64-bit on i386 as on every other target.

proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.importc: "open64", sideEffect.}
proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate64".}
proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat64", sideEffect.}
proc lstat*(a1: cstring, a2: var Stat): cint {.importc: "lstat64", sideEffect.}
proc stat*(a1: cstring, a2: var Stat): cint {.importc: "stat64".}

proc mmap*(a1: nil pointer, a2: csize_t, a3, a4, a5: cint, a6: Off): pointer {.
  importc: "mmap64".}

proc fallocateImpl(fd: cint; mode: cint; offset, len: Off): cint {.
  importc: "fallocate64", sideEffect.}

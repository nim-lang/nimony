# Private implementation included by std/posix/posix; not a standalone module.

when defined(i386):
  include "i386/bindings"
elif linuxA64Raw:
  include "arm64/raw"
else:
  proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.importc: "open", sideEffect.}
  proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate".}
  proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", sideEffect.}
  proc lstat*(a1: cstring, a2: var Stat): cint {.importc: "lstat", sideEffect.}
  proc stat*(a1: cstring, a2: var Stat): cint {.importc: "stat".}

  proc mmap*(a1: nil pointer, a2: csize_t, a3, a4, a5: cint, a6: Off): pointer {.
    importc: "mmap".}

when not linuxA64Raw:
  proc readlink*(a1, a2: cstring, a3: int): int {.importc: "readlink".}
  proc symlink*(a1, a2: cstring): cint {.importc: "symlink".}

  proc chmod*(path: cstring, mode: Mode): cint {.importc: "chmod", sideEffect.}
  proc mkdir*(path: cstring, mode: Mode): cint {.importc: "mkdir", sideEffect.}
  proc rmdir*(path: cstring): cint {.importc: "rmdir", sideEffect.}
  proc unlink*(path: cstring): cint {.importc: "unlink", sideEffect.}

  proc pipe*(a: ptr cint): cint {.importc: "pipe", sideEffect.}
  proc dup2*(oldfd, newfd: cint): cint {.importc: "dup2", sideEffect.}
  proc fork*(): Pid {.importc: "fork", sideEffect.}

# Implemented over the `fallocate` symbol, which is a glibc/musl export
# AND a raw syscall name arkham can lower — one binding serves every
# configuration (glibc's own posix_fallocate is just this plus a
# gap-plugging fallback for filesystems without fallocate support).
when not defined(i386):
  proc fallocateImpl(fd: cint; mode: cint; offset, len: Off): cint {.
    importc: "fallocate", sideEffect.}
proc posix_fallocate*(a1: cint, a2, a3: Off): cint =
  let r = pcall(fallocateImpl(a1, 0, a2, a3))
  if r < 0: cint(-r) else: cint(0)

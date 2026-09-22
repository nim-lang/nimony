# Private implementation included by std/posix/posix; not a standalone module.

proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.importc: "open", sideEffect.}
proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate".}
proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", sideEffect.}
proc lstat*(a1: cstring, a2: var Stat): cint {.importc: "lstat", sideEffect.}
proc stat*(a1: cstring, a2: var Stat): cint {.importc: "stat".}

proc mmap*(a1: nil pointer, a2: csize_t, a3, a4, a5: cint, a6: Off): pointer {.
  importc: "mmap".}

proc readlink*(a1, a2: cstring, a3: int): int {.importc: "readlink".}
proc symlink*(a1, a2: cstring): cint {.importc: "symlink".}

proc chmod*(path: cstring, mode: Mode): cint {.importc: "chmod", sideEffect.}
proc mkdir*(path: cstring, mode: Mode): cint {.importc: "mkdir", sideEffect.}
proc rmdir*(path: cstring): cint {.importc: "rmdir", sideEffect.}
proc unlink*(path: cstring): cint {.importc: "unlink", sideEffect.}

proc pipe*(a: ptr cint): cint {.importc: "pipe", sideEffect.}
proc dup2*(oldfd, newfd: cint): cint {.importc: "dup2", sideEffect.}
proc fork*(): Pid {.importc: "fork", sideEffect.}

# 2001 POSIX evidently does not concern Apple: no posix_fallocate on
# macOS. Keep the traditional fcntl(F_PREALLOCATE) emulation, with the
# fstore_t layout and F_* values transcribed from the xnu <fcntl.h>.
type FStore {.pure.} = object ## fstore_t (32 bytes)
  fst_flags: uint32     ## IN: flags word
  fst_posmode: cint     ## IN: indicates offset field
  fst_offset: Off       ## IN: start of the region
  fst_length: Off       ## IN: size of the region
  fst_bytesalloc: Off   ## OUT: number of bytes allocated
const
  F_PREALLOCATE = cint(42)
  F_PEOFPOSMODE = cint(3)
  F_ALLOCATEALL = uint32(4)
proc posix_fallocate*(a1: cint, a2, a3: Off): cint =
  var fst = FStore(fst_flags: F_ALLOCATEALL, fst_posmode: F_PEOFPOSMODE,
                   fst_offset: a2, fst_length: a3)
  # Must also call ftruncate to match what POSIX does. Unlike posix_fallocate,
  # this can shrink files.  Could guard w/getFileSize, but caller likely knows
  # present size & has no good reason to call this unless it is growing.
  if fcntl(a1, F_PREALLOCATE, fst.addr) != cint(-1): ftruncate(a1, a2 + a3)
  else: cint(-1)

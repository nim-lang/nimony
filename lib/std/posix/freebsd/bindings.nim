# Private implementation included by std/posix/posix; not a standalone module.

when freebsdRaw:
  const
    AT_FDCWD* = cint(-100)
    AT_SYMLINK_NOFOLLOW* = cint(0x200)

  # See `freebsdRaw`: `stat`/`lstat` are `fstatat` relative to the cwd, as in
  # FreeBSD's own libc.
  proc fstatat(dirfd: cint; path: cstring; buf: var Stat; flags: cint): cint {.
    importc: "fstatat", sideEffect.}
  proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.importc: "open", sideEffect.}
  proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate".}
  proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", sideEffect.}
  proc lstat*(a1: cstring, a2: var Stat): cint {.inline, sideEffect.} =
    fstatat(AT_FDCWD, a1, a2, AT_SYMLINK_NOFOLLOW)
  proc stat*(a1: cstring, a2: var Stat): cint {.inline, sideEffect.} =
    fstatat(AT_FDCWD, a1, a2, cint(0))

  # The `__getcwd` trap answers 0 on success rather than the buffer.
  proc sysGetcwd(buf: cstring; size: int): cint {.importc: "__getcwd", sideEffect.}
  proc getcwd*(a1: cstring, a2: int): nil cstring {.sideEffect.} =
    let r = pcall(sysGetcwd(a1, a2))
    if r < 0:
      setErrno cint(-r)
      result = nil
    else:
      result = a1

else:
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

when freebsdRaw:
  # No `pipe` trap since FreeBSD 11 (libc's `pipe` is `pipe2(fds, 0)`).
  proc pipe2(a: ptr cint; flags: cint): cint {.importc: "pipe2", sideEffect.}
  proc pipe*(a: ptr cint): cint {.inline, sideEffect.} = pipe2(a, cint(0))
  proc dup2*(oldfd, newfd: cint): cint {.importc: "dup2", sideEffect.}
  proc fork*(): Pid {.importc: "fork", sideEffect.}
else:
  proc pipe*(a: ptr cint): cint {.importc: "pipe", sideEffect.}
  proc dup2*(oldfd, newfd: cint): cint {.importc: "dup2", sideEffect.}
  proc fork*(): Pid {.importc: "fork", sideEffect.}

# POSIX returns the error number directly, rather than setting errno.
proc posix_fallocate*(a1: cint, a2, a3: Off): cint {.
  importc: "posix_fallocate", sideEffect.}

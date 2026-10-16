# Private implementation included by std/posix/posix; not a standalone module.

const
  AT_FDCWD* = cint(-100)          ## Resolve a relative path against the cwd.
  AT_SYMLINK_NOFOLLOW* = cint(0x100)
  AT_REMOVEDIR* = cint(0x200)     ## Make unlinkat behave like rmdir.

# See `linuxA64Raw`: there is no `open`/`stat`/`lstat` syscall on AArch64, so
# each is expressed through the `*at` twin the kernel does provide. The
# argument shuffle is exactly what glibc's own wrappers do.
proc openat(dirfd: cint; path: cstring; flags: cint; mode: Mode): cint {.
  importc: "openat", sideEffect.}
proc newfstatat(dirfd: cint; path: cstring; buf: var Stat; flags: cint): cint {.
  importc: "newfstatat", sideEffect.}
proc open*(a1: cstring; a2: cint; mode: Mode = 0): cint {.inline, sideEffect.} =
  openat(AT_FDCWD, a1, a2, mode)
proc ftruncate*(a1: cint, a2: Off): cint {.importc: "ftruncate".}
proc fstat*(a1: cint, a2: var Stat): cint {.importc: "fstat", sideEffect.}
proc lstat*(a1: cstring, a2: var Stat): cint {.inline, sideEffect.} =
  newfstatat(AT_FDCWD, a1, a2, AT_SYMLINK_NOFOLLOW)
proc stat*(a1: cstring, a2: var Stat): cint {.inline, sideEffect.} =
  newfstatat(AT_FDCWD, a1, a2, cint(0))

proc mmap*(a1: nil pointer, a2: csize_t, a3, a4, a5: cint, a6: Off): pointer {.
  importc: "mmap".}

proc readlinkat(dirfd: cint; path, buf: cstring; size: int): int {.
  importc: "readlinkat".}
proc symlinkat(target: cstring; dirfd: cint; linkpath: cstring): cint {.
  importc: "symlinkat".}
proc readlink*(a1, a2: cstring, a3: int): int {.inline.} =
  readlinkat(AT_FDCWD, a1, a2, a3)
proc symlink*(a1, a2: cstring): cint {.inline.} =
  symlinkat(a1, AT_FDCWD, a2)

proc mkdirat(dirfd: cint; path: cstring; mode: Mode): cint {.
  importc: "mkdirat", sideEffect.}
proc unlinkat(dirfd: cint; path: cstring; flags: cint): cint {.
  importc: "unlinkat", sideEffect.}
proc mkdir*(path: cstring, mode: Mode): cint {.inline, sideEffect.} =
  mkdirat(AT_FDCWD, path, mode)
proc rmdir*(path: cstring): cint {.inline, sideEffect.} =
  unlinkat(AT_FDCWD, path, AT_REMOVEDIR)
proc unlink*(path: cstring): cint {.inline, sideEffect.} =
  unlinkat(AT_FDCWD, path, cint(0))
proc fchmodat(dirfd: cint; path: cstring; mode: Mode; flags: cint): cint {.
  importc: "fchmodat", sideEffect.}
proc chmod*(path: cstring, mode: Mode): cint {.inline, sideEffect.} =
  fchmodat(AT_FDCWD, path, mode, cint(0))

proc pipe2(a: ptr cint; flags: cint): cint {.importc: "pipe2", sideEffect.}
proc dup3(oldfd, newfd, flags: cint): cint {.importc: "dup3", sideEffect.}
# `fork()` is `clone(SIGCHLD, 0, 0, 0, 0)` — what glibc's own fork reduces to
# once its pthread bookkeeping is stripped. The child inherits everything and
# signals SIGCHLD to the parent on exit, which is what `wait4` waits for.
proc clone(flags: culong; stack, parentTid, tls, childTid: nil pointer): Pid {.
  importc: "clone", sideEffect.}
proc pipe*(a: ptr cint): cint {.inline, sideEffect.} = pipe2(a, cint(0))
proc dup2*(oldfd, newfd: cint): cint {.inline, sideEffect.} =
  # `dup3` rejects oldfd == newfd (EINVAL) where `dup2` returns it unchanged.
  if oldfd == newfd: oldfd else: dup3(oldfd, newfd, cint(0))
proc fork*(): Pid {.inline, sideEffect.} =
  const SIGCHLD = culong(17)
  clone(SIGCHLD, nil, nil, nil, nil)

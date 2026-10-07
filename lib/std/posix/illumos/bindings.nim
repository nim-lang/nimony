# Private include. All illumos configurations link libc; no raw syscalls.
{.passL: "-lsocket -lnsl".}

proc open*(path: cstring; flags: cint; mode: Mode = 0): cint {.importc: "open", sideEffect.}
proc ftruncate*(fd: cint; size: Off): cint {.importc: "ftruncate".}
proc stat*(path: cstring; buf: var Stat): cint {.importc: "stat".}
proc lstat*(path: cstring; buf: var Stat): cint {.importc: "lstat", sideEffect.}
proc fstat*(fd: cint; buf: var Stat): cint {.importc: "fstat", sideEffect.}
proc mmap*(a1: nil pointer; a2: csize_t; a3, a4, a5: cint; a6: Off): pointer {.importc: "mmap".}
proc readlink*(path, buf: cstring; size: int): int {.importc: "readlink".}
proc symlink*(target, path: cstring): cint {.importc: "symlink".}
proc chmod*(path: cstring; mode: Mode): cint {.importc: "chmod", sideEffect.}
proc mkdir*(path: cstring; mode: Mode): cint {.importc: "mkdir", sideEffect.}
proc rmdir*(path: cstring): cint {.importc: "rmdir", sideEffect.}
proc unlink*(path: cstring): cint {.importc: "unlink", sideEffect.}
proc pipe*(fds: ptr cint): cint {.importc: "pipe", sideEffect.}
proc dup2*(oldfd, newfd: cint): cint {.importc: "dup2", sideEffect.}
proc fork*(): Pid {.importc: "fork", sideEffect.}
proc posix_fallocate*(fd: cint; offset, len: Off): cint {.importc: "posix_fallocate".}
proc waitpid*(pid: Pid; status: var cint; options: cint): Pid {.importc: "waitpid", sideEffect.}

# sys/wait.h: WCONTINUED (8) is an OPTION, not the status word (0xffff).
proc WEXITSTATUS*(s: cint): cint = (s shr 8) and 0xff
proc WTERMSIG*(s: cint): cint = s and 0x7f
proc WSTOPSIG*(s: cint): cint = (s shr 8) and 0xff
proc WIFEXITED*(s: cint): bool = (s and 0xff) == 0
proc WIFSIGNALED*(s: cint): bool = (s and 0xff) > 0 and (s and 0xff00) == 0
proc WIFSTOPPED*(s: cint): bool = (s and 0xff) == 0x7f and (s and 0xff00) != 0
proc WIFCONTINUED*(s: cint): bool = (s and 0xffff) == 0xffff

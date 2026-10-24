# Private implementation included by std/posix/posix; not a standalone module.

# Linux `getdents64`; exported by glibc (2.30+) and musl, and lowered to the
# raw syscall by arkham. Each record is a `struct linux_dirent64`:
#   d_ino @0 (u64), d_off @8 (s64), d_reclen @16 (u16),
#   d_type @18 (u8), d_name @19 (NUL-terminated, variable length).
when defined(android) and not defined(nimNoLibc):
  # Bionic doesn't export `getdents64`; its generic `syscall()` wrapper reaches
  # the same kernel call, and `../dirbuf` parses the records unchanged.
  const SYS_getdents64 =
    when defined(amd64): clong(217)
    elif defined(i386): clong(220)
    elif defined(arm64) or defined(riscv64): clong(61)
    elif defined(arm): clong(217)
    else: clong(217)
  proc syscall(number: clong): clong {.importc: "syscall", varargs, sideEffect.}
else:
  proc getdents64(fd: cint; dirp: pointer; count: int): clong {.importc: "getdents64", sideEffect.}

proc readDirRecords(fd: cint; buf: pointer; size: int; cookie: ptr Off): clong {.sideEffect.} =
  when defined(android) and not defined(nimNoLibc):
    pcall(syscall(SYS_getdents64, clong(fd), buf, clong(size)))
  else:
    pcall(getdents64(fd, buf, size))

const
  direntReclenAt* = 16
  direntTypeAt* = 18
  direntNameAt* = 19
  direntSkipsZeroIno* = false

include "../dirbuf"

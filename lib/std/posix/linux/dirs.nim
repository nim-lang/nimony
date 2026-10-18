# Private implementation included by std/posix/posix; not a standalone module.

# Linux `getdents64`; exported by glibc (2.30+) and musl, and lowered to the
# raw syscall by arkham. Each record is a `struct linux_dirent64`:
#   d_ino @0 (u64), d_off @8 (s64), d_reclen @16 (u16),
#   d_type @18 (u8), d_name @19 (NUL-terminated, variable length).
proc getdents64(fd: cint; dirp: pointer; count: int): clong {.importc: "getdents64", sideEffect.}

proc readDirRecords(fd: cint; buf: pointer; size: int; cookie: ptr Off): clong {.sideEffect.} =
  pcall(getdents64(fd, buf, size))

const
  direntReclenAt* = 16
  direntTypeAt* = 18
  direntNameAt* = 19
  direntSkipsZeroIno* = false

include "../dirbuf"

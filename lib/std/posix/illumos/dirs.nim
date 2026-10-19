# Private implementation included by std/posix/posix; not a standalone module.

# illumos `getdents(2)`, the call libc's `readdir` is built on. Each record is
# the LP64 `struct dirent`: d_ino @0 (u64), d_off @8 (s64), d_reclen @16 (u16),
# d_name @18 (NUL-terminated). There is no d_type: `std/dirs` falls back to
# `lstat` for `DT_UNKNOWN`.
proc getdents(fd: cint; buf: pointer; nbyte: csize_t): cint {.importc: "getdents", sideEffect.}

proc readDirRecords(fd: cint; buf: pointer; size: int; cookie: ptr Off): clong {.sideEffect.} =
  pcall(getdents(fd, buf, csize_t(size)))

const
  direntReclenAt* = 16
  direntTypeAt* = -1
  direntNameAt* = 18
  direntSkipsZeroIno* = false

include "../dirbuf"

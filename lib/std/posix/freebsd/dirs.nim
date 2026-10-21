# Private implementation included by std/posix/posix; not a standalone module.

# FreeBSD `getdirentries`, both a libc function and (freestanding) the trap
# itself. Since FreeBSD 12 it writes the 64-bit-inode `struct dirent`:
#   d_fileno @0 (u64), d_off @8 (s64), d_reclen @16 (u16), d_type @18 (u8),
#   d_namlen @20 (u16), d_name @24 (NUL-terminated).
# A record whose d_fileno is 0 is the leftover slot of a deleted entry.
const
  O_DIRECTORY = cint(0x20000)

proc getdirentries(fd: cint; buf: pointer; nbytes: int;
                   basep: ptr Off): clong {.importc: "getdirentries", sideEffect.}

proc readDirRecords(fd: cint; buf: pointer; size: int; cookie: ptr Off): clong {.sideEffect.} =
  pcall(getdirentries(fd, buf, size, cookie))

const
  direntReclenAt* = 16
  direntTypeAt* = 18
  direntNameAt* = 24
  direntSkipsZeroIno* = true

include "../dirbuf"

# Private implementation included by std/posix/posix; not a standalone module.

# Hardcoded Linux/i386 ABI: glibc's `struct stat64` (96 bytes), so `Off`
# stays 64-bit like every other target. The stat procs in bindings.nim bind the
# `stat64`/`lstat64`/`fstat64` symbols on this architecture to match.
# i386 aligns 64-bit integers to 4 bytes, which is why st_size can sit at
# offset 44; nimony's C backend inherits that rule from the C compiler.
type
  Mode* = uint32   ## mode_t
  Off* = int64     ## off_t
  Dev* = uint64    ## dev_t (64-bit even on i386 in the LFS layout)
  Ino* = uint64    ## ino64_t

  Stat* {.pure.} = object ## Linux/i386 `struct stat64`
    st_dev*: Dev              # offset 0
    pad0: uint32              # 8
    st_ino32: uint32          # 12 (truncated inode; real one at the tail)
    st_mode*: Mode            # 16
    st_nlink: uint32          # 20
    st_uid: uint32            # 24
    st_gid: uint32            # 28
    st_rdev: Dev              # 32
    pad3: uint32              # 40
    st_size*: Off             # 44
    st_blksize: int32         # 52
    st_blocks: int64          # 56
    st_atim: Timespec         # 64
    st_mtim*: Timespec        # 72
    st_ctim: Timespec         # 80
    st_ino*: Ino              # 88

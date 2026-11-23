# Private implementation included by std/posix/posix; not a standalone module.

# Hardcoded Linux/ARM (32-bit EABI) ABI: the kernel's `struct stat64`
# (arch/arm/include/uapi/asm/stat.h), 104 bytes, which glibc's `struct stat64`
# and bionic's `struct stat` follow. The fields are i386's, but EABI aligns
# 64-bit integers to 8, so st_size sits at 48 (i386: 44) and everything after
# it moves. The padding is spelled out so no alignment rule is needed. As on
# i386, bindings.nim binds the `stat64`/`lstat64`/`fstat64` symbols.
type
  Mode* = uint32   ## mode_t
  Off* = int64     ## off_t
  Dev* = uint64    ## dev_t (the stat64 fields are 64-bit)
  Ino* = uint64    ## ino64_t

  Stat* {.pure.} = object ## Linux/ARM `struct stat64`
    st_dev*: Dev              # offset 0
    pad0: uint32              # 8
    st_ino32: uint32          # 12 (truncated inode; real one at the tail)
    st_mode*: Mode            # 16
    st_nlink: uint32          # 20
    st_uid: uint32            # 24
    st_gid: uint32            # 28
    st_rdev: Dev              # 32
    pad3: uint32              # 40
    padSize: uint32           # 44 (aligns st_size to 8)
    st_size*: Off             # 48
    st_blksize: uint32        # 56
    padBlocks: uint32         # 60 (aligns st_blocks to 8)
    st_blocks: uint64         # 64
    st_atim: Timespec         # 72
    st_mtim*: Timespec        # 80
    st_ctim: Timespec         # 88
    st_ino*: Ino              # 96

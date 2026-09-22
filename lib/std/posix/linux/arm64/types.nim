# Private implementation included by std/posix/posix; not a standalone module.

# Hardcoded Linux/arm64 ABI: the kernel's asm-generic `struct stat`
# (include/uapi/asm-generic/stat.h), 128 bytes, also used by riscv64 and
# loongarch64. Note the field ORDER differs from amd64 (mode before nlink,
# both 32-bit) and the trailing padding is two u32s.
type
  Mode* = uint32   ## mode_t
  Off* = int64     ## off_t
  Dev* = uint      ## dev_t
  Ino* = uint      ## ino_t

  Stat* {.pure.} = object ## Linux/arm64 `struct stat` (asm-generic)
    st_dev*: Dev              # offset 0
    st_ino*: Ino              # 8
    st_mode*: Mode            # 16
    st_nlink: uint32          # 20
    st_uid: uint32            # 24
    st_gid: uint32            # 28
    st_rdev: Dev              # 32
    pad1: uint64              # 40
    st_size*: Off             # 48
    st_blksize: int32         # 56
    pad2: int32               # 60
    st_blocks: int64          # 64
    st_atim: Timespec         # 72
    st_mtim*: Timespec        # 88
    st_ctim: Timespec         # 104
    unused4: uint32           # 120
    unused5: uint32           # 124

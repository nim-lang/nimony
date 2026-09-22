# Private implementation included by std/posix/posix; not a standalone module.

# Hardcoded Linux/amd64 ABI. `struct stat` is 144 bytes; only the fields
# other modules actually read are exposed — the rest are correctly-sized
# private padding so `fstat` writes land at the right offsets.
type
  Mode* = uint32   ## mode_t
  Off* = int64     ## off_t
  Dev* = uint      ## dev_t
  Ino* = uint      ## ino_t

  Stat* {.pure.} = object ## Linux/amd64 `struct stat`
    st_dev*: Dev              # offset 0
    st_ino*: Ino              # 8
    st_nlink: uint            # 16
    st_mode*: Mode            # 24
    st_uid: uint32            # 28
    st_gid: uint32            # 32
    pad0: int32               # 36
    st_rdev: Dev              # 40
    st_size*: Off             # 48
    st_blksize: int64         # 56
    st_blocks: int64          # 64
    st_atim: Timespec         # 72
    st_mtim*: Timespec        # 88
    st_ctim: Timespec         # 104
    glibcReserved: array[3, int64]  # 120 .. 143

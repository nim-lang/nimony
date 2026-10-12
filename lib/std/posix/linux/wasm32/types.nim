# Private implementation included by std/posix/posix; not a standalone module.

# Hardcoded Linux/wasm32 ABI as provided by Emscripten's musl (ILP32 with a
# 64-bit `off_t`, `ino_t` and `time_t`). `struct stat` is Emscripten's own
# 96-byte layout. 64-bit integers are 8-byte aligned on wasm32, unlike i386,
# so `st_size` sits at offset 24 with no padding before it.
type
  Mode* = uint32   ## mode_t
  Off* = int64     ## off_t
  Dev* = uint32    ## dev_t
  Ino* = uint64    ## ino_t

  Stat* {.pure.} = object ## Emscripten wasm32 `struct stat`
    st_dev*: Dev              # offset 0
    st_mode*: Mode            # 4
    st_nlink: uint32          # 8
    st_uid: uint32            # 12
    st_gid: uint32            # 16
    st_rdev: Dev              # 20
    st_size*: Off             # 24
    st_blksize: int32         # 32
    st_blocks: int32          # 36
    st_atim: Timespec         # 40
    st_mtim*: Timespec        # 56
    st_ctim: Timespec         # 72
    st_ino*: Ino              # 88

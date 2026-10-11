# illumos LP64 ABI, verified against sys/stat.h with a -m64 C probe.
type
  Mode* = uint32
  Off* = int64
  Dev* = uint64
  Ino* = uint64
  Stat* {.pure.} = object # 128 bytes, 8-aligned
    st_dev*: Dev
    st_ino*: Ino
    st_mode*: Mode
    st_nlink: uint32
    st_uid: uint32
    st_gid: uint32
    st_rdev: Dev
    st_size*: Off
    st_atim: Timespec
    st_mtim*: Timespec
    st_ctim: Timespec
    st_blksize: int32
    st_blocks: int64
    st_fstype: array[16, char]

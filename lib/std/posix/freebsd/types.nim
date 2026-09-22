# Private implementation included by std/posix/posix; not a standalone module.

when not (defined(amd64) or defined(arm64)):
  {.error: "std/posix has no transcribed ABI for this FreeBSD architecture; supported: amd64, arm64".}

type
  Sockaddr_storage* {.pure.} = object
    abi: array[16, uint64] # 128 bytes
  TSa_Family* = uint8  ## sa_family_t

  Sockaddr_in* {.pure.} = object ## struct sockaddr_in (BSD layout with sin_len)
    sin_len*: uint8            ## sizeof(struct sockaddr_in); FreeBSD checks it
    sin_family*: TSa_Family
    sin_port*: cushort         ## network byte order
    sin_addr*: InAddr
    sin_zero: array[8, char]

  SockAddr* {.pure.} = object ## struct sockaddr (BSD layout with sa_len)
    sa_len: uint8                 ## Total length of the address.
    sa_family*: TSa_Family        ## Address family.
    sa_data*: array[0..255, char] ## Socket address (variable-length data).

  Tmsghdr* {.pure} = object ## FreeBSD: int msg_iovlen, socklen_t msg_controllen
    msg_name*: pointer
    msg_namelen*: SockLen
    msg_iov*: ptr IOVec
    msg_iovlen*: cint
    msg_control*: pointer
    msg_controllen*: SockLen
    msg_flags*: cint

# Hardcoded FreeBSD (12+, 64-bit inode) `struct stat` for LP64 targets;
# amd64 and arm64 share it (only i386 inserts `__STAT_TIME_T_EXT`
# padding). `struct stat` is 224 bytes.
type
  Mode* = uint16   ## mode_t
  Off* = int64     ## off_t
  Dev* = uint64    ## dev_t
  Ino* = uint64    ## ino_t

  Stat* {.pure.} = object ## FreeBSD `struct stat`
    st_dev*: Dev              # offset 0
    st_ino*: Ino              # 8
    st_nlink: uint64          # 16
    st_mode*: Mode            # 24
    st_bsdflags: int16        # 26
    st_uid: uint32            # 28
    st_gid: uint32            # 32
    st_padding1: int32        # 36
    st_rdev: Dev              # 40
    st_atim: Timespec         # 48
    st_mtim*: Timespec        # 64
    st_ctim: Timespec         # 80
    st_birthtim: Timespec     # 96
    st_size*: Off             # 112
    st_blocks: int64          # 120
    st_blksize: int32         # 128
    st_flags: uint32          # 132
    st_gen: uint64            # 136
    st_filerev: uint64        # 144
    st_spare: array[9, uint64]  # 152 .. 223

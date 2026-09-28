# Private implementation included by std/posix/posix; not a standalone module.

type
  Sockaddr_storage* {.pure.} = object
    abi: array[16, uint64] # 128 bytes
  TSa_Family* = uint8  ## sa_family_t

  Sockaddr_in* {.pure.} = object ## struct sockaddr_in (BSD layout with sin_len)
    sin_len: uint8
    sin_family*: TSa_Family
    sin_port*: cushort         ## network byte order
    sin_addr*: InAddr
    sin_zero: array[8, char]

  SockAddr* {.pure.} = object ## struct sockaddr (BSD layout with sa_len)
    sa_len: uint8                 ## Total length of the address.
    sa_family*: TSa_Family        ## Address family.
    sa_data*: array[0..255, char] ## Socket address (variable-length data).

  Tmsghdr* {.pure} = object  ## struct msghdr (Darwin: msg_iovlen is int)
    msg_name*: pointer     ## Optional address.
    msg_namelen*: SockLen  ## Size of address.
    msg_iov*: ptr IOVec    ## Scatter/gather array.
    msg_iovlen*: cint      ## Members in msg_iov.
    msg_control*: pointer  ## Ancillary data; see below.
    msg_controllen*: SockLen ## Ancillary data buffer len.
    msg_flags*: cint ## Flags on received message.

# Hardcoded macOS (`__DARWIN_64_BIT_INO_T`) `struct stat`. The layout is
# the same on arm64 and x86_64 (both use the 64-bit-inode ABI);
# `struct stat` is 144 bytes. Only the fields other modules read are
# exposed by name — the rest are correctly-sized private padding so
# `fstat` writes land at the right offsets.
type
  Mode* = uint16   ## mode_t
  Off* = int64     ## off_t
  Dev* = int32     ## dev_t
  Ino* = uint64    ## ino_t

  Stat* {.pure.} = object ## macOS `struct stat`
    st_dev*: Dev              # offset 0
    st_mode*: Mode            # 4
    st_nlink: uint16          # 6
    st_ino*: Ino              # 8
    st_uid: uint32            # 16
    st_gid: uint32            # 20
    st_rdev: Dev              # 24
    pad0: int32               # 28 (pad to 8-align the timespecs)
    st_atim: Timespec         # 32
    st_mtim*: Timespec        # 48
    st_ctim: Timespec         # 64
    st_birthtim: Timespec     # 80
    st_size*: Off             # 96
    st_blocks: int64          # 104
    st_blksize: int32         # 112
    st_flags: uint32          # 116
    st_gen: uint32            # 120
    st_lspare: int32          # 124
    st_qspare: array[2, int64]  # 128 .. 143

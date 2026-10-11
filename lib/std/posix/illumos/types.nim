# Private include. Socket structures use the X/Open ABI (__xnet_* symbols).
when defined(amd64):
  include "amd64/types"
else:
  {.error: "std/posix: illumos ABI is currently transcribed only for amd64".}

type
  TSa_Family* = uint16
  Sockaddr_storage* {.pure.} = object
    abi: array[32, uint64] # 256 bytes, 8-aligned (sys/socket_impl.h)
  Sockaddr_in* {.pure.} = object
    sin_family*: TSa_Family
    sin_port*: cushort
    sin_addr*: InAddr
    sin_zero: array[8, char]
  SockAddr* {.pure.} = object
    sa_family*: TSa_Family
    sa_data*: array[0..255, char]
  Tmsghdr* {.pure.} = object
    msg_name*: pointer
    msg_namelen*: SockLen
    msg_iov*: ptr IOVec
    msg_iovlen*: cint
    msg_control*: pointer
    msg_controllen*: SockLen
    msg_flags*: cint

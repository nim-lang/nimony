# ABI-precise POSIX declarations. Nimony does not import declarations from C
# headers: the LLVM and native backends never see headers, and on the C
# backend mixing header prototypes with our own declarations redeclares libc
# symbols with conflicting types (nim-lang/nimony#2155). Every type and
# constant here is transcribed from the platform ABI and machine-checked by
# tests/nimony/stdlib/tposixabi.nim.

when defined(linux):
  when not (defined(amd64) or defined(arm64) or defined(i386)):
    {.error: "std/posix has no transcribed ABI for this Linux architecture; supported: amd64, arm64, i386".}
elif defined(osx):
  discard # one ABI for arm64 and x86_64 (64-bit-inode layouts)
else:
  {.error: "std/posix has no transcribed ABI for this OS; supported: Linux (amd64/arm64/i386) and macOS".}

include posix_other_consts

type
  ClockId* = cint  ## clockid_t (int on Linux and Darwin)

  Time* = distinct clong   ## time_t

  Timespec* {.pure.} = object ## struct timespec: `long` seconds + `long`
                              ## nanoseconds on both Linux and Darwin
    tv_sec*: Time  ## Seconds.
    tv_nsec*: clong  ## Nanoseconds.

  IOVec* {.pure} = object ## struct iovec
    iov_base*: pointer ## Base address of a memory region for input or output.
    iov_len*: csize_t  ## The size of the memory pointed to by iov_base.

  SockLen* = cuint  ## socklen_t

  Sockaddr_storage* {.importc: "struct sockaddr_storage".} = object
  Sockaddr_in* {.importc: "struct sockaddr_in".} = object
    sin_family*: cushort
    sin_port*: cushort
    sin_addr*: InAddr
  InAddr* {.importc: "struct in_addr".} = object
    s_addr*: uint32

when defined(linux):
  type
    TSa_Family* = uint16  ## sa_family_t

    SockAddr* {.pure.} = object ## struct sockaddr
      sa_family*: TSa_Family        ## Address family (offset 0, no sa_len).
      sa_data*: array[0..255, char] ## Socket address (variable-length data).

    Tmsghdr* {.pure} = object  ## struct msghdr (Linux: msg_iovlen and
                               ## msg_controllen are size_t; natural alignment
                               ## pads msg_namelen out to the pointer size on
                               ## 64-bit, matching glibc/musl and the kernel)
      msg_name*: pointer     ## Optional address.
      msg_namelen*: SockLen  ## Size of address.
      msg_iov*: ptr IOVec    ## Scatter/gather array.
      msg_iovlen*: csize_t   ## Members in msg_iov.
      msg_control*: pointer  ## Ancillary data; see below.
      msg_controllen*: csize_t ## Ancillary data buffer len.
      msg_flags*: cint ## Flags on received message.
else:
  type
    TSa_Family* = uint8  ## sa_family_t

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

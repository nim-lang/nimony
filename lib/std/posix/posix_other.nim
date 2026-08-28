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

  InAddr* {.pure.} = object ## struct in_addr
    s_addr*: uint32

  Sockaddr_storage* {.pure.} = object ## struct sockaddr_storage: 128 bytes,
                                      ## 8-aligned on both ABIs. The payload is
                                      ## opaque; only its *size* matters, and it
                                      ## has to be right — accept(2) and
                                      ## recvfrom(2) are handed `sizeof` this as
                                      ## the caller's buffer length.
    abi: array[16, uint64]

when defined(linux):
  type
    TSa_Family* = uint16  ## sa_family_t

    Sockaddr_in* {.pure.} = object ## struct sockaddr_in
      sin_family*: TSa_Family
      sin_port*: cushort         ## network byte order
      sin_addr*: InAddr
      sin_zero: array[8, char]   ## padding to sizeof(struct sockaddr); the
                                 ## kernel rejects a bind(2)/connect(2) whose
                                 ## addrlen is short of the full 16 bytes

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

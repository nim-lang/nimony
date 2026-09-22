# Private implementation included by std/posix/posix; not a standalone module.

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
when defined(amd64):
  include "amd64/types"
  include "amd64/consts"
elif defined(arm64):
  include "arm64/types"
  include "arm64/consts"
elif defined(i386):
  include "i386/types"
  include "i386/consts"
else:
  {.error: "std/posix has no transcribed ABI for this Linux architecture; supported: amd64, arm64, i386".}

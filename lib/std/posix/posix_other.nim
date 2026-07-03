
# Regenerate using detect.nim!
include posix_other_consts


type
  # TODO: fixme {.importc: "clockid_t", header: "<sys/types.h>", nodecl.}
  ClockId* = cint

when defined(nimNativeIo) and (defined(amd64) or defined(osx)):
  # `time_t` and `struct timespec` have the same shape on Linux/amd64 and
  # macOS/arm64 (LP64): a `long` seconds field followed by a `long`
  # nanoseconds field, 16 bytes total. Hardcoding it means no <time.h>.
  type
    Time* = distinct clong   ## time_t, hardcoded so no <time.h>

    Timespec* {.pure.} = object ## struct timespec (16 bytes)
      tv_sec*: Time  ## Seconds.
      tv_nsec*: clong  ## Nanoseconds.
    
    IOVec* {.pure} = object ## struct iovec
      iov_base*: pointer ## Base address of a memory region for input or output.
      iov_len*: csize_t  ## The size of the memory pointed to by iov_base.
    
    SockLen* = cuint
    TSa_Family* = uint8

    SockAddr* {.pure.} = object ## struct sockaddr
      sa_family*: TSa_Family         ## Address family.
      sa_data*: array[0..255, char] ## Socket address (variable-length data).
    
    Tmsghdr* {.pure} = object  ## struct msghdr
      msg_name*: pointer     ## Optional address.
      msg_namelen*: SockLen  ## Size of address.
      msg_iov*: ptr IOVec    ## Scatter/gather array.
      msg_iovlen*: cint      ## Members in msg_iov.
      msg_control*: pointer  ## Ancillary data; see below.
      msg_controllen*: SockLen ## Ancillary data buffer len.
      msg_flags*: cint ## Flags on received message.
else:
  type
    Time* {.importc: "time_t", header: "<time.h>", nodecl.} = distinct clong

    Timespec* {.importc: "struct timespec",
                 header: "<time.h>", final, pure.} = object ## struct timespec
      tv_sec* {.exportc.} : Time  ## Seconds.
      tv_nsec* {.exportc.} : clong  ## Nanoseconds.
    
    IOVec* {.importc: "struct iovec", pure, final,
            header: "<sys/uio.h>".} = object ## struct iovec
      iov_base*: pointer ## Base address of a memory region for input or output.
      iov_len*: csize_t  ## The size of the memory pointed to by iov_base.
    
    SockLen* {.importc: "socklen_t", header: "<sys/socket.h>".} = cuint
    TSa_Family* {.importc: "sa_family_t", header: "<sys/socket.h>".} = uint8

    SockAddr* {.importc: "struct sockaddr", header: "<sys/socket.h>",
                pure, final.} = object ## struct sockaddr
      sa_family*: TSa_Family         ## Address family.
      sa_data*: array[0..255, char] ## Socket address (variable-length data).
    
    Tmsghdr* {.importc: "struct msghdr", pure, final,
             header: "<sys/socket.h>".} = object  ## struct msghdr
      msg_name*: pointer     ## Optional address.
      msg_namelen*: SockLen  ## Size of address.
      msg_iov*: ptr IOVec    ## Scatter/gather array.
      msg_iovlen*: cint      ## Members in msg_iov.
      msg_control*: pointer  ## Ancillary data; see below.
      msg_controllen*: SockLen ## Ancillary data buffer len.
      msg_flags*: cint ## Flags on received message.


when defined(linux):
  var
    MAP_POPULATE* {.importc, header: "<sys/mman.h>".}: cint
      ## Populate (prefault) page tables for a mapping.
else:
  var
    MAP_POPULATE*: cint = 0
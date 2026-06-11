
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
else:
  type
    Time* {.importc: "time_t", header: "<time.h>", nodecl.} = distinct clong

    Timespec* {.importc: "struct timespec",
                 header: "<time.h>", final, pure.} = object ## struct timespec
      tv_sec* {.exportc.} : Time  ## Seconds.
      tv_nsec* {.exportc.} : clong  ## Nanoseconds.



# Regenerate using detect.nim!
include posix_other_consts


type
  # TODO: fixme {.importc: "clockid_t", header: "<sys/types.h>", nodecl.}
  ClockId* = cint

when defined(nimNativeIo) and defined(amd64):
  type
    Time* = distinct clong   ## time_t, hardcoded so no <time.h>

    Timespec* {.pure.} = object ## struct timespec (Linux/amd64, 16 bytes)
      tv_sec*: Time  ## Seconds.
      tv_nsec*: clong  ## Nanoseconds.
else:
  type
    Time* {.importc: "time_t", header: "<time.h>", nodecl.} = distinct clong

    Timespec* {.importc: "struct timespec",
                 header: "<time.h>", final, pure.} = object ## struct timespec
      tv_sec* {.exportc.} : Time  ## Seconds.
      tv_nsec* {.exportc.} : clong  ## Nanoseconds.


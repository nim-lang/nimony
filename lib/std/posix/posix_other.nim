
# Regenerate using detect.nim!
include posix_other_consts


type
  # TODO: fixme {.importc: "clockid_t", header: "<sys/types.h>", nodecl.}
  ClockId* = cint

  Time* {.importc: "time_t", header: "<time.h>", nodecl.} = distinct clong

  Timespec* {.importc: "struct timespec",
               header: "<time.h>", final, pure.} = object ## struct timespec
    tv_sec* {.exportc.} : Time  ## Seconds.
    tv_nsec* {.exportc.} : clong  ## Nanoseconds.


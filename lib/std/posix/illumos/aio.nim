# Private include of std/posix/event_port. All calls are libc-backed.
when defined(amd64):
  include "amd64/aio"
else:
  {.error: "illumos AIO ABI is currently transcribed only for amd64".}

const
  SIGEV_NONE* = cint(1)
  SIGEV_SIGNAL* = cint(2)
  SIGEV_THREAD* = cint(3)
  SIGEV_PORT* = cint(4)
  AIO_CANCELED* = cint(0)
  AIO_ALLDONE* = cint(1)
  AIO_NOTCANCELED* = cint(2)
  LIO_NOWAIT* = cint(0)
  LIO_WAIT* = cint(1)
  LIO_NOP* = cint(0)
  LIO_READ* = cint(1)
  LIO_WRITE* = cint(2)

proc aio_read*(cb: ptr AioCb): cint {.importc: "aio_read", sideEffect.}
proc aio_write*(cb: ptr AioCb): cint {.importc: "aio_write", sideEffect.}
proc aio_error*(cb: ptr AioCb): cint {.importc: "aio_error", sideEffect.}
proc aio_return*(cb: ptr AioCb): int {.importc: "aio_return", sideEffect.}
proc aio_cancel*(fd: cint; cb: nil ptr AioCb): cint {.importc: "aio_cancel", sideEffect.}
proc aio_suspend*(list: ptr UncheckedArray[ptr AioCb]; count: cint;
                  timeout: nil ptr Timespec): cint {.importc: "aio_suspend", sideEffect.}
proc aio_fsync*(op: cint; cb: ptr AioCb): cint {.importc: "aio_fsync", sideEffect.}

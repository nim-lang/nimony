# illumos LP64: sys/siginfo.h, sys/aio.h, sys/aiocb.h.
# Pointer-valued sigval is sufficient for SIGEV_PORT. Function/attribute
# pointers are opaque here; this interface does not synthesize callbacks.
type
  SigEvent* {.pure.} = object # 40 bytes, 8-aligned
    sigev_notify*: cint
    sigev_signo*: cint
    sigev_value*: nil pointer
    sigev_notify_function*: nil pointer
    sigev_notify_attributes*: nil pointer
    pad: cint
  AioResult* {.pure.} = object
    aio_return*: int
    aio_errno*: cint
  AioCb* {.pure.} = object # 112 bytes, 8-aligned
    aio_fildes*: cint
    aio_buf*: nil pointer
    aio_nbytes*: csize_t
    aio_offset*: Off
    aio_reqprio*: cint
    aio_sigevent*: SigEvent
    aio_lio_opcode*: cint
    aio_resultp*: AioResult
    aio_state*: cint
    pad: cint

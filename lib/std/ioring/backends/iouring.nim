# Linux io_uring backend with epoll fallback.
# NOTE: This was written for the old struct-based Backend.
# Needs rewriting using the OOP Backend interface (like epoll.nim).

import ../platform
import ../core/types

type IoUringBackend* = ref object
  epoll: Backend
  ringAvail: bool
  pendingSqes: int
  cqeCb: proc (userData: uint64; res: int32) {.nimcall.}

proc initIoUringBackend*(): Backend =
  new result

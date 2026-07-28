# Windows IOCP backend.
# NOTE: This was written for the old struct-based Backend.
# Needs rewriting using the OOP Backend interface (like epoll.nim).

import ../platform
import ../core/types
import ../core/backend

type IocpBackend* = ref object
  fd: cint

proc initIocpBackend*(): Backend =
  new result

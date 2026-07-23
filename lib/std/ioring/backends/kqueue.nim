# macOS/BSD kqueue backend — oneshot fd registration + poll.
# NOTE: This was written for the old struct-based Backend.
# Needs rewriting using the OOP Backend interface (like epoll.nim).

import ../platform
import ../core/types

type KqueueBackend* = ref object
  fd: cint

proc initKqueueBackend*(): Backend =
  new result

# Windows IOCP backend (stub).

import ../core/types
import ../core/backend

var dummyFd: cint

proc dummyPoll(timeoutMs: int): bool {.nimcall.} = false
proc dummyClose() {.nimcall.} = discard
proc dummyForgetFd(fd: cint) {.nimcall.} = discard

proc initIocpBackendRelays*(): BackendRelays =
  result = BackendRelays(
    poll: dummyPoll,
    close: dummyClose,
    forgetFd: dummyForgetFd,
  )

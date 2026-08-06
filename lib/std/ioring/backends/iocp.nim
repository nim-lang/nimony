# Windows IOCP backend (stub).

import ../core/types
import ../core/backend

var dummyFd: cint

proc dummySubmit(slotIdx: int; op: ptr OpContext) {.nimcall.} = discard
proc dummyPoll(timeoutMs: int): bool {.nimcall.} = false
proc dummyClose() {.nimcall.} = discard
proc dummyForgetFd(fd: cint) {.nimcall.} = discard

proc initIocpBackendRelays*(): BackendRelays =
  result = BackendRelays(
    submit: dummySubmit,
    poll: dummyPoll,
    close: dummyClose,
    forgetFd: dummyForgetFd,
  )

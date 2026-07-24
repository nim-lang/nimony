# Common types shared across all ioring layers.
import std/posix/posix

type
  IoOp* = enum
    opRead, opWrite, opAccept

  SeqNum* = uint32

  IoCompletion* = object
    id*: SeqNum
    op*: IoOp
    fd*: FileHandle
    result*: int

  OpContext* = object
    inUse*: bool
    kind*: IoOp
    fd*: FileHandle
    seqnum*: SeqNum
    buf*: nil pointer
    len*: int
    cont*: Continuation
    res*: int
    acceptAddr*: Sockaddr_storage
    acceptLen*: SockLen

  Backend* = ref object of RootObj
    completeFn*: proc(slotIdx: int; res: int; env: int) {.nimcall.}
    completeEnv*: int

method submit*(b: Backend; slotIdx: int; op: ptr OpContext) {.base.} =
  discard

method close*(b: Backend) {.base.} =
  discard

method poll*(b: Backend; timeoutMs: int): bool {.base.} =
  return false

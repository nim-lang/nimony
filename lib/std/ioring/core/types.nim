# Common types shared across all ioring layers.
import std/posix/posix

type
  IoOp* = enum
    opNop, opRead, opWrite, opAccept

  SeqNum* = uint32

  IoCompletion* = object
    id*: SeqNum
    op*: IoOp
    fd*: FileHandle
    result*: int

  OpContext* = object
    kind*: IoOp
    fd*: FileHandle
    seqnum*: SeqNum
    buf*: nil pointer
    len*: int
    cont*: Continuation
    res*: int
    acceptAddr*: Sockaddr_storage
    acceptLen*: SockLen

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
    # Intrusive doubly-linked list of every in-flight slot that shares `fd`,
    # so a readiness event can find "all ops for this fd" in O(k) (k = ops on
    # this fd) instead of scanning the whole arena. -1 means "no neighbour".
    nextInFd*: int
    prevInFd*: int

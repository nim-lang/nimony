# Common types shared across all ioring layers.
import std/posix/posix

const
  EvRead* = 1
  EvWrite* = 2

type
  IoOp* = enum
    opNop, opRead, opWrite, opAccept, opPollAdd

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
    pollMask*: int
      ## opPollAdd only: the direction(s) the caller actually waits for
      ## (EvRead / EvWrite / both). Without it a readiness probe has to arm
      ## both directions, and a caller waiting to READ is woken every time the
      ## fd is merely WRITABLE — which, for a socket, is almost always. Since
      ## the op is oneshot, that caller's re-arm turns into a hot spin.
    acceptAddr*: Sockaddr_storage
    acceptLen*: SockLen

# Common types shared across all ioring layers.
when defined(posix):
  import std/posix/posix
else:
  # Windows: the ring's socket surface is Winsock and `std/posix/posix` is
  # empty there, so the POSIX-named types the op layout needs are declared
  # here in their Winsock ABI shapes. `Sockaddr_storage` is the 128-byte
  # `SOCKADDR_STORAGE`; `SockLen` is the `int` namelen Winsock's accept takes;
  # `FileHandle` is the ring's fd — a Winsock `SOCKET` narrowed to `cint`
  # (see ioring.nim's Windows arm for why that narrowing is sound).
  type
    FileHandle* = cint
    SockLen* = cint
    Sockaddr_storage* {.pure.} = object
      ss_family*: uint16
      ss_pad*: array[126, uint8]

const
  # Result of an op cancelled by `closeFd` before it completed: the ring's own
  # convention (mirrors -ECANCELED), reported identically by every backend —
  # readiness/POSIX through `cancelPendingOps`, IOCP when the kernel aborts an
  # overlapped op on closesocket (STATUS_CANCELLED).
  ECancelled* = -125

type
  IoEvent* = enum
    ## A readiness direction. `submitPollAdd` takes a set of these, and an
    ## `opPollAdd` completion reports the set that actually fired.
    evRead   ## readable — data is available, or a listener has a pending connection
    evWrite  ## writable — the send buffer has room

  IoEvents* = set[IoEvent]

  IoOp* = enum
    opNop, opRead, opWrite, opAccept, opPollAdd

  SeqNum* = uint32

  IoCompletion* = object
    id*: SeqNum
    op*: IoOp
    fd*: FileHandle
    result*: int
      ## Op-dependent: a byte count for `opRead`/`opWrite`, the accepted fd for
      ## `opAccept`, -1 on failure — and for `opPollAdd` the fired directions
      ## encoded as a bit mask, which `readyEvents` decodes into `IoEvents`.

  OpContext* = object
    kind*: IoOp
    fd*: FileHandle
    seqnum*: SeqNum
    buf*: nil pointer
    len*: int
    cont*: Continuation
    res*: int
    pollMask*: IoEvents
      ## opPollAdd only: the direction(s) the caller actually waits for.
      ## Without it a readiness probe has to arm both directions, and a caller
      ## waiting to READ is woken every time the fd is merely WRITABLE — which,
      ## for a socket, is almost always. Since the op is oneshot, that caller's
      ## re-arm turns into a hot spin.
    acceptAddr*: Sockaddr_storage
    acceptLen*: SockLen

proc toEventMask*(events: IoEvents): int {.inline.} =
  ## Encode `events` for the plain `int` channels a completion travels through
  ## (`IoCompletion.result` and the `resPtr` out-parameter), neither of which
  ## can carry a set.
  result = 0
  if evRead in events: result = result or (1 shl ord(evRead))
  if evWrite in events: result = result or (1 shl ord(evWrite))

proc toIoEvents*(mask: int): IoEvents {.inline.} =
  ## Inverse of `toEventMask`.
  result = {}
  if (mask and (1 shl ord(evRead))) != 0: result.incl evRead
  if (mask and (1 shl ord(evWrite))) != 0: result.incl evWrite

proc readyEvents*(c: IoCompletion): IoEvents {.inline.} =
  ## The direction(s) that fired, for an `opPollAdd` completion. Empty for
  ## every other op, whose `result` is a byte count or an error instead.
  if c.op == opPollAdd: toIoEvents(c.result) else: {}

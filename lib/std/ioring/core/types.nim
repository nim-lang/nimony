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
  Deadline* = distinct int64
    ## An absolute instant on the ring's monotonic clock, in nanoseconds.
    ##
    ## Deadlines rather than timeouts, because timeouts do not compose: a
    ## relative timeout per operation makes the worst case the *sum* of them,
    ## which grows with however many operations the code happens to perform,
    ## so the work as a whole has no bound anyone can state. One absolute
    ## instant threaded through bounds the total regardless of what happens
    ## inside it.

const
  never* = Deadline(high(int64))
    ## No deadline. It exists, but it has to be written: the difference
    ## between "no deadline because I decided" and "no deadline because I did
    ## not think about it" is whether the programmer had to type the word.
    ## Nothing that can park has a default.

proc `==`*(a, b: Deadline): bool {.inline, borrow.}
proc `<`*(a, b: Deadline): bool {.inline, borrow.}
proc `<=`*(a, b: Deadline): bool {.inline, borrow.}

proc earlier*(a, b: Deadline): Deadline {.inline.} =
  ## The only combinator, and that is the point: a sub-operation can tighten
  ## its caller's budget, never widen it.
  if a < b: a else: b

when defined(posix):
  when defined(linux):
    const RingClock = ClockId(7)
      ## `CLOCK_BOOTTIME`, not `CLOCK_MONOTONIC`. A machine that suspends for an
      ## hour should find its in-flight deadlines blown, not extended — the peer
      ## is long gone either way. Never `CLOCK_REALTIME`, or an NTP step retimes
      ## everything in flight.
  else:
    const RingClock = CLOCK_MONOTONIC
      ## Darwin and the BSDs have no `CLOCK_BOOTTIME`; this is the nearest.

  proc monoNow*(): Deadline =
    ## The ring's clock. Never the wall clock — see `RingClock`.
    var ts = default(Timespec)
    discard clock_gettime(RingClock, ts)
    result = Deadline(int64(ts.tv_sec) * 1_000_000_000'i64 + int64(ts.tv_nsec))

else:
  # Windows: QueryPerformanceCounter. Monotonic, never stepped by the wall
  # clock, and — unlike `GetTickCount64` or the interrupt-time clock — finer
  # than the ~15.6 ms scheduler tick, which a deadline heap that hands
  # millisecond waits to WSAPoll/GetQueuedCompletionStatusEx needs.
  #
  # Not an exact match for the POSIX arm's `CLOCK_BOOTTIME`: whether QPC keeps
  # counting across suspend depends on the counter the HAL picked. Windows
  # offers no clock that is both high-resolution and suspend-inclusive, and the
  # resolution is what a deadline is actually measured against, so that is the
  # side to err on.
  proc queryPerformanceCounter(res: ptr int64): int32 {.
    stdcall, importc: "QueryPerformanceCounter", dynlib: "kernel32".}
  proc queryPerformanceFrequency(res: ptr int64): int32 {.
    stdcall, importc: "QueryPerformanceFrequency", dynlib: "kernel32".}

  var gQpcFreq: int64 = 0
    ## Fixed for the boot session, so it is read once. Racing threads all write
    ## the same value, which is why this needs no lock.

  proc monoNow*(): Deadline =
    ## The ring's clock. Never the wall clock — see the note above.
    var freq = gQpcFreq
    if freq == 0:
      discard queryPerformanceFrequency(addr freq)
      if freq <= 0: return Deadline(0)
      gQpcFreq = freq
    var c: int64 = 0
    discard queryPerformanceCounter(addr c)
    # Split rather than `c * 1_000_000_000 div freq`: the counter counts from
    # boot, so at the usual 10 MHz the plain form overflows int64 after a few
    # hours of uptime and every deadline comparison goes backwards.
    result = Deadline((c div freq) * 1_000_000_000'i64 +
                      ((c mod freq) * 1_000_000_000'i64) div freq)

proc after*(ns: int64): Deadline {.inline.} =
  ## A deadline `ns` nanoseconds from now. Sugar for the common case; the
  ## value is still absolute from here on.
  Deadline(int64(monoNow()) + ns)

proc afterMs*(ms: int): Deadline {.inline.} = after(int64(ms) * 1_000_000'i64)

proc millisUntil*(d, base: Deadline): int =
  ## Whole milliseconds from `base` to `d`, rounded up so a wait never returns
  ## just before the deadline it was sized for. `0` if already past, and
  ## `high(int)` for `never`.
  if d == never: return high(int)
  let ns = int64(d) - int64(base)
  if ns <= 0: return 0
  let ms = (ns + 999_999'i64) div 1_000_000'i64
  result = if ms > int64(high(int32)): int(high(int32)) else: int(ms)

proc nanosUntil*(d, base: Deadline): int64 =
  ## The same question in the unit io_uring's wait actually takes. `0` if the
  ## deadline is already past, `-1` for `never`. No rounding up: a `timespec`
  ## can name the instant, so unlike `millisUntil` this does not have to give
  ## a deadline a whole extra millisecond to be sure of reaching it.
  if d == never: return -1
  result = int64(d) - int64(base)
  if result < 0: result = 0

type
  IoEvent* = enum
    ## A readiness direction. `submitPollAdd` takes a set of these, and an
    ## `opPollAdd` completion reports the set that actually fired.
    evRead   ## readable — data is available, or a listener has a pending connection
    evWrite  ## writable — the send buffer has room

  IoEvents* = set[IoEvent]

  IoOp* = enum
    opNop, opRead, opWrite, opAccept, opPollAdd, opConnect, opOpen,
    opSocket, opSetSockOpt, opBind, opSetNonBlocking,
    opRecvFrom, opSendTo, opTimeout

  SeqNum* = uint32

  IoCompletion* = object
    id*: SeqNum
    op*: IoOp
    fd*: FileHandle
    result*: int
      ## Op-dependent: a byte count for `opRead`/`opWrite`, the accepted fd for
      ## `opAccept`, the opened fd for `opOpen`, -1 on failure — and for
      ## `opPollAdd` the fired directions encoded as a bit mask, which
      ## `readyEvents` decodes into `IoEvents`.

  OpBuf* = object
    ## A caller-owned buffer and its byte count: the entire payload of the
    ## single-buffer transfers `opRead` and `opWrite`. Spelled once, as its own
    ## type — each branch names its own field of it — so neither op ever pays
    ## for, or can see, the other's fields or any other op's.
    ##
    ## The buffer is borrowed, never copied: the backend reads or writes it at
    ## issue and completion time, long after the submit returned, so it must
    ## outlive the op — the caller's frame is parked for the duration (the same
    ## contract as `submitRead`'s docstring). An owning `seq` inside `OpContext`
    ## would be shared across lanes for no reason.
    buf*: nil pointer
      ## The transfer buffer.
    len*: int
      ## Its byte count.

  IoAddr* = object
    ## A socket address and its length: the entire payload of the
    ## caller-supplied addressing ops `opConnect` and `opBind`, which carry
    ## exactly these two fields and nothing else. The address is copied into
    ## the op by `submitConnect`/`submitBind`, so — unlike a transfer buffer —
    ## it need not outlive the call: the backend reads it at issue time and the
    ## kernel never keeps it.
    sockAddr*: Sockaddr_storage
    sockAddrLen*: SockLen

  AcceptArgs* = object
    ## `opAccept` only. The kernel fills `sockAddr` in as the connecting peer
    ## while the accept runs; `peer`, when given, is where the completed
    ## address is copied. Accepting without a peer is legal — the address is
    ## simply dropped for a caller that does not care who connected.
    sockAddr*: Sockaddr_storage
    sockAddrLen*: SockLen
    peer*: nil ptr Sockaddr_storage
      ## `nil` for a caller that does not care who connected. Written only when
      ## the accept succeeds, and must outlive the op: the completion writes
      ## through it from whichever lane ran the accept.
      ##
      ## An out-parameter rather than a field on `IoCompletion`, for the same
      ## reason `res` is one: a completion is 24 bytes and there are `CqSize`
      ## of them, so putting a 128-byte `sockaddr_storage` in the struct would
      ## make every `read` pay for a field only `accept` fills. The kernel
      ## already wrote the address into the op — this is only the hand-off.
      ##
      ## No matching length out-parameter: `ss_family` says how to read the
      ## bytes, and a length that only ever restates the family is a second
      ## name for one fact and hence one of them that can be wrong.

  RecvFromArgs* = object
    ## `opRecvFrom` only: a datagram's buffer, the source address the kernel
    ## fills in as part of the receive, and where to copy it — the one op that
    ## both transfers bytes and answers "where did this come from".
    buf*: nil pointer
      ## The datagram's buffer (the `OpBuf` contract).
    len*: int
    sockAddr*: Sockaddr_storage
      ## Filled in by the kernel as the datagram's source.
    sockAddrLen*: SockLen
    peer*: nil ptr Sockaddr_storage
      ## Where to copy `sockAddr` when the receive completes, or `nil` for a
      ## caller that does not care where the datagram came from. Same
      ## out-parameter rationale as `AcceptArgs.peer`.

  SendToArgs* = object
    ## `opSendTo` only: a datagram's buffer and the caller-supplied target
    ## address, which is copied into the op so it does not need to outlive the
    ## submit.
    buf*: nil pointer
    len*: int
    sockAddr*: Sockaddr_storage
    sockAddrLen*: SockLen

  OpenArgs* = object
    ## `opOpen` only: the path to open and the arguments the backend's open
    ## needs. The path is read by the backend at issue time and is never given
    ## a copy, so it must outlive the op — the caller's frame is parked for
    ## the duration (the same contract as every other borrowed buffer).
    buf*: nil pointer
      ## The path to open.
    len*: int
      ## The path's byte count.
    openFlags*: int32
      ## What the backend's open needs as arguments. The caller translates
      ## `FileMode` into the platform's bits before submitting: on POSIX the
      ## O_* flags, on Windows the Win32 `desiredAccess` (a truncating table,
      ## so the value is an int32 bit-pattern the backend widens back via
      ## `cast[uint32]`).
    openMode*: int32
      ## The mode argument. POSIX reads it only when the flags create the file,
      ## but the value is carried anyway so the backend has nothing to decide.
      ## On Windows it carries the Win32 `creationDisposition` the same way.

  OpContext* = object
    ## One in-flight operation, as the backends hand it to the kernel.
    ##
    ## A sum type, and a strict one: the fields above the `case` are common to
    ## *every* op — the fd/identity, the resume, and the bound on how long any
    ## of them may wait — and everything else lives in the branch of the op
    ## that actually uses it. Each kind carries exactly its own payload and no
    ## other kind's: a read holds a buffer, a connect holds an address, and
    ## each is invisible to the other because in this layout the other's fields
    ## do not exist. Where two kinds genuinely need the same bytes they share a
    ## payload *type* (`opRead`/`opWrite` each hold an `OpBuf`;
    ## `opConnect`/`opBind` each hold an `IoAddr`) — a matter of definition,
    ## not a union hiding fields some branch never uses.
    fd*: FileHandle
      ## The fd the op works on, or `-1` for the fd-less kinds (nop, timeout,
      ## open, socket). Also the slot arena's key: every op is linked into the
      ## per-fd list keyed by this, the fd-less ones sharing the `-1` bucket.
    seqnum*: SeqNum
    cont*: Continuation
    res*: int
    deadline*: Deadline
      ## When this op stops being worth waiting for. `never` is legal and has
      ## to be spelled. Every op carries one, which is what makes "nothing
      ## parks forever" a property of the ring rather than a habit of its
      ## callers.
    case kind*: IoOp
    of opNop, opTimeout, opSetNonBlocking:
      ## The payload-less ops: a nop completes `0` on arrival, a timer is
      ## nothing but its deadline (the heap IS the wait), and a non-blocking
      ## fcntl answers on the polling thread.
      discard
    of opRead:
      read*: OpBuf
        ## The buffer to fill. An opRead never carries an address, a path or
        ## any other op's fields.
    of opWrite:
      write*: OpBuf
        ## The buffer to drain.
    of opOpen:
      open*: OpenArgs
        ## The path (as the "buffer") and the open arguments; see `OpenArgs`.
    of opAccept:
      accept*: AcceptArgs
        ## Kernel-filled connecting peer and the caller's copy target; see
        ## `AcceptArgs`.
    of opRecvFrom:
      recvfrom*: RecvFromArgs
        ## The datagram buffer and the kernel-filled source; see
        ## `RecvFromArgs`.
    of opSendTo:
      sendto*: SendToArgs
        ## The datagram buffer and the target address; see `SendToArgs`.
    of opConnect:
      connect*: IoAddr
        ## The target address; see `IoAddr`.
    of opBind:
      bindTo*: IoAddr
        ## The address to bind to; see `IoAddr`. Named `bindTo` — `bind` is a
        ## Nim keyword, so it cannot be the field's identifier.
    of opSocket:
      sockDomain*: int32
        ## opSocket only: the `domain` argument passed to socket(2). The caller
        ## picks the platform's AF_* constant before submitting.
      sockType*: int32
        ## opSocket only: the `type` argument (platform SOCK_* constant).
      sockProtocol*: int32
        ## opSocket only: the `protocol` argument (platform IPPROTO_* constant).
    of opSetSockOpt:
      optLevel*: int32
        ## opSetSockOpt only: the `level` argument (platform SOL_* constant).
      optName*: int32
        ## opSetSockOpt only: the `optname` argument. What the option is, and
        ## whether it is a flag or a value, is entirely the caller's business —
        ## the backend just forwards `optVal`/`optLen` to setsockopt(2).
      optVal*: nil pointer
        ## opSetSockOpt only: the option value's bytes. The value does not have
        ## to outlive the submit: these ops are completed synchronously by the
        ## polling thread before the flag is set, so the submitter's stack is
        ## still live when the platform call happens.
      optLen*: SockLen
        ## opSetSockOpt only: how many bytes `optVal` has.
    of opPollAdd:
      pollMask*: IoEvents
        ## opPollAdd only: the direction(s) the caller actually waits for.
        ## Without it a readiness probe has to arm both directions, and a caller
        ## waiting to READ is woken every time the fd is merely WRITABLE — which,
        ## for a socket, is almost always. Since the op is oneshot, that caller's
        ## re-arm turns into a hot spin.

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

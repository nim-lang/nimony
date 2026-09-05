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
    opNop, opRead, opWrite, opAccept, opPollAdd, opConnect, opTimeout

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
    deadline*: Deadline
      ## When this op stops being worth waiting for. `never` is legal and has
      ## to be spelled. Every op carries one, which is what makes "nothing
      ## parks forever" a property of the ring rather than a habit of its
      ## callers.
    pollMask*: IoEvents
      ## opPollAdd only: the direction(s) the caller actually waits for.
      ## Without it a readiness probe has to arm both directions, and a caller
      ## waiting to READ is woken every time the fd is merely WRITABLE — which,
      ## for a socket, is almost always. Since the op is oneshot, that caller's
      ## re-arm turns into a hot spin.
    sockAddr*: Sockaddr_storage
      ## `opAccept` has the kernel fill this in; `opConnect` supplies it. The
      ## two never coexist on one op, so they share the storage rather than
      ## paying for both.
    sockAddrLen*: SockLen

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

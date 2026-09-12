# Linux io_uring backend.
# Uses the existing Queue from lib/std/posix/io_uring.nim.
# Does not use PollBackend since io_uring uses its own submission/completion
# queue model.
#
# Submissions are deferred to a shared queue so that the calling thread
# (e.g. main) never owns an SQE — every SQE is filled and flushed on the
# thread that calls poll(), which is always a worker thread (or whomever
# calls waitCompletions()). That avoids the "submitted on main, never
# polled" hang.
#
# ------------------------------------------------- the one syscall policy ---
#
# An io_uring backend must not make its own syscalls: the ring IS the kernel
# for this process's I/O, and every operation here is expressed as an SQE. To
# that end, before adding a ring op a future agent MUST first check whether
# io_uring already has the operation (`OP_*` in lib/std/posix/io_uring.nim,
# plus `SOCKET_URING_OP_*` commands of `IORING_OP_URING_CMD`), verified
# against the kernel's own io_uring sources (`io_uring/net.c`,
# `io_uring/uring_cmd.c`, `include/uapi/linux/io_uring.h`) and its kernel
# floors (socket op: 5.19, socket uring-cmds: 6.7, see each builder's doc).
# Only when a requested operation genuinely has NO io_uring form — as
# bind(2) has on every released kernel, `IORING_OP_BIND` being unreleased —
# and the syscall cannot be avoided does this backend fall back to one, and
# only with the user's explicit allowance. Right now the ledger is:
#
#   socket(2)        -> IORING_OP_SOCKET, SOCK_NONBLOCK in the type (5.19+)
#   setsockopt(2)    -> IORING_OP_URING_CMD + SOCKET_URING_OP_SETSOCKOPT (6.7+)
#   bind(2)          -> bind(2) syscall, the ONE allowed syscall (the user
#                       explicitly allowed it; there is no ring form on
#                       released kernels)
#   fcntl O_NONBLOCK -> no syscall: the ring fds are non-blocking by construction
#
# Every kernel >= the io_uring baseline satisfies the floors here (5.19/6.7);
# older kernels instead get per-op -EOPNOTSUPP, never a prohibited syscall.

import std/[assertions, atomics, posix/posix, tables, ticketlocks, threadpool]
import std/syncio   # quit
import ../../posix/io_uring
import ../core/types
import ../core/slots
import ../core/backend
from ./poll import completeBind
from ./epoll import initEpollBackendRelays

const
  DrainBatch = 128  ## Max deferred entries drained per poll() call.
  CancelUserData = 0xffff_ffff_ffff_ffff'u64
    ## `user_data` for the cancellation SQEs this backend submits on its own
    ## behalf. Its low word is not a slot index any arena can hold, so the
    ## completion loop drops the CQE on the bounds check below and no slot is
    ## touched by it.
  AtFdCwd = cint(-100)  ## resolve an AT_* path against the cwd; `posix` only
    ## exposes its own `AT_FDCWD` under `linuxA64Raw`, so name it here.
  SockNonBlock = 0x800
    ## `SOCK_NONBLOCK` — the Linux socket-type word's flag bit. io_uring is
    ## Linux-only, so the high bit is folded into the type of every
    ## `IORING_OP_SOCKET`, which is where `io_socket_prep` reads the flags
    ## from; the ring's sockets are non-blocking from birth and
    ## `opSetNonBlocking` never touches a syscall here.

proc tagFor(idx: int; gen: uint32): uint64 {.inline.} =
  ## A CQE's `user_data`: the slot index, and the generation of the op that was
  ## in it when the SQE was filled.
  uint64(uint32(idx)) or (uint64(gen) shl 32)

var
  sqEntries: int
  localQueues: seq[Queue]

type
  MsgSlot = object
    ## Per-op msghdr storage for `IORING_OP_RECVMSG`/`IORING_OP_SENDMSG`.
    ## The kernel holds a pointer to the `msghdr` from fill time until the op
    ## completes, so the storage must not move for exactly as long as its op is
    ## in flight. `hdr`/`iov` are therefore never stored by value in anything
    ## the kernel could see move — each block lives on the heap, and only its
    ## pointer travels.
    hdr: Tmsghdr
    iov: IOVec

  MsgArena = object
    ## Pool of `MsgSlot` blocks io_uring pins for its in-flight RECVMSG/SENDMSG
    ## ops. A block is allocated the first time it is needed and then recycled
    ## through `freelist` — never freed while the ring lives: each op borrows a
    ## block for its flight and returns it on completion, so the pool's size
    ## settles at the deepest concurrent datagram load, and allocation is not a
    ## per-op cost. `allocs` maps the op's `tagFor(idx, gen)` — the same tag its
    ## SQE carries — to the pool index holding its block, so a stale CQE (an op
    ## the slot arena already completed on a blown deadline or `closeFd`) can
    ## only release *its own* block, never one a later op registered under the
    ## same slot index.
    ##
    ## Only those two ops ever touch the pool: a lane serving reads, writes,
    ## accepts or timers alone keeps an empty pool, which is the point — no
    ## storage is reserved for slots that never carry a datagram. An entry is
    ## registered the moment an SQE is filled and released when the kernel
    ## acknowledges the op with a CQE, in time or late.
    ##
    ## The pool holds pointers rather than `MsgSlot`s: a `Tmsghdr`'s non-nil
    ## pointers leave `MsgSlot` a type without a `default`, which a value seq
    ## would demand, and io_uring holds `addr msg.hdr` of an in-flight op,
    ## which a value seq's realloc would move. The seq of pointers may itself
    ## move as it grows, but the blocks it points at never do.
    msgs: seq[ptr MsgSlot]       ## pool of blocks, one allocation each ever
    freelist: seq[uint32]        ## pool indices of blocks not currently in use
    allocs: Table[uint64, uint32]  ## tag -> pool index of the op's block

proc growMsg(a: var MsgArena) =
  ## One block for the pool, allocated once and never deallocated while the
  ## ring lives; the freelist recycles it from there on.
  a.msgs.add cast[ptr MsgSlot](alloc0(sizeof(MsgSlot)))
  a.freelist.add uint32(a.msgs.len - 1)

proc registerMsg(a: var MsgArena; idx: int; gen: uint32): ptr MsgSlot =
  ## The `MsgSlot` block for the op in slot `idx` at generation `gen`, taking
  ## a free block from the pool (allocating the pool's first as needed). Each
  ## op gets its own block for its whole flight, so the address the SQE hands
  ## to the kernel stays valid until the op's CQE arrives.
  if a.freelist.len == 0: a.growMsg()
  let bi = a.freelist.pop()
  a.allocs[tagFor(idx, gen)] = bi
  result = a.msgs[bi]

proc releaseMsg(a: var MsgArena; idx: int; gen: uint32) =
  ## Return the block for op `idx`/`gen` to the freelist. Called once the
  ## kernel is done with the op's storage — whether the op finished in time or
  ## its CQE only turned up late after a cancel, so this is also the moment a
  ## cancelled op's block stops being the kernel's. No-ops for CQEs of ops
  ## that never registered (reads, ...).
  let key = tagFor(idx, gen)
  let bi = a.allocs.getOrDefault(key, high(uint32))
  if bi != high(uint32):
    a.freelist.add bi
    a.allocs.del(key)

proc blockMsg(a: var MsgArena; idx: int; gen: uint32): ptr MsgSlot =
  ## The registered block for op `idx`/`gen`, for reads that must happen
  ## before `releaseMsg`. A live completion of a RECVMSG always has its block,
  ## because fill registered it; a missing block is a bookkeeping bug.
  let key = tagFor(idx, gen)
  let bi = a.allocs.getOrDefault(key, high(uint32))
  assert bi != high(uint32), "ioring/iouring: msghdr block missing for live op"
  result = a.msgs[bi]

proc releaseAllMsg(a: var MsgArena) =
  ## Teardown: free the pool's blocks. Nothing is in flight at ring close.
  for m in a.msgs:
    if m != nil: dealloc(m)
  a.msgs = @[]
  a.freelist = @[]
  a.allocs = initTable[uint64, uint32]()

var gMsgs: seq[MsgArena]   ## per lane, registered by slot tag

proc tryInitLocalQueues(): bool =
  localQueues = @[]
  gMsgs = @[]
  try:
    for i in 0..<ioLanes():
      localQueues.add newQueue(sqEntries)
      gMsgs.add MsgArena(allocs: initTable[uint64, uint32]())
  except ErrorCode:
    return false   # the caller falls back to the epoll backend
  return true

proc fillSqe(sqe: ptr Sqe; lane: int; idx: int) {.inline.} =
  let slot = addr gSlots[lane].slots[idx]
  let op = addr slot.op
  case op.kind
  of opRead:
    if op.buf != nil:
      # `off = -1` means stream semantics: read at the fd's current position
      # and advance it. The default (0) would make this a pread at offset 0
      # forever — invisible on a socket (the kernel ignores `off` there), but
      # a regular-file read that always returns the file's first bytes.
      discard sqe.read(op.fd, cast[pointer](op.buf), op.len, -1)
  of opWrite:
    if op.buf != nil:
      discard sqe.write(op.fd, cast[pointer](op.buf), op.len, -1)
  of opAccept:
    discard sqe.accept(SocketHandle(op.fd), cast[ptr SockAddr](addr op.sockAddr), addr op.sockAddrLen, 0)
  of opPollAdd:
    # Single-shot readiness probe on the direction(s) the caller asked for;
    # completes with the fired poll mask, then the slot is freed so the caller
    # re-arms with a new submitPollAdd (matching the epoll/kqueue oneshot
    # behaviour). Watching both regardless would spin a read-waiter on a
    # writable socket — see submitPollAdd's docstring.
    # The kernel speaks poll(2) events, the ring speaks `IoEvents`; the two
    # never share a representation, so translate in both directions here and
    # in the completion loop below.
    var pollEvents: PollEvents = {}
    if evRead in op.pollMask: pollEvents.incl POLL_IN
    if evWrite in op.pollMask: pollEvents.incl POLL_OUT
    discard sqe.poll_add(op.fd, pollEvents)
  of opConnect:
    discard sqe.connect(SocketHandle(op.fd),
                        cast[ptr SockAddr](addr op.sockAddr), op.sockAddrLen)
  of opOpen:
    # `IORING_OP_OPENAT` is the ring's own open: nothing waits on it and this
    # backend never makes the syscall itself — the only syscalls an io_uring
    # backend may make are the io_uring ones. The path is the caller-owned
    # buffer carried in the op context; it stays alive until the op completes,
    # the same contract `submitRead`'s buffer has.
    discard sqe.openat(AtFdCwd, cast[pointer](op.buf), cint(op.openFlags), op.openMode)
  of opRecvFrom:
    # IORING_OP_RECVMSG has no recvfrom form: the source address travels in the
    # msghdr (`msg_name`), which the kernel reads on submission and writes back
    # into at completion, so the msghdr lives in the lane's `MsgArena` — never
    # on this stack. `msg_name` points at the op's own `sockAddr`, already
    # arena-stable, so `complete` hands the storage to `peer` the way accept's
    # does, and `msg_iov` points at the caller's buffer, which is alive until
    # the op completes (the same contract as `submitRead`).
    let m = gMsgs[lane].registerMsg(idx, slot.gen)
    zeroMem(addr m.hdr, sizeof(m.hdr))
    m.hdr.msg_name = addr op.sockAddr
    m.hdr.msg_namelen = op.sockAddrLen
    m.hdr.msg_iov = addr m.iov
    m.hdr.msg_iovlen = csize_t(1)
    m.iov.iov_base = cast[pointer](op.buf)
    m.iov.iov_len = csize_t(op.len)
    discard sqe.recvmsg(SocketHandle(op.fd), addr m.hdr)
  of opSendTo:
    let m = gMsgs[lane].registerMsg(idx, slot.gen)
    zeroMem(addr m.hdr, sizeof(m.hdr))
    m.hdr.msg_name = addr op.sockAddr
    m.hdr.msg_namelen = op.sockAddrLen
    m.hdr.msg_iov = addr m.iov
    m.hdr.msg_iovlen = csize_t(1)
    m.iov.iov_base = cast[pointer](op.buf)
    m.iov.iov_len = csize_t(op.len)
    discard sqe.sendmsg(SocketHandle(op.fd), addr m.hdr)
  of opNop, opTimeout:
    # A timer needs no SQE. The lane's deadline heap already knows when it is
    # due and bounds the `submit(waitNr)` below, so letting the kernel hold a
    # second copy of the same deadline would only be a second thing to cancel.
    discard sqe.nop()
  of opSocket:
    # IORING_OP_SOCKET: the ring creates the fd and its CQE carries it back —
    # no socket(2) syscall anywhere. Non-blocking is folded into the type's
    # high bits (SOCK_NONBLOCK), which is this backend's answer to
    # `opSetNonBlocking`: the ring's sockets are non-blocking from birth, so
    # that op only completes 0 below. The flags the kernel accepts are exactly
    # SOCK_NONBLOCK/SOCK_CLOEXEC (anything else is EINVAL); SOCK_CLOEXEC is
    # deliberately not set, matching the plain socket(2) the other backends
    # make.
    discard sqe.socket(op.sockDomain, op.sockType, op.sockProtocol, SockNonBlock)
  of opSetSockOpt:
    # setsockopt(2) as IORING_OP_URING_CMD + SOCKET_URING_OP_SETSOCKOPT
    # (kernel 6.7+, the only released across the board): level/optname/optlen/
    # optval travel in the SQE's socket-cmd slots and the ring performs the
    # option update. `optval` must stay valid until the op completes — the
    # same contract as a submitRead buffer; the passive caller's frame is
    # parked exactly that long.
    discard sqe.cmdSockSetsockopt(op.fd, op.optLevel, op.optName,
                                  cast[pointer](op.optVal), int32(op.optLen))
  of opBind, opSetNonBlocking:
    # Unreachable: the two remaining config ops are completed in `iouringPoll`
    # before an SQE is taken (bind has no ring form; non-blocking is already a
    # property of every ring-created socket, see there).
    discard

proc iouringPoll(timeoutMs: int): bool {.nimcall.} =
  # Drain the shared deferred queue: for every pending slot, fill a fresh
  # SQE in THIS thread's io_uring instance. Only worker threads (and
  # callers of waitCompletions) poll, so all SQEs are always submitted
  # from within the poll loop that also reads their CQEs.
  #
  # Drain is bounded (DrainBatch) so a flood of submissions cannot keep a
  # worker inside poll() forever — the outer worker loop also runs task
  # draining, and remaining deferred entries are picked up next iteration.
  let lane = ioLane()
  var buf {.noinit.}: array[DrainBatch, OpContext]
  var n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
  if n > 0:
    for i in 0..<n:
      if buf[i].kind == opTimeout:
        # No SQE, but it still needs a slot so the heap can complete it.
        let idx = gSlots[lane].allocSlot(buf[i])
        armDeadline(lane, idx)
        continue
      if buf[i].kind in {opBind, opSetNonBlocking}:
        # The two config ops that have NO ring form, completed here before any
        # SQE is taken. bind(2) is this backend's ONE allowed syscall (there is
        # no bind operation on any released kernel — IORING_OP_BIND is
        # unreleased — and the user explicitly allowed it: it answers in
        # microseconds and there is nothing for the ring to wait on), performed
        # on the polling thread exactly as the readiness backends perform it.
        # opSetNonBlocking makes no syscall at all: every ring-created socket
        # (IORING_OP_SOCKET with SOCK_NONBLOCK, its accepted/connected
        # siblings) is non-blocking by construction, so the op succeeds the
        # moment it reaches a poll. socket(2) and setsockopt(2), by contrast,
        # both have ring forms and go through SQEs below.
        let idx = gSlots[lane].allocSlot(buf[i])
        armDeadline(lane, idx)
        case buf[i].kind
        of opBind:
          completeBind(idx, buf[i].fd, addr buf[i].sockAddr, buf[i].sockAddrLen)
        of opSetNonBlocking:
          complete(idx, 0)
        else:
          discard
        continue
      var sqe: nil ptr Sqe
      try:
        sqe = localQueues[lane].getSqe()
      except ErrorCode:
        # Ops buf[i..<n] were dequeued but never got an SQE/slot; put them
        # back so the next poll picks them up instead of losing them forever.
        for k in i..<n:
          discard gOpQueues[lane].tryEnqueue(buf[k])
        break
      if sqe == nil:
        for k in i..<n:
          discard gOpQueues[lane].tryEnqueue(buf[k])
        break
      let idx = gSlots[lane].allocSlot(buf[i])
      armDeadline(lane, idx)
      let slot = addr gSlots[lane].slots[idx]
      sqe.userData = cast[pointer](tagFor(idx, slot.gen))
      # Fill from the ARENA copy, never from `buf`: an accept SQE stores
      # `addr op.sockAddr`/`addr op.sockAddrLen` and the kernel writes through
      # those at completion time, long after this stack frame is gone.
      fillSqe(sqe, lane, idx)
  # Sleep in the kernel until something is due — an I/O completion, or the
  # earliest deadline this lane is waiting on, whichever comes first. That is
  # ONE timeout for the whole lane, taken off the top of the deadline heap,
  # rather than one timeout per op: a `link_timeout` on every SQE would buy the
  # same wakeup for a second submission and a second completion each, and a
  # server holding an idle deadline per connection would spend most of its ring
  # on timers. The readiness backends bound their wait exactly this way
  # (`epoll_wait(waitMs)`), so all three answer "how long may I sleep" from the
  # same heap.
  #
  # The wait is still bounded from above by what the caller asked for, because
  # nothing wakes a worker when a TASK is enqueued — `threadpool.submit` only
  # enqueues — so a lane may not sleep past its next look at the run queue.
  # Give the pool a wakeup and this bound can go, and then the ring sleeps
  # until there is genuinely something to do.
  #
  # Before this the ring never entered the kernel to wait at all: the bound was
  # computed and discarded, and an idle worker `nanosleep`t a millisecond with
  # its eyes shut, so a completion arriving 10us in was noticed 990us late.
  let blocking = timeoutMs != 0 and localQueues[lane].cqReady == 0 and
                 localQueues[lane].hasExtArg
  if blocking:
    let ns = waitNanos(lane, timeoutMs)
    var ts = Timespec(tv_sec: Time(ns div 1_000_000_000'i64),
                      tv_nsec: clong(ns mod 1_000_000_000'i64))
    var tsp: nil ptr Timespec = nil
    if ns >= 0: tsp = addr ts
    try:
      discard localQueues[lane].submitAndWait(1, tsp)
    except ErrorCode as e:
      quit "fatal: bug: submit and wait cannot fail: " & $e
  elif n > 0:
    try:
      discard localQueues[lane].submit()
    except ErrorCode as e:
      quit "fatal: bug: submit cannot fail: " & $e
  if localQueues[lane].cqReady > 0:
    var cqes {.noinit.}: array[DrainBatch, Cqe]
    try:
      n = localQueues[lane].copyCqes(cqes)
    except ErrorCode as e:
      quit "fatal: bug: copyCqes cannot fail: " & $e
    if n > 0:
      for i in 0..<n:
        # A slot is recycled the instant its op completes, and an op can
        # complete *here* while the kernel is still working on it: a blown
        # deadline expires it from the lane's heap, and `closeFd` cancels it.
        # The CQE that turns up afterwards then names a slot that holds an
        # unrelated op. Applying it completed that op with the wrong result and
        # — worse — freed a slot that was already free, so its index went onto
        # the freelist twice and two later ops shared one slot. One of them
        # could never complete, which is how the ring came to hang instead of
        # merely misreporting. The generation in `user_data` says which op the
        # kernel is talking about; anything else is already accounted for.
        let idx = int(cqes[i].userData and 0xffff_ffff'u64)
        let gen = uint32(cqes[i].userData shr 32)
        var slot: nil ptr Slot = nil
        if idx < gSlots[lane].slots.len:
          slot = addr gSlots[lane].slots[idx]
        if slot == nil or not slot.inUse or slot.gen != gen:
          # The op is already accounted for (a deadline/`closeFd` completed it
          # here), so its kind is no longer readable from the slot — but only a
          # datagram op ever registered a block, so this release is inherently
          # selective: the kernel just relinquished the msghdr it still held.
          gMsgs[lane].releaseMsg(idx, gen)
          continue
        let op = addr slot.op
        # Only RECVMSG/SENDMSG ops ever registered a block, so return it only
        # for those; a live read, accept or poll CQE never touches the arena.
        if op.kind == opRecvFrom or op.kind == opSendTo:
          if op.kind == opRecvFrom and int(cqes[i].res) > 0:
            # RECVMSG wrote the actual source-address length back into our
            # msghdr; narrow `sockAddrLen` to it before the block goes,
            # exactly as the readiness backends do with their recvfrom's in-out
            # length, so the storage `complete` hands to `peer` is described by
            # the real length.
            op.sockAddrLen = gMsgs[lane].blockMsg(idx, gen).hdr.msg_namelen
          # The kernel is done with the op's storage, so the block goes back
          # to the pool it was borrowed from.
          gMsgs[lane].releaseMsg(idx, gen)
        # For OP_POLL_ADD the kernel reports the fired mask in poll(2) form;
        # translate it to the same internal `IoEvents` the epoll/kqueue
        # backends report, so the completion's `readyEvents` are consistent no
        # matter which backend is in use.
        var res = int(cqes[i].res)
        if op.kind == opPollAdd:
          var fired = toPollEvents(uint32(res))
          var ev: IoEvents = {}
          if POLL_IN in fired: ev.incl evRead
          if POLL_OUT in fired: ev.incl evWrite
          res = toEventMask(ev)
        complete(idx, res)
      expireDeadlines(lane)
      return true
  expireDeadlines(lane)
  return false

proc iouringCancelInFlight(slotIdx: int; gen: uint32) {.nimcall.} =
  ## The lane's deadline heap is about to complete this op locally, but the
  ## kernel does not know that and still owns the op's buffer. Match the op by
  ## the `user_data` its SQE carries and ask for it back. Failing to get an SQE
  ## is not fatal: the CQE that eventually arrives is stale and dropped, and
  ## the only cost is that the kernel held the buffer longer than we wanted.
  let lane = ioLane()
  if lane >= localQueues.len: return
  var sqe: nil ptr Sqe
  try:
    sqe = localQueues[lane].getSqe()
  except ErrorCode:
    return
  if sqe == nil: return
  discard sqe.cancel(tagFor(slotIdx, gen))
  sqe.userData = cast[pointer](CancelUserData)
  # Submitted here rather than left for the next `submit` in the poll loop:
  # this runs from `expireDeadlines`, which the loop reaches *after* its own
  # submit, and a deadline blowing is the slow path anyway.
  try:
    discard localQueues[lane].submit()
  except ErrorCode:
    discard

proc iouringForgetFd(fd: cint) {.nimcall.} =
  ## `closeFd` calls this before close(2). The readiness backends drop a
  ## registration here; io_uring has none to drop, but it does have ops the
  ## kernel is still working on — and `closeFd` is about to free their slots
  ## locally. Until the kernel acknowledges, it still owns each op's buffer and
  ## may write through it, so ask it to give them back now rather than at some
  ## unpredictable later point. `cancelFd` is the one-SQE form of exactly that
  ## question. Whatever CQEs still arrive for those ops are stale by then, and
  ## the generation check in `iouringPoll` drops them.
  let lane = ioLane()
  if lane >= localQueues.len: return
  if not gSlots[lane].hasPendingForFd(fd): return
  var sqe: nil ptr Sqe
  try:
    sqe = localQueues[lane].getSqe()
  except ErrorCode:
    return
  if sqe == nil: return
  discard sqe.cancelFd(FileHandle(fd))
  sqe.userData = cast[pointer](CancelUserData)
  try:
    discard localQueues[lane].submit()
  except ErrorCode:
    discard

proc iouringClose() {.nimcall.} =
  # By index, and `var`: a `Queue` owns an fd and three mappings, so tearing
  # down a copy would leave the original for the seq's own destructor to tear
  # down a second time — including a second `close(2)` on a number the OS may
  # have handed to somebody else by then.
  for i in 0..<localQueues.len:
    if localQueues[i].params != nil:
      teardown(localQueues[i])
  # Free the blocks no CQE is coming for. In-flight ops are cancelled before
  # ring teardown, so every entry here is one the kernel already gave back.
  for i in 0..<gMsgs.len:
    gMsgs[i].releaseAllMsg()
  gMsgs = @[]

proc initIoUringBackendRelays*(sqE = 256): BackendRelays =
  sqEntries = sqE
  if not tryInitLocalQueues():
    return initEpollBackendRelays()
  gCancelInFlight = iouringCancelInFlight
  result = BackendRelays(
    poll: iouringPoll,
    waits: localQueues[0].hasExtArg,
    close: iouringClose,
    forgetFd: iouringForgetFd,
  )

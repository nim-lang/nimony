# illumos event ports: one-shot FD readiness plus SIGEV_PORT AIO completion.
# Positioned regular-file operations use private staging buffers and duplicated
# descriptors. A deadline may therefore release the CALLER'S buffer immediately
# while libc finishes/cancels against storage owned solely by this backend.
# The aiocb/notification/staging allocation survives until its port event, even
# when aio_cancel says ALLDONE or CANCELED (both still deliver notification).
# Sequential transfers are for nonblocking streams; files use submit*At.

import std/[threadpool, tables, assertions, syncio]
import ../../posix/posix
import ../../posix/event_port
import ../core/[types, slots, backend]
import ./poll

const
  DrainBatch = 128
  MaxAioBytes = 64 * 1024 * 1024 # per lane, including logically cancelled I/O
  F_DUPFD_CLOEXEC = cint(37)

type
  AioRequest = object
    cb: AioCb
    notify: PortNotify
    slot: int
    gen: uint32
    suppressed: bool
    next: nil ptr AioRequest
  PortLane = object
    fd: cint
    serial: uint
    registrations: Table[cint, uint]
    head: nil ptr AioRequest
    bytes: int
    requests: int

var lanes: seq[PortLane]

proc eventPortWake*(lane: int) =
  # EAGAIN means the port is full and already has something to wake it.
  discard port_send(lanes[lane].fd, 0, nil)

proc portReArm(fd: cint; events: IoEvents; alreadyRegistered: bool): bool {.nimcall.} =
  let lane = ioLane()
  if events == {}:
    discard port_dissociate(lanes[lane].fd, PORT_SOURCE_FD, uint(fd))
    lanes[lane].registrations.del(fd)
    return true
  var mask = cint(0)
  if evRead in events: mask = mask or POLLIN
  if evWrite in events: mask = mask or POLLOUT
  # Cookies distinguish stale queued events from a newly reused fd number.
  inc lanes[lane].serial
  if lanes[lane].serial == 0: inc lanes[lane].serial
  let cookie = lanes[lane].serial
  result = port_associate(lanes[lane].fd, PORT_SOURCE_FD, uint(fd), mask,
                          cast[pointer](cookie)) == 0
  if result: lanes[lane].registrations[fd] = cookie

proc cancelAio(slot: int; gen: uint32) {.nimcall.} =
  var req = lanes[ioLane()].head
  while req != nil:
    if req.slot == slot and req.gen == gen:
      req.suppressed = true
      discard aio_cancel(req.cb.aio_fildes, addr req.cb)
      break
    req = req.next

proc forgetFd(fd: cint) {.nimcall.} =
  let lane = ioLane()
  discard port_dissociate(lanes[lane].fd, PORT_SOURCE_FD, uint(fd))
  lanes[lane].registrations.del(fd)
  for slot in gSlots[lane].slotsForFd(fd):
    cancelAio(slot, gSlots[lane].slots[slot].gen)

proc releaseRequest(lane: int; req: ptr AioRequest) =
  var prev: nil ptr AioRequest = nil
  var cur = lanes[lane].head
  while cur != nil:
    if cur == req:
      if prev == nil: lanes[lane].head = cur.next
      else: prev.next = cur.next
      break
    prev = cur
    cur = cur.next
  lanes[lane].bytes -= int(req.cb.aio_nbytes)
  dec lanes[lane].requests
  discard close(req.cb.aio_fildes)
  dealloc(req.cb.aio_buf)
  dealloc(req)

proc reap(lane: int; event: PortEvent; publish: bool) =
  # Do not trust a pointer from an event without finding it in the live list.
  # Objects are never freed before their sole notification is consumed.
  var req = lanes[lane].head
  while req != nil and cast[uint](addr req.cb) != event.portev_object:
    req = req.next
  if req == nil: return
  let err = aio_error(addr req.cb)
  assert err != EINPROGRESS, "event port delivered an unfinished AIO request"
  let value = aio_return(addr req.cb) # exactly once, including cancelled I/O
  let idx = req.slot
  let live = publish and not req.suppressed and
    gSlots[lane].slots[idx].inUse and gSlots[lane].slots[idx].gen == req.gen
  if live:
    let op = addr gSlots[lane].slots[idx].op
    if op.deadline != never and op.deadline <= monoNow():
      complete(idx, IoTimedOut)
    else:
      if err == 0 and value > 0 and op.kind == opRead:
        copyMem(op.buf, req.cb.aio_buf, value)
      complete(idx, if err != 0: -int(err) else: value)
  releaseRequest(lane, req)

proc submitAio(lane, idx: int) =
  let op = addr gSlots[lane].slots[idx].op
  if op.len < 0 or op.offset < 0:
    complete(idx, -int(EINVAL))
    return
  if op.len > MaxAioBytes - lanes[lane].bytes or lanes[lane].requests >= MaxOps:
    complete(idx, -int(EAGAIN))
    return
  var st = default(Stat)
  if fstat(op.fd, st) < 0:
    complete(idx, -int(errno()))
    return
  if not S_ISREG(st.st_mode):
    complete(idx, -int(ENOTSUP))
    return
  let ownedFd = fcntl(op.fd, F_DUPFD_CLOEXEC, cint(0))
  if ownedFd < 0:
    complete(idx, -int(errno()))
    return
  let req = cast[ptr AioRequest](alloc0(sizeof(AioRequest)))
  req.slot = idx
  req.gen = gSlots[lane].slots[idx].gen
  req.cb.aio_fildes = ownedFd
  req.cb.aio_nbytes = csize_t(op.len)
  req.cb.aio_offset = Off(op.offset)
  req.cb.aio_buf = alloc(max(op.len, 1))
  if op.kind == opWrite and op.len > 0:
    copyMem(req.cb.aio_buf, op.buf, op.len)
  req.notify = PortNotify(portnfy_port: lanes[lane].fd, portnfy_user: req)
  req.cb.aio_sigevent.sigev_notify = SIGEV_PORT
  req.cb.aio_sigevent.sigev_value = addr req.notify
  # Link before submission: even an immediately completed request has stable
  # ownership, and only this lane consumes the resulting port event.
  req.next = lanes[lane].head
  lanes[lane].head = req
  lanes[lane].bytes += op.len
  inc lanes[lane].requests
  let r = if op.kind == opRead: aio_read(addr req.cb) else: aio_write(addr req.cb)
  if r < 0:
    let err = errno()
    releaseRequest(lane, req) # rejected synchronously: no event is generated
    complete(idx, -int(err))

proc submitStream(lane, idx: int) =
  let op = addr gSlots[lane].slots[idx].op
  # Regular files are always poll-ready; reading them on the reactor could
  # block on disk. Require explicit positions and AIO rather than doing that.
  var st = default(Stat)
  if fstat(op.fd, st) < 0:
    complete(idx, -int(errno()))
  elif S_ISREG(st.st_mode):
    complete(idx, -int(ENOTSUP))
  else:
    let flags = fcntl(op.fd, F_GETFL)
    if flags < 0: complete(idx, -int(errno()))
    elif (flags and O_NONBLOCK) == 0: complete(idx, -int(EINVAL))
    else: submitForPoll(op.fd)

proc eventPortPoll(timeoutMs: int): bool {.nimcall.} =
  let lane = ioLane()
  var pending {.noinit.}: array[DrainBatch, OpContext]
  let n = gOpQueues[lane].tryBulkDequeue(DrainBatch, pending)
  for i in 0..<n:
    let idx = gSlots[lane].allocSlot(pending[i])
    armDeadline(lane, idx)
    if pending[i].deadline != never and pending[i].deadline <= monoNow():
      complete(idx, if pending[i].kind == opTimeout: 0 else: IoTimedOut)
      continue
    case pending[i].kind
    of opNop: complete(idx, 0)
    of opTimeout: discard
    of opRead, opWrite:
      if pending[i].positioned: submitAio(lane, idx)
      else: submitStream(lane, idx)
    of opConnect:
      if startConnect(pending[i].fd, idx): submitForPoll(pending[i].fd)
    of opAccept, opPollAdd:
      submitForPoll(pending[i].fd)
  var events = default(array[64, PortEvent])
  var count = cuint(1)
  let waitMs = waitMillis(lane, if n > 0: 0 else: timeoutMs)
  var ts = Timespec(tv_sec: Time(max(waitMs, 0) div 1000),
                    tv_nsec: clong((max(waitMs, 0) mod 1000) * 1_000_000))
  let timeout: nil ptr Timespec = if waitMs < 0: nil else: addr ts
  let r = port_getn(lanes[lane].fd, addr events[0], cuint(events.len), addr count, timeout)
  # ETIME may accompany a PARTIAL batch; those events have been consumed and
  # must still be processed. On other errors count is not an output batch.
  if r < 0 and errno() != ETIME: count = 0
  for i in 0..<int(count):
    let ev = events[i]
    if ev.portev_source == uint16(PORT_SOURCE_AIO):
      reap(lane, ev, true)
    elif ev.portev_source == uint16(PORT_SOURCE_FD):
      let fd = cint(ev.portev_object)
      let cookie = cast[uint](ev.portev_user)
      if lanes[lane].registrations.getOrDefault(fd, 0'u) != cookie: continue
      lanes[lane].registrations.del(fd) # delivery consumes the association
      var fired: IoEvents = {}
      if (ev.portev_events and (POLLIN or POLLERR or POLLHUP or POLLNVAL)) != 0:
        fired.incl evRead
      if (ev.portev_events and (POLLOUT or POLLERR or POLLHUP or POLLNVAL)) != 0:
        fired.incl evWrite
      processFd(fd, fired)
  expireDeadlines(lane)
  result = count > 0 or n > 0

proc eventPortClose() {.nimcall.} =
  # Pool workers have stopped. Drain every physical AIO completion before
  # releasing its storage/descriptor or closing its notification port.
  for lane in 0..<lanes.len:
    var req = lanes[lane].head
    while req != nil:
      req.suppressed = true
      discard aio_cancel(req.cb.aio_fildes, addr req.cb)
      req = req.next
    while lanes[lane].head != nil:
      var ev = default(PortEvent)
      if port_get(lanes[lane].fd, addr ev, nil) == 0:
        if ev.portev_source == uint16(PORT_SOURCE_AIO): reap(lane, ev, false)
      else:
        assert errno() == EINTR, "event port failed while draining AIO"
    discard close(lanes[lane].fd)
  gCancelInFlight = nil

proc initEventPortBackendRelays*(): BackendRelays =
  lanes = newSeq[PortLane](ioLanes())
  for lane in 0..<lanes.len:
    lanes[lane].fd = port_create()
    if lanes[lane].fd < 0:
      for j in 0..<lane: discard close(lanes[j].fd)
      quit "cannot create illumos event port"
    discard fcntl(lanes[lane].fd, F_SETFD, FD_CLOEXEC)
    lanes[lane].registrations = initTable[cint, uint]()
  reArmEvent = portReArm
  gCancelInFlight = cancelAio
  result = BackendRelays(poll: eventPortPoll, waits: true,
                         close: eventPortClose, forgetFd: forgetFd)

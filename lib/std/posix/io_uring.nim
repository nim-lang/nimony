import ./[posix, epoll]
import std/[oserrors, atomics, syncio]
import ../nativesocket

type
  KernelRwfT* {.importc: "__kernel_rwf_t", header: "<linux/fs.h>".} = int

type
  SqeFlag* {.size: sizeof(uint8).} = enum
    SQE_FIXED_FILE
    SQE_IO_DRAIN
    SQE_IO_LINK
    SQE_IO_HARDLINK
    SQE_ASYNC
    SQE_BUFFER_SELECT
    SQE_CQE_SKIP_SUCCESS
  SqeFlags* = set[SqeFlag]
  
  FsyncFlag* {.size: sizeof(uint32).} = enum
    FSYNC_DATASYNC
  FsyncFlags* = set[FsyncFlag]

  TimeoutFlag* {.size: sizeof(uint32).} = enum
    TIMEOUT_ABS
    TIMEOUT_UPDATE
    TIMEOUT_BOOTTIME
    TIMEOUT_REALTIME
    LINK_TIMEOUT_UPDATE
    TIMEOUT_ETIME_SUCCESS
  TimeoutFlags* = set[TimeoutFlag]

  PollFlag* {.size: sizeof(uint16).} = enum
    POLL_ADD_MULTI
    POLL_UPDATE_EVENTS
    POLL_UPDATE_USER_DATA
    POLL_ADD_LEVEL
  PollFlags* = set[PollFlag]

  AsyncCancelFlag* {.size: sizeof(uint32).} = enum
    ASYNC_CANCEL_ALL
    ASYNC_CANCEL_FD
    ASYNC_CANCEL_ANY
    ASYNC_CANCEL_FD_FIXED
  AsyncCancelFlags* = set[AsyncCancelFlag]

  IoprioFlag* {.size: sizeof(uint16).} = enum
    RECVSEND_POLL_FIRST
    RECV_MULTISHOT
    RECVSEND_FIXED_BUF
    SEND_ZC_REPORT_USAGE
  IoprioFlags* = set[IoprioFlag]

  MsgRingOp* = enum
    MSG_DATA
    MSG_SEND_FD

  MsgRingOpFlag* {.size: sizeof(uint32).} = enum
    MSG_RING_CQE_SKIP
    MSG_RING_FLAGS_PASS
  MsgRingOpFlags* = set[MsgRingOpFlag]

  InnerSqeFlags* {.union.} = object
    rwFlags*: KernelRwfT
    fsyncFlags*: FsyncFlags
    pollEvents*: PollFlags
    poll32Events*: uint32
    syncRangeFlags*: uint32
    msgFlags*: uint32
    timeoutFlags*: TimeoutFlags
    acceptFlags*: uint32
    cancelFlags*: uint32
    openFlags*: uint32
    statxFlags*: uint32
    fadviseAdvice*: uint32
    spliceFlags*: uint32
    renameFlags*: uint32
    unlinkFlags*: uint32
    hardlinkFlags*: uint32
    xattrFlags*: uint32
    msgRingFlags*: MsgRingOpFlags
    uringCmdFlags*: uint32

  InnerSqeBuf* {.union, packed.} = object
    bufIndex*: uint16
    bufGroup*: uint16

  InnerSqeSplicePadAddrLen = object
    addrLen* {.importc: "addr_len".}: uint16
    pad3* {.importc: "__pad3".}: array[1, uint16]

  InnerSqeSplice* {.union.} = object
    spliceFdIn* {.importc: "splice_fd_in".}: uint32
    fileIndex* {.importc: "file_index".}: uint32
    addrLen*: InnerSqeSplicePadAddrLen

  InnerSqeCmd* {.union.} = object
    addr3* {.importc: "addr3".}: nil pointer
    pad2* {.importc: "__pad2".}: array[1, uint64]
    cmd* {.importc: "cmd".}: uint8
  
  Op* {.size: sizeof(uint8).} = enum
    OP_NOP
    OP_READV
    OP_WRITEV
    OP_FSYNC
    OP_READ_FIXED
    OP_WRITE_FIXED
    OP_POLL_ADD
    OP_POLL_REMOVE
    OP_SYNC_FILE_RANGE
    OP_SENDMSG
    OP_RECVMSG
    OP_TIMEOUT
    OP_TIMEOUT_REMOVE
    OP_ACCEPT
    OP_ASYNC_CANCEL
    OP_LINK_TIMEOUT
    OP_CONNECT
    OP_FALLOCATE
    OP_OPENAT
    OP_CLOSE
    OP_FILES_UPDATE
    OP_STATX
    OP_READ
    OP_WRITE
    OP_FADVISE
    OP_MADVISE
    OP_SEND
    OP_RECV
    OP_OPENAT2
    OP_EPOLL_CTL
    OP_SPLICE
    OP_PROVIDE_BUFFERS
    OP_REMOVE_BUFFERS
    OP_TEE
    OP_SHUTDOWN
    OP_RENAMEAT
    OP_UNLINKAT
    OP_MKDIRAT
    OP_SYMLINKAT
    OP_LINKAT
    OP_MSG_RING
    OP_FSETXATTR
    OP_SETXATTR
    OP_FGETXATTR
    OP_GETXATTR
    OP_SOCKET
    OP_URING_CMD
    OP_SEND_ZC
    OP_SENDMSG_ZC
    OP_LAST
  
  InnerSqeOffset* {.union.} = object
    off*: Off
    addr2*: nil pointer
    cmdOp*: uint32
    pad1*: Off

  InnerSqeAddr* {.union.} = object
    `addr`*: nil pointer
    spliceOffIn*: Off

  Sqe* {.pure, bycopy.} = object
    opcode*: Op
    flags*: SqeFlags
    ioprio*: IoprioFlags
    fd*: FileHandle
    off*: InnerSqeOffset
    `addr`*: InnerSqeAddr
    len*: int32
    opFlags*: InnerSqeFlags
    userData*: nil pointer
    buf*: InnerSqeBuf
    personality*: uint16
    splice*: InnerSqeSplice
    cmd*: InnerSqeCmd

  CqeFlag* {.size: sizeof(uint32).} = enum
    CQE_F_BUFFER
    CQE_F_MORE
    CQE_F_SOCK_NONEMPTY
    CQE_F_NOTIF
  CqeFlags* = set[CqeFlag]
  
  Cqe* = object
    userData*: uint64
    res*: int32
    flags*: CqeFlags
  
  SetupFlag* {.size: sizeof(uint32).} = enum
    SETUP_IOPOLL
    SETUP_SQPOLL
    SETUP_SQ_AFF
    SETUP_CQSIZE
    SETUP_CLAMP
    SETUP_ATTACH_WQ
    SETUP_R_DISABLED
    SETUP_SUBMIT_ALL
    SETUP_COOP_TASKRUN
    SETUP_TASKRUN
    SETUP_SQE128
    SETUP_CQE32
    SETUP_SINGLE_ISSUER
    SETUP_DEFER_TASKRUN
  SetupFlags* = set[SetupFlag]

  Feature* {.size: sizeof(uint32).} = enum
    FEAT_SINGLE_MMAP
    FEAT_NODROP
    FEAT_SUBMIT_STABLE
    FEAT_RW_CUR_POS
    FEAT_CUR_PERSONALITY
    FEAT_FAST_POLL
    FEAT_POLL_32BITS
    FEAT_SQPOLL_NONFIXED
    FEAT_EXT_ARG
    FEAT_NATIVE_WORKERS
    FEAT_RSRC_TAGS
    FEAT_CQE_SKIP
    FEAT_LINKED_FILE
    FEAT_REG_REG_RING
  Features* = set[Feature]

  SqringOffsets* = object
    head*: uint32
    tail*: uint32
    ringMask*: uint32
    ringEntries*: uint32
    flags*: uint32
    dropped*: uint32
    array*: uint32
    resv1*: uint32
    resv2*: uint64
  
  SqringFlag* {.size: sizeof(uint32).} = enum
    SQ_NEED_WAKEUP
    SQ_CQ_OVERFLOW
    SQ_TASKRUN
  SqringFlags* = set[SqringFlag]

  CqringOffsets* = object
    head*: uint32
    tail*: uint32
    ringMask*: uint32
    ringEntries*: uint32
    overflow*: uint32
    cqes*: uint32
    flags*: uint32
    resv1*: uint32
    resv2*: uint64
  
  CqringFlag* {.size: sizeof(uint32).} = enum
    CQ_EVENTFD_DISABLED
  CqringFlags* = set[CqringFlag]

  Params* = object
    sqEntries*: uint32
    cqEntries*: uint32
    flags*: SetupFlags
    sqThreadCpu*: uint32
    sqThreadIdle*: uint32
    features*: Features
    wqFd*: uint32
    resv*: array[3, uint32]
    sqOff*: SqringOffsets
    cqOff*: CqringOffsets
  
  EnterFlag* {.size: sizeof(cint).} = enum
    ENTER_GETEVENTS
    ENTER_SQ_WAKEUP
    ENTER_SQ_WAIT
    ENTER_EXT_ARG
    ENTER_REGISTERED_RING
  EnterFlags* = set[EnterFlag]

  RegisterOp* {.size: sizeof(cint).} = enum
    REGISTER_BUFFERS
    UNREGISTER_BUFFERS
    REGISTER_FILES
    UNREGISTER_FILES
    REGISTER_EVENTFD
    UNREGISTER_EVENTFD
    REGISTER_FILES_UPDATE
    REGISTER_EVENTFD_ASYNC
    REGISTER_PROBE
    REGISTER_PERSONALITY
    UNREGISTER_PERSONALITY
    REGISTER_RESTRICTIONS
    REGISTER_ENABLE_RINGS
    REGISTER_FILES2
    REGISTER_FILES_UPDATE2
    REGISTER_BUFFERS2
    REGISTER_BUFFERS_UPDATE
    REGISTER_IOWQ_AFF
    UNREGISTER_IOWQ_AFF
    REGISTER_IOWQ_MAX_WORKERS
    REGISTER_RING_FDS
    UNREGISTER_RING_FDS
    REGISTER_PBUF_RING
    UNREGISTER_PBUF_RING
    REGISTER_SYNC_CANCEL
    REGISTER_FILE_ALLOC_RANGE
    REGISTER_LAST
  
  RsrcRegister* = object
    nr*: uint32
    flags*: uint32
    resv2*: uint64
    data* {.align: 64.}: uint64
    tags* {.align: 64.}: uint64

  RsrcUpdate* = object
    offset*: uint32
    resv*: uint32
    data*: uint64

  RsrcUpdate2* = object
    offset*: uint32
    resv*: uint32
    data*: uint64
    tags*: uint64
    nr*: uint32
    resv2*: uint32

  NotificationSlot* = object
    tag*: uint64
    resv*: array[3, uint64]

  NotificationRegister* = object
    nrSlots*: uint32
    resv*: uint32
    resv2*: uint64
    data*: uint64
    resv3*: uint64
  
  ProbeOp* = object
    op*: uint8
    resv*: uint8
    flags*: uint16
    resv2*: uint32

  Probe* = object
    lastOp*: uint8
    opsLen*: uint8
    resv*: uint16
    resv2*: array[3, uint32]
    ops*: ptr ProbeOp

  Restriction* = object
    opcode*: RestrictionOp
    registerOp*: uint8
    sqeOp*: uint8
    sqeFlags*: uint8
    resv*: uint8
    resv2*: array[3, uint32]

  RestrictionOp* {.size: sizeof(uint16).} = enum
    RESTRICTION_REGISTER_OP
    RESTRICTION_SQE_OP
    RESTRICTION_SQE_FLAGS_ALLOWED
    RESTRICTION_SQE_FLAGS_REQUIRED
    RESTRICTION_LAST

  Buf* = object
    `addr`*: uint64
    len*: uint32
    bid*: uint16
    resv*: uint16

  BufRing* = object
    resv1*: uint64
    resv2*: uint32
    resv3*: uint16
    tail*: uint16
    bufs*: UncheckedArray[Buf]
  
  BufReg* = object
    ringAddr*: uint64
    ringEntries*: uint32
    bgid*: uint16
    pad*: uint16
    resv*: array[3, uint64]
  
  GeteventsArg* = object
    sigmask*: uint64
    sigmaskSz*: uint32
    pad*: uint32
    ts*: uint64
  
  SyncCancelReg* = object
    `addr`*: uint64
    fd*: int32
    flags*: uint32
    timeout*: Timespec
    pad*: array[4, uint64]
  
  FileIndexRange* = object
    off*: uint32
    len*: uint32
    resv*: uint64

  RecvmsgOut* = object
    namelen*: uint32
    controllen*: uint32
    payloadlen*: uint32
    flags*: uint32

const
  FILE_INDEX_ALLOC* = not 0u
  URING_CMD_FIXED* = not 0u
  TIMEOUT_CLOCK_MASK* = {TIMEOUT_BOOTTIME, TIMEOUT_REALTIME}
  TIMEOUT_UPDATE_MASK* = {TIMEOUT_UPDATE, LINK_TIMEOUT_UPDATE}
  SPLICE_F_FD_IN_FIXED* = 1u shl 32
  NOTIF_USAGE_ZC_COPIED* = 1u shl 32
  ACCEPT_MULTISHOT* = 1u shl 0
  CQE_BUFFER_SHIFT* = 16
  OFF_SQ_RING*: Off = 0
  OFF_CQ_RING*: Off = 0x8000000
  OFF_SQES*: Off = 0x10000000
  REGISTER_USE_REGISTERED_RING* = 1u shl 31
  IO_WQ_BOUND* = 0
  IO_WQ_UNBOUND* = 1
  RSRC_REGISTER_SPARSE* = 1u shl 0
  REGISTER_FILES_SKIP* = -2
  OP_SUPPORTED* = 1u shl 0

proc syscall(arg: cint): cint {.importc, header: "<unistd.h>", varargs.}
var
  SYS_io_uring_setup {.importc, header: "<sys/syscall.h>".}: cint
  SYS_io_uring_enter {.importc, header: "<sys/syscall.h>".}: cint
  SYS_io_uring_register {.importc, header: "<sys/syscall.h>".}: cint

proc setup*(entries: cint, params: ptr Params): FileHandle {.raises, tags: [].} =
  result = syscall(SYS_io_uring_setup, entries, params, 0, 0, 0, 0)
  if result < 0:
    raiseOSError(osLastError(), "io_uring setup syscall failed")

proc enter*(fd: cint, toSubmit: cint, minComplete: cint,
            flags: cint, sig: nil ptr Sigset, sz: cint): cint {.raises, tags: [].} =
  result = syscall(SYS_io_uring_enter, fd, toSubmit, minComplete, flags, sig, sz)
  if result < 0:
    raiseOSError(osLastError(), "io_uring enter syscall failed")

proc register*(fd: cint, op: cint, arg: nil pointer, nr_args: cint): cint {.raises, tags: [].} =
  result = syscall(SYS_io_uring_register, fd, op, arg, nr_args, 0, 0)
  if result < 0:
    raiseOSError(osLastError(), "io_uring register syscall failed")

# ============= QUEUE ==================

proc `+`(p: pointer; i: uint32): pointer =
  result = cast[pointer](cast[uint](p) + i.uint)

proc uringMap(offset: Off; fd: FileHandle; begin: uint32;
               count: uint32; typSize: int): pointer {.raises, tags: [].} =
  let size = int(begin + count * typSize.uint32)
  result = mmap(nil, size, PROT_READ or PROT_WRITE, MAP_SHARED or MAP_POPULATE,
                fd.cint, offset)
  if result == MAP_FAILED:
    raiseOSError(osLastError(), "io_uring uringMap failed")

proc uringUnmap(p: pointer; size: int) {.raises, tags: [].} =
  ## interface to tear down some memory (probably mmap'd)
  let code = munmap(p, size)
  if code < 0:
    raiseOSError(osLastError(), "io_uring uringUnmap failed")

type
  Ring = object of RootObj
    head*: ptr uint32
    tail*: ptr uint32
    mask*: ptr uint32
    entries*: ptr uint32
    size*: uint32
    ring*: pointer

  SqRing* = object of Ring
    flags*: ptr SqringFlags
    dropped*: pointer
    array*: pointer
    sqes*: ptr Sqe
    sqeTail*: uint32
    sqeHead*: uint32

  CqRing* = object of Ring
    flags*: ptr CqringFlags
    overflow*: ptr int
    cqes*: pointer
  
  Queue* = object
    params*: ptr Params
    fd*: FileHandle
    cq*: CqRing
    sq*: SqRing

const
  defaultFlags: SetupFlags = {}

proc newRing(fd: FileHandle; offset: ptr CqringOffsets; size: uint32): CqRing {.raises, tags: [].} =
  ## mmap a Cq ring from the given file-descriptor, using the size spec'd
  let ring = OFF_CQ_RING.uringMap(fd, offset.cqes, size, sizeof(Cqe))
  result = CqRing(
    size: size, ring: ring,
    cqes: ring + offset.cqes,
    overflow: cast[ptr int](ring + offset.overflow),
    flags: cast[ptr CqringFlags](ring + offset.flags),
    head: cast[ptr uint32](ring + offset.head),
    tail: cast[ptr uint32](ring + offset.tail),
    mask: cast[ptr uint32](ring + offset.ringMask),
    entries: cast[ptr uint32](ring + offset.ringEntries))
  # if offset.ringEntries <= 0:
  #   raise ERROR_io_uring_not_initializes

proc newRing(fd: FileHandle; offset: ptr SqringOffsets; size: uint32): SqRing {.raises, tags: [].} =
  ## mmap a Sq ring from the given file-descriptor, using the size spec'd
  let ring = OFF_SQ_RING.uringMap(fd, offset.array, size, sizeof(pointer))
  result = SqRing(
    size: size, ring: ring,
    dropped: ring + offset.dropped,
    array: ring + offset.array,
    flags: cast[ptr SqringFlags](ring + offset.flags),
    sqes: cast[ptr Sqe](OFF_SQES.uringMap(fd, 0, size, sizeof(Sqe))),
    head: cast[ptr uint32](ring + offset.head),
    tail: cast[ptr uint32](ring + offset.tail),
    mask: cast[ptr uint32](ring + offset.ringMask),
    entries: cast[ptr uint32](ring + offset.ringEntries))
  # Directly map SQ slots to SQEs
  var arr = cast[ptr UncheckedArray[uint32]](result.array)
  for i in 0..size.int:
    arr[i] = i.uint32
  # if offset.ringEntries <= 0:
  #   raise ERROR_io_uring_not_initializes

# XXX: is it ok that =destroy can raise error?
proc `=destroy`(queue: Queue) {.raises, tags: [].} =
  ## tear down the queue
  if queue.fd != 0:
    discard close(queue.fd)
  if queue.cq.ring != nil:
    uringUnmap(queue.cq.ring, queue.params.cqEntries.int * sizeof(Cqe))
  if queue.sq.ring != nil:
    uringUnmap(queue.sq.ring, queue.params.sqEntries.int * sizeof(pointer))
  if queue.sq.sqes != nil:
    uringUnmap(queue.sq.sqes, queue.params.sqEntries.int * sizeof(Sqe))
  if queue.params != nil:
    # deallocShared(queue.params)
    dealloc(queue.params)

proc `=sink`(dest: var Queue, source: Queue) =
  # avoid unmapping uring object after moving
  copyMem(dest.addr, source.addr, sizeof Queue)

proc `=copy`(dest: var Queue; source: Queue) {.error: "Queue can has only one owner".}

proc isPowerOfTwo(x: int): bool = (x != 0) and ((x and (x - 1)) == 0)

proc newQueue*(sqEntries: int;  flags = defaultFlags; sqThreadCpu = 0;
               sqThreadIdle = 0; wqFd = 0; cqEntries = 0
): Queue {.raises, tags: [].}  =
  # if not sqEntries.isPowerOfTwo:
  #   raise ERROR_entries_must_be_power_of_two
  # var params = createShared(Params)
  var params = cast[ptr Params](alloc(sizeof(Params)))
  params.flags = flags
  params.sqThreadCpu = sqThreadCpu.uint32
  params.sqThreadIdle = sqThreadIdle.uint32
  params.wqFd = wqFd.uint32
  params.cqEntries = cqEntries.uint32
  # ask the kernel for the file-descriptor to a ring pair of the spec'd size
  # this also populates the contents of the params object
  let fd = setup(sqEntries.cint, params)
  return Queue(
    fd: fd, params: params,
    sq: newRing(fd, addr params.sqOff, params.sqEntries),
    cq: newRing(fd, addr params.cqOff, params.cqEntries)
  )

proc sqFlush(queue: var Queue): int =
  ## Sync internal state with kernel ring state on the SQ side. Returns the
  ## number of pending items in the SQ ring, for the shared ring.
  var tail = queue.sq.sqeTail
  if queue.sq.sqeHead != tail:
    queue.sq.sqeHead = tail
    # Ensure kernel sees the SQE updates before the tail update.
    if SETUP_SQPOLL in queue.params.flags:
      atomicStore(queue.sq.tail[], tail, moRelaxed)
      # atomic_store_explicit(queue.sq.tail, tail, moRelaxed)
    else:
      atomicStore(queue.sq.tail[], tail, moRelease)
      # atomic_store_explicit(queue.sq.tail, tail, moRelease)
  # This _may_ look problematic, as we're not supposed to be reading
  # SQ->head without acquire semantics. When we're in SQPOLL mode, the
  # kernel submitter could be updating this right now. For non-SQPOLL,
  # task itself does it, and there's no potential race. But even for
  # SQPOLL, the load is going to be potentially out-of-date the very
  # instant it's done, regardless or whether or not it's done
  # atomically. Worst case, we're going to be over-estimating what
  # we can submit. The point is, we need to be able to deal with this
  # situation regardless of any perceived atomicity.
  return int(tail - queue.sq.head[])

proc getSqe*(queue: var Queue): nil ptr Sqe {.inline.} =
  ## Return an sqe to fill. Application must later call queue.submit()
  ## when it's ready to tell the kernel about it. The caller may call this
  ## function multiple times before calling queue.submit().
  ## Returns a vacant sqe, or nil if we're full.
  result = nil
  var
    head: uint32
    next = queue.sq.sqeTail + 1
    shift = 0
  if SETUP_SQE128 in queue.params.flags:
    shift = 1
  if SETUP_SQPOLL in queue.params.flags:
    # head = atomic_load_explicit(queue.sq.head, moRelaxed)
    head = atomicLoad(queue.sq.head[], moRelaxed)
  else:
    # head = atomic_load_explicit(queue.sq.head, moAcquire)
    head = atomicLoad(queue.sq.head[], moAcquire)
  if next - head <= queue.sq.entries[]:
    let index = (queue.sq.sqeTail and queue.sq.mask[]) shl shift
    result = cast[ptr Sqe](queue.sq.sqes + index.uint32 * sizeof(Sqe).uint32)
    result[] = default(Sqe)
    queue.sq.sqeTail = next

proc sqNeedsEnter(queue: var Queue; submit: int; flags: var EnterFlags): bool =
  ## Returns true if we're not using SQ thread (thus nobody submits but us)
  ## or if IORING_SQ_NEED_WAKEUP is set, so submit thread must be explicitly
  ## awakened. For the latter case, we set the thread wakeup flag.
  ## If no SQEs are ready for submission, returns false.
  if submit == 0:
    return false
  if SETUP_SQPOLL notin queue.params.flags:
    return true
  # Ensure the kernel can see the store to the SQ tail before we read
  # the flags.
  # atomic_thread_fence(moSequentiallyConsistent)
  atomicFence(moSequentiallyConsistent)
  # if SQ_NEED_WAKEUP in atomic_load_explicit(queue.sq.flags, moRelaxed):
  var sqFlags = atomicLoad(cast[ptr uint32](queue.sq.flags)[], moRelaxed)
  if SQ_NEED_WAKEUP in cast[ptr SqringFlags](sqFlags.addr)[]:
    flags.incl(ENTER_SQ_WAKEUP)
    return true
  return false;

proc cqNeedsFlush(queue: var Queue): bool =
  var sqFlags = atomicLoad(cast[ptr uint32](queue.sq.flags)[], moRelaxed)
  return {SQ_CQ_OVERFLOW, SQ_TASKRUN} <= cast[ptr SqringFlags](sqFlags.addr)[]
  # {SqCqOverflow, SqTaskrun} <= atomic_load_explicit(queue.sq.flags, moRelaxed)

proc cqNeedsEnter(queue: var Queue): bool =
  SETUP_IOPOLL in queue.params.flags or queue.cqNeedsFlush
  # SetupIopoll in queue.params.flags or queue.cqNeedsFlush

proc submit*(queue: var Queue; waitNr: uint = 0): int {.raises, tags: [], discardable.} =
  ## Submit sqes acquired from queue.getSqe() to the kernel.
  ## Returns number of sqes submitted
  let
    submited = queue.sqFlush
    cqNeedsEnter = waitNr > 0 or queue.cqNeedsEnter
  var flags: EnterFlags = {}
  if queue.sqNeedsEnter(submited, flags) or cqNeedsEnter:
    if cqNeedsEnter:
      flags.incl(ENTER_GETEVENTS)
    result = enter(queue.fd, submited.cint, waitNr.cint, cast[ptr cint](flags.addr)[], nil, 0.cint)
  else:
    result = submited

proc sqReady*(queue: var Queue): uint32 =
  ## Returns the number of flushed and unflushed SQEs pending in the submission queue.
  ## In other words, this is the number of SQEs in the submission queue, i.e. its length.
  ## These are SQEs that the kernel is yet to consume.
  ## Matches the implementation of io_uring_sq_ready in liburing.
  # Always use the shared ring state (i.e. head and not sqe_head) to avoid going out of sync,
  # see https://github.com/axboe/liburing/issues/92.
  # return queue.sq.sqeTail - atomic_load_explicit(queue.sq.head, moAcquire)
  return queue.sq.sqeTail - atomicLoad(queue.sq.head[], moAcquire)

proc cqReady*(queue: var Queue): uint32 =
  ## Returns the number of CQEs in the completion queue, i.e. its length.
  ## These are CQEs that the application is yet to consume.
  ## Matches the implementation of io_uring_cq_ready in liburing.
  # return atomic_load_explicit(queue.cq.tail, moAcquire) - queue.cq.head[]
  return atomicLoad(queue.cq.tail[], moAcquire) - queue.cq.head[]

proc waitReady(queue: var Queue; waitNr: uint = 0): uint32 {.raises, tags: [], inline.} =
  result = queue.cqReady
  if result == 0 and (queue.cqNeedsFlush or waitNr > 0):
    var flags = {ENTER_GETEVENTS}
    discard enter(queue.fd, 0.cint, waitNr.cint, cast[ptr cint](flags.addr)[], nil, 0.cint)
    result = queue.cqReady

proc copyCqesToSeq(queue: var Queue; cqes: seq[Cqe]; ready: uint32) {.inline.} =
  var
    head = queue.cq.head[]
    tail = head + ready
  let
    startIndex = int(head and queue.cq.mask[])
    endIndex = int(tail and queue.cq.mask[])
    startCount = queue.cq.entries[].int - startIndex
  
  if startCount < ready.int:
    # overflow needs 2 memcpy
    copyMem(cqes[0].addr, queue.cq.cqes + startIndex.uint32 * sizeof(Cqe).uint32, startCount * sizeof(Cqe))
    copyMem(cqes[startCount].addr, queue.cq.cqes, endIndex * sizeof(Cqe))
  else:
    copyMem(cqes[0].addr, queue.cq.cqes + startIndex.uint32 * sizeof(Cqe).uint32, ready.int * sizeof(Cqe))
  # atomic_store_explicit(queue.cq.head, tail, moRelease)
  atomicStore(queue.cq.head[], tail, moRelease)

proc copyCqes*(queue: var Queue; waitNr: uint = 0): seq[Cqe] {.raises, tags: [].} =
  ## Copies as many CQEs as are ready.
  ## If none are available, enters into the kernel to wait for at most `wait_nr` CQEs.
  ## Returns the number of CQEs copied, advancing the CQ ring.
  ## Provides all the wait/peek methods found in liburing, but with batching and a single method.
  ## The rationale for copying CQEs rather than copying pointers is that pointers are 8 bytes
  ## whereas CQEs are not much more at only 16 bytes, and this provides a safer faster interface.
  ## Safer, because you no longer need to call cqe_seen(), avoiding idempotency bugs.
  ## Faster, because we can now amortize the atomic store release to `cq.head` across the batch.
  ## See https://github.com/axboe/liburing/issues/103#issuecomment-686665007.
  ## Matches the implementation of io_uring_peek_batch_cqe() in liburing, but supports waiting.
  var ready = queue.waitReady(waitNr)
  if ready == 0:
    return @[]
  newSeq[Cqe](result, ready.int)
  copyCqesToSeq(queue, result, ready)

proc copyCqes*(queue: var Queue; cqes: seq[Cqe]; waitNr: uint = 0): int {.raises, tags: [].} =
  ## same as copyCqes(queue, waitNr) but copy cqes to your array
  ## returns copied cqe count
  var ready = queue.waitReady(waitNr)
  if ready == 0:
    return ready.int
  copyCqesToSeq(queue, cqes, ready)
  return ready.int

proc registerFiles*(q: var Queue; fds: seq[FileHandle]): int {.raises, tags: [], discardable.} =
  ## Registers an array of file descriptors.
  ## Every time a file descriptor is put in an SQE and submitted to the kernel, the kernel must
  ## retrieve a reference to the file, and once I/O has completed the file reference must be
  ## dropped. The atomic nature of this file reference can be a slowdown for high IOPS workloads.
  ## This slowdown can be avoided by pre-registering file descriptors.
  ## To refer to a registered file descriptor, IOSQE_FIXED_FILE must be set in the SQE's flags,
  ## and the SQE's fd must be set to the index of the file descriptor in the registered array.
  ## Registering file descriptors will wait for the ring to idle.
  ## Files are automatically unregistered by the kernel when the ring is torn down.
  ## An application need unregister only if it wants to register a new array of file descriptors.
  return register(q.fd.cint, REGISTER_FILES.cint, fds[0].addr, fds.len.cint)

proc registerFilesUpdate*(q: var Queue; offset: Off; fds: seq[FileHandle]): int {.raises, tags: [], discardable.} =
  ## Updates registered file descriptors.
  ##
  ## Updates are applied starting at the provided offset in the original file descriptors slice.
  ## There are three kind of updates:
  ## * turning a sparse entry (where the fd is -1) into a real one
  ## * removing an existing entry (set the fd to -1)
  ## * replacing an existing entry with a new fd
  ## Adding new file descriptors must be done with `register_files`.
  let update = RsrcUpdate(
    offset: offset.uint32,
    data: cast[uint64](fds[0].addr)
  )
  return register(q.fd.cint, REGISTER_FILES_UPDATE.cint, update.addr, fds.len.cint)

proc unregisterFiles*(q: var Queue;): int {.raises, tags: [], discardable.} =
  ## Unregisters all registered file descriptors previously associated with the ring.
  return register(q.fd.cint, UNREGISTER_FILES.cint, nil, 0)

proc registerEventFd*(q: var Queue; fd: FileHandle): int {.raises, tags: [], discardable.} =
  ## Registers the file descriptor for an eventfd that will be notified of completion events on
  ##  an io_uring instance.
  ## Only a single a eventfd can be registered at any given point in time.
  return register(q.fd.cint, REGISTER_EVENTFD.cint, fd.addr, 1)

proc registerEventFdAsync*(q: var Queue; fd: FileHandle): int {.raises, tags: [], discardable.} =
  ## Registers the file descriptor for an eventfd that will be notified of completion events on
  ## an io_uring instance. Notifications are only posted for events that complete in an async manner.
  ## This means that events that complete inline while being submitted do not trigger a notification event.
  ## Only a single eventfd can be registered at any given point in time.
  return register(q.fd.cint, REGISTER_EVENTFD_ASYNC.cint, fd.addr, 1)

proc unregisterEventFd*(q: var Queue;): int {.raises, tags: [], discardable.} =
  ## Unregister the registered eventfd file descriptor.
  return register(q.fd.cint, UNREGISTER_EVENTFD.cint, nil, 0)

proc registerBuffers*(q: var Queue; buffers: seq[IOVec]): int {.raises, tags: [], discardable.} =
  ## Registers an array of buffers for use with `read_fixed` and `write_fixed`.
  ## known issues:
  ## * EOPNOTSUPP
  ##   User buffers point to file-backed memory.
  ##   error occured then you try to pass pointer allocated on stack
  ##   use alloc or alloc0
  return register(q.fd.cint, REGISTER_BUFFERS.cint, buffers[0].addr, buffers.len.cint)

proc unregisterBuffers*(q: var Queue;): int {.raises, tags: [], discardable.} =
  ## Unregister the registered buffers.
  return register(q.fd.cint, UNREGISTER_BUFFERS.cint, nil, 0)

# ============= OPS ==================

proc setUserData*[T: SomeNumber | pointer](sqe: ptr Sqe, userData: T): ptr Sqe =
  sqe.userData = cast[pointer](userData)
  return sqe

proc linkNext*(sqe: ptr Sqe): ptr Sqe =
  sqe.flags.incl(SQE_IO_LINK)
  return sqe

proc drainPrevious*(sqe: ptr Sqe): ptr Sqe =
  sqe.flags.incl(SQE_IO_DRAIN)
  return sqe

proc nop*(sqe: ptr Sqe): ptr Sqe =
  sqe.opcode = OP_NOP
  return sqe


proc prepRw[
  FD: FileHandle | SocketHandle,
  ADDR: pointer | SomeNumber,
  OFF: pointer | SomeNumber,
  LEN: SomeNumber
](sqe: ptr Sqe, op: Op; fd: FD; `addr`: ADDR; len: LEN; offset: OFF): ptr Sqe =
  sqe.opcode = op
  sqe.fd = cast[FileHandle](fd)
  sqe.off.off = cast[Off](offset)
  sqe.`addr`.`addr` = cast[pointer](`addr`)
  sqe.len = cast[int32](len)
  return sqe

proc fsync*(sqe: ptr Sqe; fd: FileHandle; flags: FsyncFlags = {}): ptr Sqe =
  sqe.opcode = OP_FSYNC
  sqe.fd = fd
  sqe.opFlags.fsyncFlags = flags
  return sqe

proc fallocate*(sqe: ptr Sqe; fd: FileHandle; mode: FileMode; offset: Off; len: int): ptr Sqe =
  sqe.prepRw(OP_FALLOCATE, fd, len, mode.int, offset)

proc statx*(sqe: ptr Sqe; fd: FileHandle; path: var string; flags: uint32; mask: uint32; buf: ptr Stat): ptr Sqe =
  sqe.opFlags.statxFlags = flags
  sqe.prepRw(OP_STATX, fd, cast[pointer](path.toCString), mask, cast[pointer](buf))

proc read*(sqe: ptr Sqe; fd: FileHandle; buffer: pointer; len: int; offset: int = 0): ptr Sqe =
  sqe.prepRw(OP_READ, fd, buffer, len, offset)

proc read*(sqe: ptr Sqe; fd: FileHandle; group_id: uint16, len: int, offset: int = 0): ptr Sqe =
  sqe.flags.incl(SQE_BUFFER_SELECT)
  sqe.buf.bufIndex = group_id
  sqe.prepRw(OP_READ, fd, 0, len, offset)

proc readv*(sqe: ptr Sqe; fd: FileHandle; iovecs: seq[IOVec]; offset: int = 0): ptr Sqe =
  sqe.prepRw(OP_READV, fd, cast[pointer](iovecs[0].addr), len(iovecs), offset)

proc readv*(sqe: ptr Sqe; fd: FileHandle; iovec: ptr IOVec; offset: int = 0): ptr Sqe =
  sqe.prepRw(OP_READV, fd, cast[pointer](iovec), 1, offset)

proc read_fixed*(sqe: ptr Sqe; fd: FileHandle; iovec: IOVec; offset: int = 0; bufferIndex: int = 0): ptr Sqe =
  sqe.buf.bufIndex = bufferIndex.uint16
  sqe.prepRw(OP_READ_FIXED, fd, iovec.iov_base, iovec.iov_len, offset)

proc write*(sqe: ptr Sqe; fd: FileHandle; buffer: pointer; len: int; offset: int = 0): ptr Sqe =
  sqe.prepRw(OP_WRITE, fd, buffer, len, offset)

proc write*(sqe: ptr Sqe; fd: FileHandle; str: var string; offset: int = 0): ptr Sqe =
  sqe.write(fd, cast[pointer](str.toCString), len(str), offset)

proc writev*(sqe: ptr Sqe; fd: FileHandle; iovecs: seq[IOVec]; offset: int = 0): ptr Sqe =
  sqe.prepRw(OP_WRITEV, fd, cast[pointer](iovecs[0].addr), len(iovecs), offset)

proc write_fixed*(sqe: ptr Sqe; fd: FileHandle; iovec: IOVec, offset: int = 0, bufferIndex: int = 0): ptr Sqe =
  sqe.buf.bufIndex = bufferIndex.uint16
  sqe.prepRw(OP_WRITE_FIXED, fd, iovec.iov_base, iovec.iov_len, offset)


proc accept*(sqe: ptr Sqe; sock: SocketHandle, `addr`: ptr SockAddr, addrLen: ptr SockLen, flags: cint): ptr Sqe =
  sqe.opFlags.acceptFlags = flags.uint32
  sqe.prepRw(OP_ACCEPT, sock, cast[pointer](`addr`), 0, cast[pointer](addrLen))

proc accept_multishot*(sqe: ptr Sqe; sock: SocketHandle, `addr`: ptr SockAddr, addrLen: ptr SockLen, flags: cint): ptr Sqe =
  sqe.ioprio.incl(RECVSEND_POLL_FIRST)
  sqe.accept(sock, `addr`, addrLen, flags)

proc connect*(sqe: ptr Sqe; sock: SocketHandle, `addr`: ptr SockAddr, addrLen: SockLen): ptr Sqe =
  sqe.prepRw(OP_CONNECT, sock, cast[pointer](`addr`), 0, cast[pointer](addrLen))

proc epoll_ctl*(sqe: ptr Sqe; epfd: FileHandle; fd: FileHandle; op: uint32; ev: ptr EpollEvent): ptr Sqe =
  sqe.prepRw(OP_EPOLL_CTL, epfd, cast[pointer](ev), op, fd)

# XXX: needs std/endians

# proc poll_add*(sqe: ptr Sqe; fd: FileHandle; pollMask: uint32): ptr Sqe =
#   littleEndian32(addr result.opFlags.poll32Events, addr pollMask)
#   sqe.prepRw(OP_POLL_ADD, fd, nil, 1, 0)

# proc poll_multi*(sqe: ptr Sqe; fd: FileHandle; pollMask: uint32): ptr Sqe =
#   var flags = PollFlags({POLL_ADD_MULTI})
#   sqe.len = cast[ptr int](flags.addr)[]
#   sqe.poll_add(fd, pollMask)

# proc poll_remove*(sqe: ptr Sqe; targetUserData: UserData): ptr Sqe =
#   sqe.prepRw(OP_POLL_REMOVE, -1, target_user_data, 0, 0)

# proc poll_update*(sqe: ptr Sqe; oldUserData: UserData; newUserData: UserData; pollMask: uint32, flags: uint32): ptr Sqe =
#   littleEndian32(addr result.opFlags.poll32Events, addr pollMask)
#   sqe.prepRw(OP_POLL_REMOVE, -1, oldUserData, int flags, cast[int](newUserData))


proc recv*(sqe: ptr Sqe; sock: SocketHandle; buffer: pointer; len: int; flags: cint = 0): ptr Sqe =
  sqe.opFlags.msgFlags = flags.uint32
  sqe.prepRw(OP_RECV, sock, buffer, len, 0)

proc recv_multishot*(sqe: ptr Sqe; sock: SocketHandle; buffer: pointer; len: int; flags: cint): ptr Sqe =
  sqe.ioprio.incl(RECV_MULTISHOT)
  sqe.recv(sock, buffer, len, flags)

proc send*(sqe: ptr Sqe; sock: SocketHandle; buffer: pointer; len: int; flags: cint = 0): ptr Sqe =
  sqe.opFlags.msgFlags = flags.uint32
  sqe.prepRw(OP_SEND, sock, buffer, len, 0)

proc send*(sqe: ptr Sqe; sock: SocketHandle; str: var string; flags: cint = 0): ptr Sqe =
  sqe.send(sock, cast[pointer](str.toCString), str.len, 0)

proc send_zc*(sqe: ptr Sqe; sock: SocketHandle; buffer: pointer; len: int; flags: uint32; zc_flags: uint; buf_index: uint): ptr Sqe =
  sqe.opFlags.msgFlags = flags
  sqe.ioprio = cast[ptr IoprioFlags](zc_flags.addr)[]
  sqe.prepRw(OP_SEND_ZC, sock, buffer, len, 0)


proc recvmsg*(sqe: ptr Sqe; sock: SocketHandle; msghdr: ptr Tmsghdr; flags: cint): ptr Sqe =
  sqe.opFlags.msgFlags = flags.uint32
  sqe.prepRw(OP_RECVMSG, sock, cast[pointer](msghdr), 1, 0)

proc recvmsg_multishot*(sqe: ptr Sqe; sock: SocketHandle; msghdr: ptr Tmsghdr; flags: cint): ptr Sqe =
  sqe.ioprio.incl(RECV_MULTISHOT)
  sqe.recvmsg(sock, msghdr, flags)

proc sendmsg*(sqe: ptr Sqe; sock: SocketHandle; msghdr: ptr Tmsghdr; flags: cint): ptr Sqe =
  sqe.opFlags.msgFlags = flags.uint32
  sqe.prepRw(OP_SENDMSG, sock, cast[pointer](msghdr), 1, 0)

proc sendmsg_zc*(sqe: ptr Sqe; sock: SocketHandle; msghdr: ptr Tmsghdr; flags: cint): ptr Sqe =
  sqe.opFlags.msgFlags = flags.uint32
  sqe.prepRw(OP_SENDMSG_ZC, sock, cast[pointer](msghdr), 1, 0)


proc openat*(sqe: ptr Sqe; dfd: FileHandle; path: var string; flags: int32 = 0; mode: set[FilePermission] = {}): ptr Sqe =
  sqe.opFlags.openFlags = cast[uint32](flags)
  sqe.prepRw(OP_OPENAT, dfd, cast[pointer](path.toCString), cast[ptr cint](mode.addr)[], 0)

proc close*[T: FileHandle | SocketHandle](sqe: ptr Sqe; fd: T): ptr Sqe =
  sqe.opcode = OP_CLOSE
  sqe.fd = cast[int32](fd)
  return sqe

proc renameat*(sqe: ptr Sqe; oldDirFd: FileHandle; oldPath: var string; newDirFd: FileHandle; newPath: var string; flags: uint32): ptr Sqe =
  sqe.opFlags.renameFlags = flags
  sqe.prepRw(OP_RENAMEAT, oldDirFd, cast[pointer](oldPath.toCString), newDirFd.int, cast[int](newPath.toCString))

proc unlinkat*(sqe: ptr Sqe; dirFd: FileHandle; path: var string; flags: uint32): ptr Sqe =
  sqe.opFlags.unlinkFlags = flags
  sqe.prepRw(OP_UNLINKAT, dirFd, cast[pointer](path.toCString), 0, 0)

proc mkdirat*(sqe: ptr Sqe; dirFd: FileHandle; path: var string; mode: uint32): ptr Sqe =
  sqe.prepRw(OP_MKDIRAT, dirFd, cast[pointer](path.toCString), mode, 0)

proc symlinkat*(sqe: ptr Sqe; target: var string; newDirFd: FileHandle; linkPath: var string): ptr Sqe =
  sqe.prepRw(OP_SYMLINKAT, newDirFd, cast[pointer](target.toCString), 0, cast[pointer](linkPath.toCString))

proc linkat*(sqe: ptr Sqe; oldDirFd: FileHandle; oldPath: var string; newDirFd: FileHandle; newPath: var string; flags: uint32): ptr Sqe =
  sqe.opFlags.hardlinkFlags = flags
  sqe.prepRw(OP_LINKAT, oldDirFd, cast[pointer](oldPath.toCString), newDirFd, cast[pointer](newPath.toCString))


proc timeout*(sqe: ptr Sqe; ts: ptr Timespec, count: uint32; flags: TimeoutFlags): ptr Sqe =
  sqe.opFlags.timeoutFlags = flags
  sqe.prepRw(OP_TIMEOUT, -1.cint, cast[pointer](ts), 1, count)

proc timeout_remove*(sqe: ptr Sqe; timeout_user_data: pointer; flags: TimeoutFlags): ptr Sqe =
  sqe.opFlags.timeoutFlags = flags
  sqe.prepRw(OP_TIMEOUT_REMOVE, -1.cint, timeout_user_data, 0, 0)

proc link_timeout*(sqe: ptr Sqe; ts: Timespec; flags: TimeoutFlags): ptr Sqe =
  sqe.opFlags.timeoutFlags = flags
  sqe.prepRw(OP_LINK_TIMEOUT, -1.cint, cast[pointer](ts.addr), 1, 0)


proc cancel*[T: SomeNumber | pointer](sqe: ptr Sqe; cancelUserData: T; flags: uint32): ptr Sqe =
  sqe.opFlags.cancelFlags = flags
  sqe.prepRw(OP_ASYNC_CANCEL, -1.cint, cancelUserData, 0, 0)

proc shutdown*(sqe: ptr Sqe; sockfd: FileHandle; how: uint32): ptr Sqe =
  sqe.prepRw(OP_SHUTDOWN, sockfd, 0, how.int, 0)


proc provide_buffers*(sqe: ptr Sqe; buffers: pointer; bufferSize: int; buffersCount: int; groupId: uint; bufferId: uint): ptr Sqe =
  sqe.buf.bufIndex = groupId.uint16
  sqe.prepRw(OP_PROVIDE_BUFFERS, cast[FileHandle](buffersCount), buffers, bufferSize, bufferId.int)

proc remove_buffers*(sqe: ptr Sqe; buffersCount: int; groupId: uint;): ptr Sqe =
  sqe.buf.bufIndex = groupId.uint16
  sqe.prepRw(OP_REMOVE_BUFFERS, cast[FileHandle](buffersCount), 0, 0, 0)

proc sync_file_range*(sqe: ptr Sqe; fd: FileHandle; len: int; flags: uint32; offset: Off = 0): ptr Sqe =
  sqe.opFlags.syncRangeFlags = flags
  sqe.prepRw(OP_SYNC_FILE_RANGE, fd, 0, len, offset)

proc files_update*(sqe: ptr Sqe, fds: seq[FileHandle], offset: int = 0): ptr Sqe =
  sqe.prepRw(OP_FILES_UPDATE, -1.cint, cast[pointer](fds[0].addr), fds.len, offset)

proc fadvice*(sqe: ptr Sqe, fd: FileHandle, len: int, advice: int, offset: int = 0): ptr Sqe =
  sqe.opFlags.fadviseAdvice = advice.uint32
  sqe.prepRw(OP_FADVISE, fd, 0, len, offset)

proc madvice*(sqe: ptr Sqe; `addr`: pointer; len: int; advice: int): ptr Sqe =
  sqe.opFlags.fadviseAdvice = advice.uint32
  sqe.prepRw(OP_MADVISE, -1.cint, `addr`, len, 0)

proc splice*(sqe: ptr Sqe; fd_in: FileHandle; off_in: int; fd_out: FileHandle; off_out: int; len: int; flags: int = 0, fixed: bool = false): ptr Sqe =
  sqe.opcode = OP_SPLICE
  sqe.fd = fd_out
  sqe.len = len.int32
  sqe.off = cast[ptr InnerSqeOffset](off_out.addr)[]
  sqe.splice.spliceFdIn = fd_in.uint32
  sqe.`addr`.spliceOffIn = off_in
  var spliceFlags = 
    if fixed:
      flags.uint32 or SPLICE_F_FD_IN_FIXED.uint32
    else:
      flags.uint32
  sqe.opFlags.spliceFlags = spliceFlags
  return sqe

proc tee*(sqe: ptr Sqe, fd_in: FileHandle, fd_out: FileHandle; len: int; flags: int = 0, fixed: bool = false): ptr Sqe =
  sqe.opcode = OP_TEE
  sqe.fd = fd_out
  sqe.len = len.int32
  sqe.splice.spliceFdIn = fd_in.uint32
  var spliceFlags = 
    if fixed:
      flags.uint32 or SPLICE_F_FD_IN_FIXED.uint32
    else:
      flags.uint32
  sqe.opFlags.spliceFlags = spliceFlags
  return sqe

proc msg_ring*(sqe: ptr Sqe; ring_fd: FileHandle; res: int; user_data: uint64; user_flags: uint32 = 0; opcode_flags: uint32 = 0): ptr Sqe =
  sqe.opFlags.msgRingFlags = cast[ptr MsgRingOpFlags](opcode_flags.addr)[]
  sqe.prepRw(OP_MSG_RING, ring_fd, MSG_DATA.cint, res, user_data)

proc fsetxattr*(sqe: ptr Sqe; fd: FileHandle; name: var string; value: var string; flags: int = 0): ptr Sqe =
  sqe.opFlags.xattrFlags = flags.uint32
  # TODO: which len?
  sqe.prepRw(OP_FSETXATTR, fd, cast[pointer](name.toCString), name.len, cast[pointer](value.toCString))

proc setxattr*(sqe: ptr Sqe, name: var string; value: var string; path: var string; flags: int = 0): ptr Sqe =
  sqe.opFlags.xattrFlags = flags.uint32
  sqe.cmd.addr3 = cast[pointer](path.toCString)
  sqe.prepRw(OP_SETXATTR, 0.cint, cast[pointer](name.toCString), name.len, cast[pointer](value.toCString))

proc fgetxattr*(sqe: ptr Sqe; fd: FileHandle; name: var string; buf: pointer; len: int): ptr Sqe =
  sqe.prepRw(OP_FGETXATTR, fd, cast[pointer](name.toCString), len, buf)

proc getxattr*(sqe: ptr Sqe; name: var string; buf: pointer; len: int; path: var string;): ptr Sqe =
  sqe.cmd.addr3 = cast[pointer](path.toCString)
  sqe.prepRw(OP_GETXATTR, 0.cint, cast[pointer](name.toCString), len, buf)

proc socket*(sqe: ptr Sqe; domain: Domain; `type`: SockType; protocol: Protocol; flags: int = 0): ptr Sqe =
  sqe.opFlags.rwFlags = flags
  sqe.prepRw(OP_SOCKET, domain.cint, 0, protocol.cint, `type`.cint)

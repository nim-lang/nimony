# Thread module for Nimony

{.feature: "lenientnils".}

import std / [oserrors, atomics]

const nativeThreads* = defined(nimNoLibc) and defined(linux) and defined(amd64)
  ## Whether threads here are the runtime's own `clone(2)` rather than an OS
  ## thread library's. True on the native (arkham + nifasm) backend, which links
  ## no libc and therefore has no pthreads; false everywhere else, `nimony c`
  ## included — that build is raw-syscall too, but libc is still linked.

when defined(windows):
  import windows/winlean

  type
    SysThread = Handle
    WinThreadProc = proc (x: pointer): uint32 {.stdcall.}

  proc createThread(lpThreadAttributes: pointer, dwStackSize: uint,
                     lpStartAddress: WinThreadProc,
                     lpParameter: pointer,
                     dwCreationFlags: uint32,
                     lpThreadId: var uint32): SysThread {.
    stdcall, importc: "CreateThread", dynlib: "kernel32".}

  proc winSuspendThread(hThread: SysThread): int32 {.
    stdcall, dynlib: "kernel32", importc: "SuspendThread".}

  proc winResumeThread(hThread: SysThread): int32 {.
    stdcall, dynlib: "kernel32", importc: "ResumeThread".}

  proc waitForSingleObject(hHandle: SysThread, dwMilliseconds: int32): int32 {.
    stdcall, dynlib: "kernel32", importc: "WaitForSingleObject".}

  proc waitForMultipleObjects*(nCount: int32,
                              lpHandles: ptr SysThread,
                              bWaitAll: int32,
                              dwMilliseconds: int32): int32 {.
    stdcall, dynlib: "kernel32", importc: "WaitForMultipleObjects".}

  proc terminateThread*(hThread: SysThread, dwExitCode: int32): int32 {.
    stdcall, dynlib: "kernel32", importc: "TerminateThread".}

  proc setThreadAffinityMask*(hThread: SysThread, dwThreadAffinityMask: uint): uint {.
    importc: "SetThreadAffinityMask", stdcall, dynlib: "kernel32".}

elif defined(genode):
  const
    GenodeHeader = "genode_cpp/threads.h"
  type
    SysThread {.importcpp: "Nim::SysThread",
                 header: GenodeHeader, final, pure.} = object
    GenodeThreadProc = proc (x: pointer) {.noconv.}

  proc initThread(s: var SysThread,
                  env: GenodeEnv,
                  stackSize: culonglong,
                  entry: GenodeThreadProc,
                  arg: pointer,
                  affinity: cuint) {.
    importcpp: "#.initThread(@)".}


elif nativeThreads:
  # Native (arkham + nifasm) threads. There is no libc here, so there are no
  # pthreads: a thread is `clone(2)` plus the three things libc's wrapper would
  # otherwise have supplied — a stack, a thread-local block, and a word for
  # `join` to wait on. See `cloneRaw` for why the syscall cannot be *called*.
  import std / posix / posix

  type
    SysThread = int32
      ## The child's kernel TID while it runs, 0 once it is gone. One word rather
      ## than a handle because that is what the kernel offers: with
      ## `CLONE_CHILD_CLEARTID` it zeroes this word and FUTEX_WAKEs it as the
      ## thread dies, and that is the whole of `join`.

  const
    NativeStackBytes = 8 * 1024 * 1024
      ## The default stack, matching glibc's, so a recursion depth that works on
      ## the C backend works here too. Pages are committed on first touch, so
      ## this spends address space rather than memory.
    CloneNr = 56
    # clone(2) flags — glibc's thread set: share the address space, the open
    # files, the signal handlers and the SysV semaphore undo list, and join the
    # caller's thread group so the result is one process with two threads.
    CloneVm = 0x00000100'u
    CloneFs = 0x00000200'u
    CloneFiles = 0x00000400'u
    CloneSighand = 0x00000800'u
    CloneThread = 0x00010000'u
    CloneSysvsem = 0x00040000'u
    CloneSettls = 0x00080000'u
    CloneParentSettid = 0x00100000'u
    CloneChildCleartid = 0x00200000'u
    ThreadCloneFlags = CloneVm or CloneFs or CloneFiles or CloneSighand or
                       CloneThread or CloneSysvsem or CloneSettls or
                       CloneParentSettid or CloneChildCleartid
      ## `PARENT_SETTID` and `CHILD_CLEARTID` name the SAME word, which is what
      ## makes `join` race-free: the kernel writes the tid there before the child
      ## runs, so a child that exits immediately cannot have its 0 overwritten by
      ## a parent still storing the return value of `clone`.
    FutexWait = clong(0)
      ## Deliberately NOT `FUTEX_WAIT_PRIVATE`. The kernel's `CLONE_CHILD_CLEARTID`
      ## wake in `mm_release` is a plain `FUTEX_WAKE` — no private flag — and
      ## private and shared futexes hash into different queues, so a private
      ## waiter on this word is never woken. (glibc's `lll_wait_tid` passes
      ## `LLL_SHARED` for exactly this reason.) With the flag, `join` blocks
      ## forever on a thread that has already exited.
    MapStack = cint(0x20000)

  proc futex(uaddr: ptr uint32; op, val: clong; timeout: nil pointer): clong {.
    importc: "futex", sideEffect.}
    ## Named, not `syscall(SYS_futex, …)`: arkham recognises the name and lowers
    ## it to the raw syscall — the same declaration `std/private/syslocks` makes,
    ## and for the same reason (there is no libc symbol called `futex`).
  proc threadExitSyscall(code: cint) {.importc: "exit".}
    ## `exit`, NOT `exit_group`: it ends this thread and leaves the process alone.
  proc gettidSyscall(): cint {.importc: "gettid".}
  proc schedSetaffinity(pid: cint; setsize: csize_t; mask: pointer): cint {.
    importc: "sched_setaffinity".}
    ## glibc's `pthread_setaffinity_np` is this syscall plus a handle lookup; a
    ## thread that pins ITSELF (pid 0) has no lookup to do.

  proc syscallInstr() {.instruction: "syscall".}
  proc testq(a, b: int) {.instruction: "test".}
  proc zf(): bool {.instruction: "zf".}
  proc tlsSizeCell(): pointer {.intrinsic: "TlsSize".}

  proc cloneRaw(flags {.register: "rdi".}: uint;
                childStack {.register: "rsi".}: nil pointer;
                ptid {.register: "rdx".}: nil pointer;
                ctid {.register: "rcx".}: nil pointer;
                tls {.register: "r8".}: nil pointer;
                childArg {.register: "r9".}: nil pointer): int {.
                assembler, naked.} =
    ## `clone`, written out because it cannot be CALLED.
    ##
    ## The child returns from the syscall at the very next instruction but with a
    ## different stack pointer — the one handed to the kernel. Every local of the
    ## surrounding proc lived on the parent's stack and is gone, and an epilogue
    ## would pop a return address from a stack that never held a frame. So the
    ## instruction has to sit where the stack is said out loud. `{.naked.}` means
    ## the only thing this proc's `ret` needs is a return address at `[rsp]`, and
    ## `create` plants the child's entry point at exactly that address: the child
    ## *returns into* `childEntry`, on its own stack, which is a call with the
    ## frame already in place.
    ##
    ## The two register moves are the ABI seam. The 4th syscall argument travels
    ## in r10 while the 4th C argument arrives in rcx, so it is relayed; and the
    ## child's argument goes into rdi — the 1st C argument register — only on the
    ## child's side of the branch, where rdi no longer holds the flags the
    ## syscall needed. Registers are the only channel a child has: the kernel
    ## copies the whole register file and replaces exactly rax and rsp.
    ##
    ## The parameter ORDER is the kernel's own, which x86-64 spells
    ## `(flags, newsp, parent_tidptr, child_tidptr, tls)` — the generic one.
    ## (`CONFIG_CLONE_BACKWARDS`, which swaps the last two, is 32-bit x86 and
    ## Arm; getting them the wrong way round here still "works" — the child runs
    ## and prints — because it points FS at whatever the tid pointer was and asks
    ## the kernel to clear a word inside the thread-local block, and neither is
    ## noticed until a thread-local is read or `join` is called.)
    var nr {.register: "rax".}: int
    var syscallArg4 {.register: "r10".}: nil pointer
    var childArgReg {.register: "rdi".}: nil pointer
    syscallArg4 = ctid
    nr = CloneNr
    syscallInstr()
    testq(nr, nr)
    if zf():
      childArgReg = childArg
    result = nr

elif defined(nimNoLibc):
  {.error: "std/rawthreads has no thread implementation for this freestanding " &
           "target; only Linux/x86-64 has one (see the `clone` path above). " &
           "Every other `nimNoLibc` target would link against pthreads, which " &
           "is not there.".}

else:
  when not (defined(macosx) or defined(haiku)):
    {.passL: "-pthread".}

  when not defined(haiku):
    {.passC: "-pthread".}

  type
    SysThread = distinct culong  ## pthread_t: unsigned long on glibc/musl,
                                 ## a pointer on Darwin — same width either way
    Pthread_attr = object ## pthread_attr_t as an opaque, oversized blob:
                          ## 56 B on glibc 64-bit, 36 B on i386, 64 B on
                          ## Darwin. int64 elements give both ABIs' alignment.
      abi: array[8, int64]

  proc pthread_attr_init(a1: var Pthread_attr): cint {.
    importc: "pthread_attr_init".}
  proc pthread_attr_setstack(a1: ptr Pthread_attr, a2: pointer, a3: int): cint {.
    importc: "pthread_attr_setstack".}
  proc pthread_attr_setstacksize(a1: var Pthread_attr, a2: int): cint {.
    importc: "pthread_attr_setstacksize".}
  proc pthread_attr_destroy(a1: var Pthread_attr): cint {.
    importc: "pthread_attr_destroy".}

  proc pthread_create(a1: var SysThread, a2: var Pthread_attr,
            a3: proc (x: pointer): pointer {.noconv.},
            a4: pointer): cint {.importc: "pthread_create".}
  proc pthread_join(a1: SysThread, a2: ptr pointer): cint {.
    importc: "pthread_join".}

  proc pthread_cancel(a1: SysThread): cint {.
    importc: "pthread_cancel".}

when defined(posix) and not defined(macosx):
  type CpuSet = object ## cpu_set_t (glibc: 1024-bit mask)
    abi: array[16, uint64]

  func cpusetZero(s: var CpuSet) =
    for i in 0 ..< s.abi.len: s.abi[i] = 0'u64
  func cpusetIncl(cpu: cint; s: var CpuSet) =
    # CPU_SET is a header macro over the bit array; reimplemented natively.
    if cpu >= 0 and int(cpu) < s.abi.len * 64:
      s.abi[int(cpu) shr 6] = s.abi[int(cpu) shr 6] or (1'u64 shl (int(cpu) and 63))

  proc setAffinity(thread: SysThread; setsize: csize_t; s: var CpuSet) {.
    importc: "pthread_setaffinity_np".}


type
  RawThread* = object ## OS thread wrapper holding the system handle and entry closure.
    sys*: SysThread
    dataFn: proc (arg: pointer) {.nimcall.}
    data: pointer
    when nativeThreads:
      # The stack + thread-local mapping this thread was given. An OS thread
      # library owns this; here there is none, so `join` gives it back and needs
      # to be told what to give back.
      stackMem: nil pointer
      stackBytes: int

proc `=copy`(dest: var RawThread; src: RawThread) {.error.}

template nimThreadProcWrapperBody(closure: pointer) =
  let t = cast[ptr RawThread](closure)
  t.dataFn(t.data)

when defined(windows):
  proc threadProcWrapper(closure: pointer): uint32 {.stdcall.} =
    result = 0'u32
    nimThreadProcWrapperBody(closure)
    # implicitly return 0
elif defined(genode):
  proc threadProcWrapper(closure: pointer) {.noconv.} =
    nimThreadProcWrapperBody(closure)
elif nativeThreads:
  proc childEntry(arg: pointer) {.nimcall.} =
    ## Where a new thread begins. `cloneRaw`'s `ret` lands here with `arg` in rdi
    ## and a stack that is this thread's alone — so this is an ordinary Nimony
    ## proc from its first instruction, which is the point of the arrangement.
    nimThreadProcWrapperBody(arg)
    # Never `return`: nothing put a return address below this frame, and there is
    # no caller to go back to. `exit` ends this thread; `exit_group` would end
    # the process.
    threadExitSyscall(0.cint)

  proc procBits(p: proc (arg: pointer) {.nimcall.}): uint =
    ## The code address of a `nimcall` proc, as a plain word. `cast` refuses to go
    ## from a proc type to an integer; reading the variable's own bytes is how it
    ## is said, and for a `nimcall` those bytes ARE the entry point.
    var tmp = p
    result = cast[ptr uint](addr tmp)[]

else:
  proc threadProcWrapper(closure: pointer): pointer {.noconv.} =
    result = nil
    nimThreadProcWrapperBody(closure)

when defined(genode):
  var affinityOffset: cuint = 1
    ## CPU affinity offset for next thread, safe to roll-over.

proc create*(t {.noinit.}: out RawThread; fn: proc (arg: pointer) {.nimcall.}; arg: pointer;
            stackSize = 0; pinnedToCpu = -1) {.raises.} =
  ## Spawns a thread running `fn(arg)`. `stackSize` selects a custom stack (0 → OS default).
  ## `pinnedToCpu` requests CPU affinity where supported (`-1` leaves scheduling to the OS).
  t.dataFn = fn
  t.data = arg
  when defined(windows):
    var dummyThreadId: uint32 = 0'u32
    t.sys = createThread(nil, uint(stackSize), threadProcWrapper, addr(t), 0'u32, dummyThreadId)
    if t.sys.int <= 0:
      raiseOSError(osLastError())
    elif pinnedToCpu >= 0:
      # we cannot undo the thread creation so we cannot raise an error if this fails here:
      discard setThreadAffinityMask(t.sys, uint(1 shl pinnedToCpu))
  elif defined(genode):
    t.sys.initThread(runtimeEnv, stackSize.culonglong,
      threadProcWrapper, addr(t), if pinnedToCpu >= 0: pinnedToCpu else: affinityOffset)
    inc affinityOffset
  elif nativeThreads:
    # One mapping holds both regions, with the stack BELOW the thread-local
    # block, so a stack overflow runs off the low end into unmapped memory and
    # faults — rather than quietly rewriting this thread's own thread-locals.
    let stackBytes = if stackSize > 0: (stackSize + 4095) and not 4095
                     else: NativeStackBytes
    # How big one thread's `{.threadvar.}`s are is a LINK-time fact: nifasm lays
    # every module's into one block and writes the total into the cell this
    # points at (`TlsSize` in `src/lib/intrinsics.nim`). The main thread's block
    # is `.bss`, i.e. zeroed, and a fresh anonymous mapping is zeroed too — so
    # what a thread starts with is the same either way.
    # `max(…, 16)`: a program whose thread-locals were all eliminated reports 0,
    # and the block still needs its self-pointer slot to be inside the mapping.
    let tlsBytes = max((cast[ptr int](tlsSizeCell())[] + 15) and not 15, 16)
    let mem = mmap(nil, csize_t(stackBytes + tlsBytes), PROT_READ or PROT_WRITE,
                   MAP_PRIVATE or MAP_ANONYMOUS or MapStack, -1.cint, 0)
    if mmapFailed(mem):
      raiseOSError(osLastError())
    t.stackMem = mem
    t.stackBytes = stackBytes + tlsBytes
    let base = cast[uint](mem)
    # The child's `ret` pops this word and jumps to it, leaving rsp 8 bytes
    # higher — so `childEntry` must find rsp ≡ 8 (mod 16), the alignment a `call`
    # would have left it, which is what the 16-aligned `sp` below buys.
    let sp = (base + uint(stackBytes)) and not 15'u
    cast[ptr uint](sp - 16)[] = procBits(childEntry)
    # Offset 0 of a thread-local block holds the block's own address: x86-64 can
    # load through FS but cannot `lea` against it, so `&threadvar` is compiled as
    # `FS:[0] + offset` and this is the `FS:[0]`. The main thread's is written by
    # the entry stub nifasm synthesizes; this thread's is ours to write, and
    # without it every thread-local aggregate here would alias the main thread's.
    let tlsBase = base + uint(stackBytes)
    cast[ptr uint](tlsBase)[] = tlsBase
    let tid = cloneRaw(ThreadCloneFlags, cast[pointer](sp - 16),
                       addr t.sys, addr t.sys,
                       cast[pointer](base + uint(stackBytes)), addr t)
    if tid < 0:
      discard munmap(mem, csize_t(t.stackBytes))
      t.stackMem = nil
      t.stackBytes = 0
      raiseOSError(osLastError())
    # `sched_setaffinity` takes a TID, not a handle, which is why this is done
    # here and not in the child: `clone` has already returned the child's, and a
    # tid cannot be recycled while its thread is alive. (glibc's
    # `pthread_setaffinity_np` is this same syscall behind a handle lookup.)
    if pinnedToCpu >= 0:
      var s {.noinit.}: CpuSet
      cpusetZero(s)
      cpusetIncl(pinnedToCpu.cint, s)
      # We cannot undo the thread creation, so a failure here is not an error:
      # the thread runs, just wherever the scheduler likes.
      discard schedSetaffinity(tid.cint, csize_t(sizeof(s)), addr s)
  else:
    var a {.noinit.}: Pthread_attr
    if pthread_attr_init(a) != 0:
      raiseOSError(osLastError())
    if stackSize > 0:
      discard pthread_attr_setstacksize(a, stackSize)
    if pthread_create(t.sys, a, threadProcWrapper, addr(t)) != 0:
      raiseOSError(osLastError())
    discard pthread_attr_destroy(a)
    when not defined(macosx):
      if pinnedToCpu >= 0:
        var s {.noinit.}: CpuSet
        cpusetZero(s)
        cpusetIncl(pinnedToCpu.cint, s)
        setAffinity(t.sys, csize_t(sizeof(s)), s)

proc join*(t: var RawThread) =
  ## Waits for the thread `t` to finish.
  when defined(windows):
    discard waitForSingleObject(t.sys, -1'i32)
  elif defined(genode):
    joinThread(t.sys)
  elif nativeThreads:
    # `t.sys` is the `CLONE_CHILD_CLEARTID` word: the kernel zeroes it and wakes
    # the futex on it as the thread dies. So the wait is the word itself — no
    # handle, no bookkeeping, and no window in which a finished thread has not
    # yet been noticed. FUTEX_WAIT returns spuriously, hence the loop.
    while true:
      let tid = atomicLoad(t.sys, moAcquire)
      if tid == 0: break
      discard futex(cast[ptr uint32](addr t.sys), FutexWait, clong(tid), nil)
    # The kernel clears the word in `mm_release`, after the thread has stopped
    # touching user memory — which is what makes it safe to take its stack back
    # here rather than leaking one mapping per thread.
    if t.stackMem != nil:
      discard munmap(t.stackMem, csize_t(t.stackBytes))
      t.stackMem = nil
      t.stackBytes = 0
  else:
    discard pthread_join(t.sys, nil)


## ------------- Thread ID retrieval ----------------------------

# we need to cache current threadId to not perform syscall all the time
var threadId {.threadvar.}: int

when defined(windows):
  proc getCurrentThreadId(): int32 {.
    stdcall, dynlib: "kernel32", importc: "GetCurrentThreadId".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(getCurrentThreadId())
    result = threadId

elif defined(linux) and defined(nimNoLibc):
  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread. `gettid` by name: arkham
    ## lowers it to the raw syscall, and there is no libc `syscall()` to route
    ## through (nor could arkham model its number-as-first-argument shape).
    if threadId == 0:
      threadId = int(gettidSyscall())
    result = threadId

elif defined(linux):
  proc syscall(arg: clong): clong {.varargs, importc: "syscall".}
  const NR_gettid = (
    when defined(amd64): clong(186)
    elif defined(i386): clong(224)
    else: clong(178))  # arm64 (and every asm-generic unistd arch)

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(syscall(NR_gettid))
    result = threadId

elif defined(dragonfly):
  proc lwp_gettid(): int32 {.importc, header: "unistd.h".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(lwp_gettid())
    result = threadId

elif defined(openbsd):
  proc getthrid(): int32 {.importc: "getthrid", header: "<unistd.h>".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(getthrid())
    result = threadId

elif defined(netbsd):
  proc lwp_self(): int32 {.importc: "_lwp_self", header: "<lwp.h>".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(lwp_self())
    result = threadId

elif defined(freebsd):
  when defined(amd64) or defined(i386):
    const SYS_thr_self = 432
  else:
    var SYS_thr_self {.importc:"SYS_thr_self", header:"<sys/syscall.h>".}: cint

  when defined(cpu64):
    type
      Off {.importc: "off_t", header: "<sys/types.h>".} = int64
      Quad {.importc: "quad_t", header: "<sys/types.h>".} = int64
    proc syscall(arg: Quad): Off {.varargs, importc: "__syscall", header: "<unistd.h>".}
  else:
    proc syscall(arg: cint): cint {.varargs, importc: "syscall", header: "<unistd.h>".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    var tid = when defined(cpu64): Off(0) else: cint(0)
    if threadId == 0:
      discard syscall(SYS_thr_self, addr tid)
      threadId = int(tid)
    result = threadId

elif defined(macosx):
  proc pthread_threadid_np(thread: SysThread, thread_id: ptr uint64): cint {.
    importc: "pthread_threadid_np".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      var tid = 0'u64
      discard pthread_threadid_np(SysThread(0), addr tid)
      threadId = int(tid)
    result = threadId

elif defined(solaris):
  type thread_t {.importc: "thread_t", header: "<thread.h>".} = distinct int
  proc thr_self(): thread_t {.importc, header: "<thread.h>".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(thr_self())
    result = threadId

elif defined(haiku):
  type thr_id {.importc: "thread_id", header: "<OS.h>".} = distinct int32
  proc find_thread(name: cstring): thr_id {.importc, header: "<OS.h>".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(find_thread(nil))
    result = threadId

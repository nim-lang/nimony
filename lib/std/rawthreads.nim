# Thread module for Nimony

import std/oserrors

const
  schedh = "<sched.h>"
  pthreadh = "<pthread.h>"

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
    stdcall, importc: "CreateThread", header: "<Windows.h>".}

  proc winSuspendThread(hThread: SysThread): int32 {.
    stdcall, header: "<Windows.h>", importc: "SuspendThread".}

  proc winResumeThread(hThread: SysThread): int32 {.
    stdcall, header: "<Windows.h>", importc: "ResumeThread".}

  proc waitForSingleObject(hHandle: SysThread, dwMilliseconds: int32): int32 {.
    stdcall, header: "<Windows.h>", importc: "WaitForSingleObject".}

  proc waitForMultipleObjects*(nCount: int32,
                              lpHandles: ptr SysThread,
                              bWaitAll: int32,
                              dwMilliseconds: int32): int32 {.
    stdcall, header: "<Windows.h>", importc: "WaitForMultipleObjects".}

  proc terminateThread*(hThread: SysThread, dwExitCode: int32): int32 {.
    stdcall, header: "<Windows.h>", importc: "TerminateThread".}

  proc setThreadAffinityMask*(hThread: SysThread, dwThreadAffinityMask: uint): uint {.
    importc: "SetThreadAffinityMask", stdcall, header: "<windows.h>".}

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


else:
  when not (defined(macosx) or defined(haiku)):
    {.passL: "-pthread".}

  when not defined(haiku):
    {.passC: "-pthread".}

  when not declared(Time):
    when defined(linux):
      type Time = clong
    else:
      type Time = int

  when (defined(linux) or defined(nintendoswitch)) and defined(amd64):
    type
      SysThread {.importc: "pthread_t",
                  header: "<sys/types.h>" .} = distinct culong
      Pthread_attr {.importc: "pthread_attr_t",
                    header: "<sys/types.h>".} = object
  elif defined(openbsd) and defined(amd64):
    type
      SysThread {.importc: "pthread_t", header: "<pthread.h>".} = object
      Pthread_attr {.importc: "pthread_attr_t",
                       header: "<pthread.h>".} = object
  else:
    type
      SysThread {.importc: "pthread_t", header: "<sys/types.h>".} = int
      Pthread_attr {.importc: "pthread_attr_t",
                       header: "<sys/types.h>".} = object
  type
    Timespec {.importc: "struct timespec", header: "<time.h>".} = object
      tv_sec: Time
      tv_nsec: clong

  proc pthread_attr_init(a1: var Pthread_attr): cint {.
    importc, header: pthreadh.}
  proc pthread_attr_setstack(a1: ptr Pthread_attr, a2: pointer, a3: int): cint {.
    importc, header: pthreadh.}
  proc pthread_attr_setstacksize(a1: var Pthread_attr, a2: int): cint {.
    importc, header: pthreadh.}
  proc pthread_attr_destroy(a1: var Pthread_attr): cint {.
    importc, header: pthreadh.}

  proc pthread_create(a1: var SysThread, a2: var Pthread_attr,
            a3: proc (x: pointer): pointer {.noconv.},
            a4: pointer): cint {.importc: "pthread_create",
            header: pthreadh.}
  proc pthread_join(a1: SysThread, a2: ptr pointer): cint {.
    importc, header: pthreadh.}

  proc pthread_cancel(a1: SysThread): cint {.
    importc: "pthread_cancel", header: pthreadh.}

when defined(posix) and not defined(macosx):
  type CpuSet {.importc: "cpu_set_t", header: schedh.} = object

  proc cpusetZero(s: var CpuSet) {.importc: "CPU_ZERO", header: schedh.}
  proc cpusetIncl(cpu: cint; s: var CpuSet) {.
    importc: "CPU_SET", header: schedh.}

  when defined(android):
    # libc of android doesn't implement pthread_setaffinity_np,
    # it exposes pthread_gettid_np though, so we can use that in combination
    # with sched_setaffinity to set the thread affinity.
    type Pid {.importc: "pid_t", header: "<sys/types.h>".} = int32 # From posix_other.nim

    proc setAffinityTID(tid: Pid; setsize: csize_t; s: var CpuSet) {.
      importc: "sched_setaffinity", header: schedh.}

    proc pthread_gettid_np(thread: SysThread): Pid {.
      importc: "pthread_gettid_np", header: pthreadh.}

    proc setAffinity(thread: SysThread; setsize: csize_t; s: var CpuSet) =
      setAffinityTID(pthread_gettid_np(thread), setsize, s)
  else:
    proc setAffinity(thread: SysThread; setsize: csize_t; s: var CpuSet) {.
      importc: "pthread_setaffinity_np", header: pthreadh.}


type
  RawThread* = object
    sys*: SysThread
    dataFn: proc (arg: pointer) {.nimcall.}
    data: pointer

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
else:
  proc threadProcWrapper(closure: pointer): pointer {.noconv.} =
    result = nil
    nimThreadProcWrapperBody(closure)

when defined(genode):
  var affinityOffset: cuint = 1
    ## CPU affinity offset for next thread, safe to roll-over.

proc create*(t {.noinit.}: out RawThread; fn: proc (arg: pointer) {.nimcall.}; arg: pointer;
            stackSize = 0; pinnedToCpu = -1) {.raises.} =
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
  else:
    discard pthread_join(t.sys, nil)


## ------------- Thread ID retrieval ----------------------------

# we need to cache current threadId to not perform syscall all the time
var threadId {.threadvar.}: int

when defined(windows):
  proc getCurrentThreadId(): int32 {.
    stdcall, header: "<Windows.h>", importc: "GetCurrentThreadId".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      threadId = int(getCurrentThreadId())
    result = threadId

elif defined(linux):
  proc syscall(arg: clong): clong {.varargs, importc: "syscall", header: "<unistd.h>".}
  when defined(amd64):
    const NR_gettid = clong(186)
  else:
    var NR_gettid {.importc: "__NR_gettid", header: "<sys/syscall.h>".}: clong

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
    importc: "pthread_threadid_np", header: "<pthread.h>".}

  proc getThreadId*(): int =
    ## Gets the ID of the currently running thread.
    if threadId == 0:
      var tid = 0'u64
      discard pthread_threadid_np(0, addr tid)
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

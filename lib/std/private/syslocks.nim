#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

# Low level system locks and condition vars.

{.feature: "lenientnils".}

# {.push stackTrace: off.}

when defined(windows):
  type
    Handle = int

    SysLock* {.importc: "CRITICAL_SECTION",
              header: "<windows.h>", final, pure, byref.} = object # CRITICAL_SECTION in WinApi
      DebugInfo {.exportc.} : nil pointer
      LockCount {.exportc.} : int32
      RecursionCount {.exportc.} : int32
      OwningThread {.exportc.} : int
      LockSemaphore {.exportc.} : int
      SpinCount {.exportc.} : int

    SysCond* {.importc: "RTL_CONDITION_VARIABLE", header: "<windows.h>", byref.} = object
      thePtr {.importc: "Ptr".} : Handle

  func initSysLock*(L: var SysLock) {.importc: "InitializeCriticalSection",
                                     header: "<windows.h>".}
    ## Initializes the lock `L`.

  func tryAcquireSysAux(L: var SysLock): int32 {.importc: "TryEnterCriticalSection",
                                                 header: "<windows.h>".}
    ## Tries to acquire the lock `L`.

  func tryAcquireSys*(L: var SysLock): bool {.inline.} =
    result = tryAcquireSysAux(L) != 0'i32

  func acquireSys*(L: var SysLock) {.importc: "EnterCriticalSection",
                                    header: "<windows.h>".}
    ## Acquires the lock `L`.

  func releaseSys*(L: var SysLock) {.importc: "LeaveCriticalSection",
                                    header: "<windows.h>".}
    ## Releases the lock `L`.

  func deinitSys*(L: SysLock) {.importc: "DeleteCriticalSection",
                                   header: "<windows.h>".}

  func initializeConditionVariable(
    conditionVariable: var SysCond
  ) {.stdcall, noSideEffect, header: "<windows.h>", importc: "InitializeConditionVariable".}

  func sleepConditionVariableCS(
    conditionVariable: var SysCond,
    PCRITICAL_SECTION: var SysLock,
    dwMilliseconds: int
  ): int32 {.stdcall, noSideEffect, header: "<windows.h>", importc: "SleepConditionVariableCS".}


  func signalSysCond*(hEvent: var SysCond) {.stdcall, noSideEffect,
    header: "<windows.h>", importc: "WakeConditionVariable".}

  func broadcastSysCond*(hEvent: var SysCond) {.stdcall, noSideEffect,
    header: "<windows.h>", importc: "WakeAllConditionVariable".}

  func initSysCond*(cond: var SysCond) {.inline.} =
    initializeConditionVariable(cond)
  func deinitSysCond*(cond: SysCond) {.inline.} =
    discard
  func waitSysCond*(cond: var SysCond, lock: var SysLock) =
    discard sleepConditionVariableCS(cond, lock, -1'i32)

elif defined(genode):
  const
    Header = "genode_cpp/syslocks.h"
  type
    SysLock* {.importcpp: "Nim::SysLock", pure, final,
              header: Header.} = object
    SysCond* {.importcpp: "Nim::SysCond", pure, final,
              header: Header.} = object

  func initSysLock*(L: var SysLock) = discard
  func deinitSys*(L: SysLock) = discard
  func acquireSys*(L: var SysLock) {.noSideEffect, importcpp.}
  func tryAcquireSys*(L: var SysLock): bool {.noSideEffect, importcpp.}
  func releaseSys*(L: var SysLock) {.noSideEffect, importcpp.}

  func initSysCond*(L: var SysCond) = discard
  func deinitSysCond*(L: SysCond) = discard
  func waitSysCond*(cond: var SysCond, lock: var SysLock) {.
    noSideEffect, importcpp.}
  func signalSysCond*(cond: var SysCond) {.
    noSideEffect, importcpp.}
  func broadcastSysCond*(cond: var SysCond) {.
    noSideEffect, importcpp.}

elif defined(nimNativeIo):
  # Freestanding, libc-header-free locks built directly on the kernel futex
  # primitive instead of pthread. `SysLock`/`SysCond` are plain 32-bit words, so
  # there is no opaque libc struct to `importc` and no fixed ABI layout to track.
  # The uncontended paths are pure userspace atomics; only contention enters the
  # kernel — matching how this build already binds raw syscalls in `posix.nim`.
  #
  # Mutex: Drepper's 3-state futex mutex ("Futexes Are Tricky", mutex3).
  # Condvar: the sequence-counter algorithm (generation bumped per signal; the
  # futex compare-and-wait closes the release/wait race, spurious wakeups are
  # permitted by condvar semantics).
  import std / atomics

  type
    SysLock* = object
      state: uint32  ## 0 = free, 1 = locked (no waiters), 2 = locked (waiters)
      recursive: bool  ## set via `setSysLockType` for RLock
      owner: int       ## owning thread token while held (0 = none); recursive only
      count: int       ## recursion depth; recursive only
    SysCond* = object
      gen: uint32    ## generation counter, bumped on every signal/broadcast
    SysLockAttr* = object
      recursive: bool
    SysCondAttr* = object
    SysLockType* = distinct cint

  const SysLockType_Reentrant* = SysLockType(1)

  # A thread-local byte gives every thread a unique, non-zero address to use as
  # its identity token — no `gettid` syscall and no pthread dependency.
  var tlsSelf {.threadvar.}: byte
  proc selfToken(): int {.inline.} = cast[int](addr tlsSelf)

  when defined(linux):
    proc syscall(n: clong): clong {.varargs, importc: "syscall", sideEffect.}
    when defined(amd64):
      const NR_futex = clong(202)
    else:
      const NR_futex = clong(98)  # arm64 and other modern 64-bit Linux ABIs
    const
      FUTEX_WAIT_PRIVATE = clong(128)  # FUTEX_WAIT or FUTEX_PRIVATE_FLAG
      FUTEX_WAKE_PRIVATE = clong(129)  # FUTEX_WAKE or FUTEX_PRIVATE_FLAG

    proc futexWait(p: var uint32; expected: uint32) {.inline.} =
      # Blocks while `p == expected`; returns spuriously, callers re-check.
      discard syscall(NR_futex, addr p, FUTEX_WAIT_PRIVATE, clong(expected), nil)
    proc futexWake(p: var uint32; all: bool) {.inline.} =
      let count = if all: clong(high(int32)) else: clong(1)
      discard syscall(NR_futex, addr p, FUTEX_WAKE_PRIVATE, count)

  elif defined(osx):
    # Darwin's `__ulock_*` (libsystem_kernel) — the same primitive libdispatch
    # and libc++ use. Bare header-free `importc`: they are real libSystem symbols
    # so they link with no `<sys/ulock.h>` (which is private anyway). `ULF_NO_ERRNO`
    # makes them return `-errno` directly, so no libc `errno` TLS is touched.
    const
      UL_COMPARE_AND_WAIT = uint32(1)
      ULF_WAKE_ALL = uint32(0x00000100)
      ULF_NO_ERRNO = uint32(0x01000000)
    proc ulock_wait(operation: uint32; ad: pointer; value: uint64;
                    timeout: uint32): cint {.importc: "__ulock_wait", sideEffect.}
    proc ulock_wake(operation: uint32; ad: pointer; wakeValue: uint64): cint {.
      importc: "__ulock_wake", sideEffect.}

    proc futexWait(p: var uint32; expected: uint32) {.inline.} =
      discard ulock_wait(UL_COMPARE_AND_WAIT or ULF_NO_ERRNO, addr p,
                         uint64(expected), 0'u32)
    proc futexWake(p: var uint32; all: bool) {.inline.} =
      let op = if all: UL_COMPARE_AND_WAIT or ULF_WAKE_ALL or ULF_NO_ERRNO
               else: UL_COMPARE_AND_WAIT or ULF_NO_ERRNO
      discard ulock_wake(op, addr p, 0'u64)

  # ---- mutex3 core (non-reentrant) ----
  proc lockSlow(L: var SysLock; c: var uint32) {.inline.} =
    if c != 2'u32:
      c = atomicExchange(L.state, 2'u32, moAcquire)
    while c != 0'u32:
      futexWait(L.state, 2'u32)
      c = atomicExchange(L.state, 2'u32, moAcquire)

  proc rawAcquire(L: var SysLock) {.inline.} =
    var c = 0'u32
    if not atomicCompareExchange(L.state, c, 1'u32, moAcquire, moRelaxed):
      lockSlow(L, c)

  proc rawTryAcquire(L: var SysLock): bool {.inline.} =
    var expected = 0'u32
    result = atomicCompareExchange(L.state, expected, 1'u32, moAcquire, moRelaxed)

  proc rawRelease(L: var SysLock) {.inline.} =
    if atomicFetchSub(L.state, 1'u32, moRelease) != 1'u32:
      atomicStore(L.state, 0'u32, moRelease)
      futexWake(L.state, false)

  # ---- public lock API (adds reentrancy for RLock) ----
  # `std/rlocks` declares its wrappers as `func`, so these must be `noSideEffect`
  # like the pthread branch's `importc` ops; the underlying syscalls/atomics are
  # cast accordingly (the same convention `posix.nim` uses for raw syscalls).
  proc initSysLock*(L: var SysLock, attr: ptr SysLockAttr = nil) {.noSideEffect.} =
    L.state = 0'u32
    L.owner = 0
    L.count = 0
    L.recursive = attr != nil and attr.recursive

  proc deinitSys*(L: SysLock) {.noSideEffect.} = discard

  proc acquireSys*(L: var SysLock) {.noSideEffect.} =
    {.cast(noSideEffect).}:
      if L.recursive:
        let me = selfToken()
        if atomicLoad(L.owner, moRelaxed) == me:
          inc L.count
          return
        rawAcquire(L)
        atomicStore(L.owner, me, moRelaxed)
        L.count = 1
      else:
        rawAcquire(L)

  proc tryAcquireSys*(L: var SysLock): bool {.noSideEffect.} =
    {.cast(noSideEffect).}:
      if L.recursive:
        let me = selfToken()
        if atomicLoad(L.owner, moRelaxed) == me:
          inc L.count
          return true
        if rawTryAcquire(L):
          atomicStore(L.owner, me, moRelaxed)
          L.count = 1
          return true
        return false
      else:
        result = rawTryAcquire(L)

  proc releaseSys*(L: var SysLock) {.noSideEffect.} =
    {.cast(noSideEffect).}:
      if L.recursive:
        dec L.count
        if L.count > 0: return
        atomicStore(L.owner, 0, moRelaxed)  # clear owner before the futex word
        rawRelease(L)
      else:
        rawRelease(L)

  # ---- rlocks attribute machinery (used by std/rlocks under `posix`) ----
  proc initSysLockAttr*(a: var SysLockAttr) {.noSideEffect.} = a.recursive = false
  proc setSysLockType*(a: var SysLockAttr, t: SysLockType) {.noSideEffect.} =
    a.recursive = cint(t) == cint(SysLockType_Reentrant)

  # ---- condition variables (sequence-counter futex) ----
  proc initSysCond*(cond: var SysCond, cond_attr: ptr SysCondAttr = nil) =
    cond.gen = 0'u32
  proc deinitSysCond*(cond: SysCond) = discard

  proc waitSysCond*(cond: var SysCond, lock: var SysLock) =
    let observed = atomicLoad(cond.gen, moAcquire)
    releaseSys(lock)
    futexWait(cond.gen, observed)
    acquireSys(lock)

  proc signalSysCond*(cond: var SysCond) =
    discard atomicFetchAdd(cond.gen, 1'u32, moRelease)
    futexWake(cond.gen, false)

  proc broadcastSysCond*(cond: var SysCond) =
    discard atomicFetchAdd(cond.gen, 1'u32, moRelease)
    futexWake(cond.gen, true)

else:
  type
    SysLockObj {.importc: "pthread_mutex_t", pure, final,
               header: """#include <sys/types.h>
                          #include <pthread.h>""", byref.} = object
      # when defined(linux) and defined(amd64):
      #   abi: array[40 div sizeof(clong), clong]

    SysLockAttr* {.importc: "pthread_mutexattr_t", pure, final
               header: """#include <sys/types.h>
                          #include <pthread.h>""".} = object
      # when defined(linux) and defined(amd64):
      #   abi: array[4 div sizeof(cint), cint]  # actually a cint

    SysCondObj {.importc: "pthread_cond_t", pure, final,
               header: """#include <sys/types.h>
                          #include <pthread.h>""", byref.} = object
      # when defined(linux) and defined(amd64):
      #   abi: array[48 div sizeof(clonglong), clonglong]

    SysCondAttr {.importc: "pthread_condattr_t", pure, final
               header: """#include <sys/types.h>
                          #include <pthread.h>""".} = object
      # when defined(linux) and defined(amd64):
      #   abi: array[4 div sizeof(cint), cint]  # actually a cint

    SysLockType = distinct cint

  func initSysLockAux(L: var SysLockObj, attr: ptr SysLockAttr) {.
    importc: "pthread_mutex_init", header: "<pthread.h>", noSideEffect.}
  func deinitSysAux(L: SysLockObj) {.noSideEffect,
    importc: "pthread_mutex_destroy", header: "<pthread.h>".}

  func acquireSysAux(L: var SysLockObj) {.noSideEffect,
    importc: "pthread_mutex_lock", header: "<pthread.h>".}
  func tryAcquireSysAux(L: var SysLockObj): cint {.noSideEffect,
    importc: "pthread_mutex_trylock", header: "<pthread.h>".}

  func releaseSysAux(L: var SysLockObj) {.noSideEffect,
    importc: "pthread_mutex_unlock", header: "<pthread.h>".}

  when defined(ios):
    # iOS will behave badly if sync primitives are moved in memory. In order
    # to prevent this once and for all, we're doing an extra malloc when
    # initializing the primitive.
    type
      SysLock* = ptr SysLockObj
      SysCond* = ptr SysCondObj

    when not declared(c_malloc):
      func c_malloc(size: csize_t): pointer {.
        importc: "malloc", header: "<stdlib.h>".}
      func c_free(p: pointer) {.
        importc: "free", header: "<stdlib.h>".}

    func initSysLock*(L: var SysLock, attr: ptr SysLockAttr = nil) =
      L = cast[SysLock](c_malloc(csize_t(sizeof(SysLockObj))))
      initSysLockAux(L[], attr)

    func deinitSys*(L: SysLock) =
      deinitSysAux(L[])
      c_free(L)

    template acquireSys*(L: var SysLock) =
      acquireSysAux(L[])
    template tryAcquireSys*(L: var SysLock): bool =
      tryAcquireSysAux(L[]) == 0'i32
    template releaseSys*(L: var SysLock) =
      releaseSysAux(L[])
  else:
    type
      SysLock* = SysLockObj
      SysCond* = SysCondObj

    template initSysLock*(L: var SysLock, attr: ptr SysLockAttr = nil) =
      initSysLockAux(L, attr)
    template deinitSys*(L: SysLock) =
      deinitSysAux(L)
    template acquireSys*(L: var SysLock) =
      acquireSysAux(L)
    template tryAcquireSys*(L: var SysLock): bool =
      tryAcquireSysAux(L) == 0'i32
    template releaseSys*(L: var SysLock) =
      releaseSysAux(L)

  # rlocks
  var SysLockType_Reentrant* {.importc: "PTHREAD_MUTEX_RECURSIVE",
    header: "<pthread.h>".}: SysLockType
  func initSysLockAttr*(a: var SysLockAttr) {.
    importc: "pthread_mutexattr_init", header: "<pthread.h>", noSideEffect.}
  func setSysLockType*(a: var SysLockAttr, t: SysLockType) {.
    importc: "pthread_mutexattr_settype", header: "<pthread.h>", noSideEffect.}

  # locks
  func initSysCondAux(cond: var SysCondObj, cond_attr: ptr SysCondAttr = nil) {.
    importc: "pthread_cond_init", header: "<pthread.h>", noSideEffect.}
  func deinitSysCondAux(cond: SysCondObj) {.noSideEffect,
    importc: "pthread_cond_destroy", header: "<pthread.h>".}

  func waitSysCondAux(cond: var SysCondObj, lock: var SysLockObj): cint {.
    importc: "pthread_cond_wait", header: "<pthread.h>", noSideEffect.}
  func signalSysCondAux(cond: var SysCondObj) {.
    importc: "pthread_cond_signal", header: "<pthread.h>", noSideEffect.}
  func broadcastSysCondAux(cond: var SysCondObj) {.
    importc: "pthread_cond_broadcast", header: "<pthread.h>", noSideEffect.}

  when defined(ios):
    func initSysCond*(cond: var SysCond, cond_attr: ptr SysCondAttr = nil) =
      cond = cast[SysCond](c_malloc(csize_t(sizeof(SysCondObj))))
      initSysCondAux(cond[], cond_attr)

    func deinitSysCond*(cond: SysCond) =
      deinitSysCondAux(cond[])
      c_free(cond)

    template waitSysCond*(cond: var SysCond, lock: var SysLock) =
      discard waitSysCondAux(cond[], lock[])
    template signalSysCond*(cond: var SysCond) =
      signalSysCondAux(cond[])
    template broadcastSysCond*(cond: var SysCond) =
      broadcastSysCondAux(cond[])
  else:
    template initSysCond*(cond: var SysCond, cond_attr: ptr SysCondAttr = nil) =
      initSysCondAux(cond, cond_attr)
    template deinitSysCond*(cond: SysCond) =
      deinitSysCondAux(cond)

    template waitSysCond*(cond: var SysCond, lock: var SysLock) =
      discard waitSysCondAux(cond, lock)
    template signalSysCond*(cond: var SysCond) =
      signalSysCondAux(cond)
    template broadcastSysCond*(cond: var SysCond) =
      broadcastSysCondAux(cond)

# {.pop.}

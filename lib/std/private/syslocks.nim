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

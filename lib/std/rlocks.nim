#
#
#            Nim's Runtime Library
#        (c) Copyright 2016 Anatoly Galiulin
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module contains Nim's support for reentrant locks.

import std/private/syslocks

type
  RLock* = SysLock ## Nim lock, re-entrant

func initRLock*(lock: var RLock) {.inline.} =
  ## Initializes the given lock.
  when defined(posix):
    var a: SysLockAttr = default(SysLockAttr)
    initSysLockAttr(a)
    {.cast(noSideEffect).}:
      setSysLockType(a, SysLockType_Reentrant)
    initSysLock(lock, a.addr)
  else:
    initSysLock(lock)

func deinitRLock*(lock: RLock) {.inline.} =
  ## Frees the resources associated with the lock.
  deinitSys(lock)

func tryAcquire*(lock: var RLock): bool {.inline.} =
  ## Tries to acquire the given lock. Returns `true` on success.
  result = tryAcquireSys(lock)

func acquire*(lock: var RLock) {.inline.} =
  ## Acquires the given lock.
  acquireSys(lock)

func release*(lock: var RLock) {.inline.} =
  ## Releases the given lock.
  releaseSys(lock)

# template withRLock*(lock: RLock, code: untyped) =
#   ## Acquires the given lock and then executes the code.
#   acquire(lock)
#   {.locks: [lock].}:
#     try:
#       code
#     finally:
#       release(lock)

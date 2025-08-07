import std/locks
import std/assertions


proc testInitAndDeinitLock() =
  var lock: Lock = default(Lock)
  initLock(lock)
  deinitLock(lock)

proc testTryAcquireAndRelease() =
  var lock: Lock = default(Lock)
  initLock(lock)
  let acquired = tryAcquire(lock)
  if acquired:
    release(lock)
  deinitLock(lock)

proc testAcquireAndRelease() =
  var lock: Lock = default(Lock)
  initLock(lock)
  acquire(lock)
  release(lock)
  deinitLock(lock)

proc testInitAndDeinitCond() =
  var cond: Cond = default(Cond)
  initCond(cond)
  deinitCond(cond)

proc testSignalAndBroadcast() =
  var cond: Cond = default(Cond)
  initCond(cond)
  signal(cond)
  broadcast(cond)
  deinitCond(cond)


testInitAndDeinitLock()
testTryAcquireAndRelease()
testAcquireAndRelease()
testInitAndDeinitCond()
testSignalAndBroadcast()

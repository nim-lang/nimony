import std/rlocks
import std/assertions

var r: RLock = default(RLock)
r.initRLock()
assert r.tryAcquire()
assert r.tryAcquire()
r.release()
r.release()

r.deinitRLock()

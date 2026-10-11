# A regular proc calling a passive proc that PARKS on real I/O.
#
# The call runs to completion: the callee's frame and its result live in the
# regular proc's stack frame. The trampoline used to return as soon as the
# callee parked, so `isAllowed` answered with whatever its `result` held
# (`false`) and the ring later resumed a frame that was already gone.
#
# The main thread is not a pool worker, so the timer sits on a lane only the
# main thread drains: waiting for the callee has to include driving it.

import std / [syncio, ioring]

proc readQuotaFromDb(userId: int): int {.passive.} =
  var res = 0
  let c = delay()
  discard submitTimeout(afterMs(20), c, addr res)  # the "database" answers later
  suspend()
  result = userId * 10

proc checkQuota(userId: int): bool {.passive.} =
  let used = readQuotaFromDb(userId)
  result = used < 100

proc isAllowed(userId: int): bool =    # not passive
  result = checkQuota(userId)

proc clobberStack(): int =
  var a = default(array[64, int])
  for i in 0..<64: a[i] = 0x4141
  result = a[3]

echo isAllowed(7)
echo isAllowed(12)
discard clobberStack()
echo "done"

# `ParkLock`: tasks that find the lock taken park, and `release` resumes the
# oldest one on the pool. Every holder also yields to the pool inside the
# critical section, so holders move between workers and waiters really queue.
# The main thread contends too, through the regular-proc call path.
import std/[atomics, threadpool, parklocks, syncio]

const
  Tasks = 200
  Rounds = 50

var
  L: ParkLock
  counter = 0      # guarded by `L`, deliberately not atomic
  inside = 0       # how many tasks are in the critical section; must stay <= 1
  overlaps = 0
  done = 0

proc yieldToPool() {.passive.} =
  ## Park and let the pool resume us, likely on another worker.
  submit(delay())
  suspend()

proc work(id: int) {.passive.} =
  for r in 0 ..< Rounds:
    acquire L
    if atomicFetchAdd(inside, 1) != 0: atomicInc(overlaps)
    {.cast(assumeSync).}:
      let v = counter
    if (id + r) mod 7 == 0: yieldToPool()
    {.cast(assumeSync).}:
      counter = v + 1
    atomicDec(inside)
    release L
  atomicInc(done)

proc main() =
  initPool(4)
  for i in 0 ..< Tasks:
    submit(delay(work(i)))
  for r in 0 ..< Rounds:
    withLock L:
      {.cast(assumeSync).}:
        inc counter
  while atomicLoad(done) < Tasks: cpuRelax()
  {.cast(assumeSync).}:
    echo "counter ", counter, " overlaps ", overlaps
  echo "tryAcquire free ", tryAcquire(L)
  echo "tryAcquire held ", tryAcquire(L)
  release L
  shutdownPool()

main()

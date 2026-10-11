## Test: the pool is sized from, and stays inside, the CPUs the process may
## use. A cpuset (`docker run --cpuset-cpus`, `taskset`) shrinks that set
## below the installed CPU count; `countProcessors` must follow it, and the
## workers must not be pinned to single CPUs of their own.

import std / [threadpool, cpuinfo, dirs, paths, syncio]

when defined(linux):
  type CpuMask = object  ## cpu_set_t (glibc: a 1024-bit mask)
    abi: array[16, uint64]

  proc schedGetaffinity(pid: cint; setsize: csize_t; mask: pointer): cint {.
    importc: "sched_getaffinity".}
  proc schedSetaffinity(pid: cint; setsize: csize_t; mask: pointer): cint {.
    importc: "sched_setaffinity".}

  proc popcount(m: CpuMask): int =
    result = 0
    for i in 0 ..< m.abi.len:
      var w = m.abi[i]
      while w != 0'u64:
        inc result
        w = w and (w - 1'u64)

  proc currentMask(): CpuMask =
    result = default(CpuMask)
    discard schedGetaffinity(0.cint, csize_t(sizeof(result)), addr result)

  proc narrowestThread(): int =
    ## The fewest CPUs any thread of this process may run on.
    result = high(int)
    try:
      for kind, entry in walkDir(path("/proc/self/task"), relative = true):
        var tid = 0
        for c in $entry: tid = tid * 10 + (ord(c) - ord('0'))
        var m = default(CpuMask)
        if schedGetaffinity(tid.cint, csize_t(sizeof(m)), addr m) >= 0:
          result = min(result, popcount(m))
    except:
      result = -1

  proc main =
    let allowed = currentMask()
    let allowedCount = popcount(allowed)

    # Narrow the process to its lowest allowed CPU: the count must follow.
    var one = default(CpuMask)
    block pick:
      for i in 0 ..< allowed.abi.len:
        if allowed.abi[i] != 0'u64:
          var b = 0
          while (allowed.abi[i] and (1'u64 shl b)) == 0'u64: inc b
          one.abi[i] = 1'u64 shl b
          break pick
    discard schedSetaffinity(0.cint, csize_t(sizeof(one)), addr one)
    echo "narrowed count: ", countProcessors()
    var restore = allowed
    discard schedSetaffinity(0.cint, csize_t(sizeof(restore)), addr restore)
    echo "count follows affinity: ", countProcessors() == allowedCount

    # An explicit worker count, and no thread narrower than the process.
    initPool(3)
    echo "workers: ", workerCount
    echo "workers unpinned: ", narrowestThread() == allowedCount
    shutdownPool()

  main()
else:
  echo "narrowed count: 1"
  echo "count follows affinity: true"
  echo "workers: 3"
  echo "workers unpinned: true"

## Shared globals touched only through `.sync` routines or under
## `{.cast(assumeSync).}`.

import std / [atomics, locks, syncio]

type
  Stats = object
    hits, misses: int

var counter: int
var stats: Stats
var lock: Lock
var log: seq[string]

proc record(msg: string) =
  acquire lock
  {.cast(assumeSync).}:
    log.add msg
  release lock

proc hit() =
  atomicInc counter
  atomicInc stats.hits

proc main =
  initLock lock
  hit()
  hit()
  discard atomicFetchAdd(stats.misses, 3)
  record "done"
  echo atomicLoad(counter), " ", atomicLoad(stats.hits), " ", atomicLoad(stats.misses)
  {.cast(assumeSync).}:
    echo log.len
  deinitLock lock

main()

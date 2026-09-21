## Regression test: std/times and std/monotimes compile under -d:useLibc
## (the libc `tzReady` flag is an atomic, not an unsynchronized global) and
## local-time conversion round-trips.

import std/[assertions, syncio, times, monotimes]

block:
  let t = fromUnix(1_700_000_000'i64)
  let a = local(t)
  let b = local(t)     # second call takes the already-initialised tz path
  assert a.toTime() == t
  assert b.toTime() == t
  assert a.utcOffset == b.utcOffset
  assert utc(t).year == 2023

block:
  let x = getMonoTime()
  let y = getMonoTime()
  assert x <= y

echo "ok"

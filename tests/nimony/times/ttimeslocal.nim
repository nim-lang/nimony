## `local()` follows the process's TZ on a hosted libc build; it must not
## silently degrade to UTC (`nimNativeIo` is the default there and says
## nothing about libc).

import std/[assertions, envvars, syncio, times]

# NZST is UTC+12 with no DST rule, so the answer is fixed all year.
try:
  putEnv("TZ", "NZST-12")
except:
  assert false

let t = fromUnix(1_700_000_000'i64)   # 2023-11-14T22:13:20Z
let dt = t.inZone(local())
assert dt.utcOffset == 12 * 3600
assert not dt.isDst
assert dt.year == 2023
assert dt.month == mNov
assert dt.monthday == 15'i32          # already the 15th in NZ
assert dt.hour == 10'i32
assert dt.minute == 13'i32
assert dt.second == 20'i32
assert toTime(dt) == t

try:
  putEnv("TZ", "UTC0")
except:
  assert false
assert t.inZone(local()).utcOffset == 0

echo "ok"

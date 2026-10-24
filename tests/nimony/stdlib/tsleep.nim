import std/[os, monotimes, assertions, syncio]

proc main =
  let start = getMonoTime()
  sleep(50)
  let elapsedNs = getMonoTime().ticks - start.ticks
  assert elapsedNs >= 50_000_000'i64
  # Negative and zero durations return immediately.
  let t0 = getMonoTime()
  sleep(0)
  sleep(-5)
  assert getMonoTime().ticks - t0.ticks < 1_000_000_000'i64

main()
echo "sleep ok"

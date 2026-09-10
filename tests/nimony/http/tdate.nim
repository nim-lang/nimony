# lib/std/http/httpdate — the one timestamp format HTTP names, both ways.

import std / [http/httpdate, times, assertions, syncio]

proc testWrite =
  ## Landmarks with an independently known weekday, because the weekday is the
  ## one field a formatter can get wrong without the rest looking wrong.
  echo formatHttpDate(fromUnix(784111777'i64))   # the RFC's own example
  echo formatHttpDate(fromUnix(0'i64))           # the epoch: a Thursday
  echo formatHttpDate(fromUnix(1000000000'i64))
  echo formatHttpDate(fromUnix(951782400'i64))   # 29 Feb 2000: a leap day
  echo formatHttpDate(fromUnix(2147483647'i64))  # the 32-bit end of time
  echo formatHttpDate(fromUnix(-1'i64))          # one second before the epoch

  var buf = default(array[HttpDateLen, char])
  assert writeHttpDate(buf, fromUnix(784111777'i64)) == HttpDateLen
  var tooSmall = default(array[HttpDateLen - 1, char])
  assert writeHttpDate(tooSmall, fromUnix(0'i64)) == 0,
    "a short buffer must be refused, not partly filled"

proc reparse(x: string): string =
  ## Every accepted spelling normalizes to the one HTTP writes, so a round
  ## trip through the canonical form is the whole test.
  var t = default(Time)
  if parseHttpDate(toOpenArray(x, 0, x.len - 1), t): formatHttpDate(t)
  else: "REJECTED"

proc testRead =
  # The three formats RFC 9110 §5.6.7 requires a recipient to accept, all
  # naming the same instant.
  echo reparse("Sun, 06 Nov 1994 08:49:37 GMT")   # IMF-fixdate
  echo reparse("Sunday, 06-Nov-94 08:49:37 GMT")  # RFC 850, two-digit year
  echo reparse("Sun Nov  6 08:49:37 1994")        # asctime, space-padded day
  echo reparse("Sun Nov 16 08:49:37 1994")        # asctime, two-digit day
  echo reparse("  Sun, 06 Nov 1994 08:49:37 GMT ")

  # The claimed weekday is redundant and is recomputed, never trusted: two
  # hops that disagree about it must still agree about the instant.
  echo reparse("Mon, 06 Nov 1994 08:49:37 GMT")

  # Rejections. Each is a different way to be wrong, and none of them may be
  # read as some other date.
  echo reparse("Sun, 06 Nov 1994 08:49:37 UTC")   # not GMT
  echo reparse("Sun, 32 Nov 1994 08:49:37 GMT")   # no such day
  echo reparse("Sun, 29 Feb 2001 00:00:00 GMT")   # not a leap year
  echo reparse("Sun, 06 Xxx 1994 08:49:37 GMT")   # no such month
  echo reparse("Sun, 06 Nov 1994 08:49:99 GMT")   # no such second
  echo reparse("Sun, 06 Nov 1994 25:49:37 GMT")   # no such hour
  echo reparse("Sun, 6 Nov 1994 08:49:37 GMT")    # day not zero-padded
  echo reparse("garbage")
  echo reparse("")

  # 29 Feb 2000 is a leap day and 2000 is a leap year the century rule alone
  # would deny.
  echo reparse("Tue, 29 Feb 2000 00:00:00 GMT")

proc testNow =
  ## The cache must be invisible: the second call is served from it and has to
  ## produce the same bytes as the first.
  var a = default(array[HttpDateLen, char])
  var b = default(array[HttpDateLen, char])
  assert nowHttpDate(a) == HttpDateLen
  assert nowHttpDate(b) == HttpDateLen
  for i in 0..<HttpDateLen:
    assert a[i] == b[i], "the cached date disagrees with the one that filled it"
  var t = default(Time)
  assert parseHttpDate(toOpenArray(a, 0, HttpDateLen - 1), t),
    "what we write must be what we accept"
  var tooSmall = default(array[4, char])
  assert nowHttpDate(tooSmall) == 0

testWrite()
testRead()
testNow()

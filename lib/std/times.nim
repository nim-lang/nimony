#
#
#            Nim's Runtime Library
#        (c) Copyright 2017 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## The `std/times` module provides basic support for working with time.
##
## This is a minimal implementation for Nimony: it covers Unix-epoch based
## points in time with nanosecond resolution, simple durations, and a
## calendar breakdown for UTC. For monotonic timestamps suitable for
## measuring durations, use `std/monotimes <monotimes.html>`_.

import strutils

const
  secondsInMin = 60
  secondsInHour = 60 * 60
  secondsInDay = 24 * 60 * 60
  minutesInHour = 60
  rateDiff = 10000000'i64     # 100-ns intervals per second
  unixEpochSeconds = 0'i64

type
  Month* = enum ## Represents a month. The enum starts at 1.
    mJan = 1, mFeb, mMar, mApr, mMay, mJun,
    mJul, mAug, mSep, mOct, mNov, mDec

  WeekDay* = enum ## Represents a weekday.
    dMon, dTue, dWed, dThu, dFri, dSat, dSun

  Time* = object  ## Represents a point in time with nanosecond resolution
                  ## stored as seconds since the Unix epoch (1970-01-01 UTC).
    seconds: int64
    nanosecond: int32

  Duration* = object  ## Represents a fixed duration of time.
    seconds: int64
    nanosecond: int32

  DateTime* = object  ## Represents a calendar date and time of day in UTC.
    year*: int
    month*: Month
    monthday*: int32     ## Day of the month, 1..31.
    hour*: int32         ## 0..23
    minute*: int32       ## 0..59
    second*: int32       ## 0..59
    nanosecond*: int32   ## 0..999_999_999
    weekday*: WeekDay
    yearday*: int32      ## 0..365

# --- basic helpers for Time / Duration ---

proc initTime*(seconds: int64; nanoseconds: int64 = 0): Time =
  ## Creates a new `Time` from a Unix timestamp.
  const nanosPerSec = 1_000_000_000'i64
  var s = seconds + nanoseconds div nanosPerSec
  var n = nanoseconds mod nanosPerSec
  if n < 0:
    n = n + nanosPerSec
    s = s - 1
  result = Time(seconds: s, nanosecond: int32(n))

proc fromUnix*(unix: int64): Time =
  ## Convert a Unix timestamp (seconds since 1970-01-01 UTC) to a `Time`.
  initTime(unix, 0)

proc toUnix*(t: Time): int64 =
  ## Converts `t` to a Unix timestamp.
  t.seconds

proc seconds*(t: Time): int64 {.inline.} = t.seconds
proc nanosecond*(t: Time): int32 {.inline.} = t.nanosecond

proc initDuration*(seconds: int64 = 0; nanoseconds: int64 = 0;
                   milliseconds: int64 = 0; microseconds: int64 = 0;
                   minutes: int64 = 0; hours: int64 = 0;
                   days: int64 = 0; weeks: int64 = 0): Duration =
  ## Creates a new `Duration`.
  const nanosPerSec = 1_000_000_000'i64
  var totalNanos = nanoseconds + microseconds * 1000'i64 +
                   milliseconds * 1_000_000'i64
  var totalSeconds = seconds +
                     minutes * int64(secondsInMin) +
                     hours * int64(secondsInHour) +
                     days * int64(secondsInDay) +
                     weeks * int64(secondsInDay) * 7'i64
  totalSeconds = totalSeconds + totalNanos div nanosPerSec
  totalNanos = totalNanos mod nanosPerSec
  if totalNanos < 0:
    totalNanos = totalNanos + nanosPerSec
    totalSeconds = totalSeconds - 1
  result = Duration(seconds: totalSeconds, nanosecond: int32(totalNanos))

proc inSeconds*(d: Duration): int64 = d.seconds
proc inMilliseconds*(d: Duration): int64 =
  d.seconds * 1_000'i64 + int64(d.nanosecond) div 1_000_000'i64
proc inMicroseconds*(d: Duration): int64 =
  d.seconds * 1_000_000'i64 + int64(d.nanosecond) div 1_000'i64
proc inNanoseconds*(d: Duration): int64 =
  d.seconds * 1_000_000_000'i64 + int64(d.nanosecond)

proc `==`*(a, b: Time): bool =
  a.seconds == b.seconds and a.nanosecond == b.nanosecond
proc `<`*(a, b: Time): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond < b.nanosecond)
proc `<=`*(a, b: Time): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond <= b.nanosecond)

proc `==`*(a, b: Duration): bool =
  a.seconds == b.seconds and a.nanosecond == b.nanosecond
proc `<`*(a, b: Duration): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond < b.nanosecond)
proc `<=`*(a, b: Duration): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond <= b.nanosecond)

proc `-`*(a, b: Time): Duration =
  ## Returns the duration between two times.
  var s = a.seconds - b.seconds
  var n = int64(a.nanosecond) - int64(b.nanosecond)
  if n < 0:
    n = n + 1_000_000_000'i64
    s = s - 1
  result = Duration(seconds: s, nanosecond: int32(n))

proc `+`*(t: Time; d: Duration): Time =
  ## Adds a duration to a time.
  var s = t.seconds + d.seconds
  var n = int64(t.nanosecond) + int64(d.nanosecond)
  if n >= 1_000_000_000'i64:
    n = n - 1_000_000_000'i64
    s = s + 1
  result = Time(seconds: s, nanosecond: int32(n))

proc `-`*(t: Time; d: Duration): Time =
  ## Subtracts a duration from a time.
  var s = t.seconds - d.seconds
  var n = int64(t.nanosecond) - int64(d.nanosecond)
  if n < 0:
    n = n + 1_000_000_000'i64
    s = s - 1
  result = Time(seconds: s, nanosecond: int32(n))

# --- Current time via C library ---

when defined(posix):
  import posix/posix

  proc getTime*(): Time {.tags: [TimeEffect].} =
    ## Gets the current time as a `Time` with up to nanosecond resolution.
    var ts: Timespec = default(Timespec)
    discard clock_gettime(CLOCK_REALTIME, ts)
    result = Time(seconds: int64(ts.tv_sec), nanosecond: int32(ts.tv_nsec))

elif defined(windows):
  import windows/winlean

  const winEpochDiff: int64 = 116444736000000000'i64

  proc getTime*(): Time {.tags: [TimeEffect].} =
    ## Gets the current time as a `Time` with up to nanosecond resolution.
    var ft: FILETIME = default(FILETIME)
    getSystemTimeAsFileTime(ft)
    let hundredNs = int64(cast[uint32](ft.dwLowDateTime)) or
                    (int64(cast[uint32](ft.dwHighDateTime)) shl 32)
    let since1970 = hundredNs - winEpochDiff
    let secs = since1970 div rateDiff
    let hns = since1970 mod rateDiff
    result = Time(seconds: secs, nanosecond: int32(hns * 100'i64))

# --- Civil date math (UTC) ---
# Based on Howard Hinnant's "chrono-compatible low-level date algorithms".

proc isLeapYear*(year: int): bool =
  ## Returns true if `year` is a leap year in the proleptic Gregorian calendar.
  (year mod 4 == 0 and year mod 100 != 0) or year mod 400 == 0

proc getDaysInMonth*(month: Month; year: int): int32 =
  ## Get the number of days in `month` of `year`.
  case month
  of mFeb:
    result = if isLeapYear(year): 29'i32 else: 28'i32
  of mApr, mJun, mSep, mNov:
    result = 30'i32
  else:
    result = 31'i32

proc civilFromDays(z: int64): tuple[y: int; m: int; d: int] =
  # Convert days since 1970-01-01 to a (year, month, day) tuple.
  let zz = z + 719468'i64
  let era = (if zz >= 0: zz else: zz - 146096) div 146097'i64
  let doe = zz - era * 146097'i64
  let yoe = (doe - doe div 1460'i64 + doe div 36524'i64 - doe div 146096'i64) div 365'i64
  let y = yoe + era * 400'i64
  let doy = doe - (365'i64 * yoe + yoe div 4'i64 - yoe div 100'i64)
  let mp = (5'i64 * doy + 2'i64) div 153'i64
  let d = doy - (153'i64 * mp + 2'i64) div 5'i64 + 1'i64
  let m = if mp < 10'i64: mp + 3'i64 else: mp - 9'i64
  result = (int(y + (if m <= 2'i64: 1'i64 else: 0'i64)), int(m), int(d))

proc daysFromCivil(y, m, d: int): int64 =
  # Days since 1970-01-01 for the given civil date.
  let yy = if m <= 2: y - 1 else: y
  let era = (if yy >= 0: yy else: yy - 399) div 400
  let yoe = int64(yy - era * 400)
  let mm = if m > 2: m - 3 else: m + 9
  let doy = int64((153 * mm + 2) div 5 + d - 1)
  let doe = yoe * 365'i64 + yoe div 4'i64 - yoe div 100'i64 + doy
  result = int64(era) * 146097'i64 + doe - 719468'i64

proc dayOfYear(y, m, d: int): int32 =
  const cumulative: array[12, int32] = [
    0'i32, 31'i32, 59'i32, 90'i32, 120'i32, 151'i32,
    181'i32, 212'i32, 243'i32, 273'i32, 304'i32, 334'i32]
  var r = cumulative[m - 1] + int32(d - 1)
  if m > 2 and isLeapYear(y):
    r = r + 1'i32
  result = r

proc weekdayFromDays(daysSinceEpoch: int64): WeekDay =
  # 1970-01-01 was a Thursday.
  var w = (daysSinceEpoch + 3'i64) mod 7'i64
  if w < 0: w = w + 7'i64
  result = WeekDay(int(w))

proc utc*(t: Time): DateTime =
  ## Converts a `Time` to a `DateTime` in UTC.
  let days = t.seconds div int64(secondsInDay)
  var secOfDay = t.seconds mod int64(secondsInDay)
  var d = days
  if secOfDay < 0:
    secOfDay = secOfDay + int64(secondsInDay)
    d = d - 1
  let civil = civilFromDays(d)
  let hour = secOfDay div int64(secondsInHour)
  let rem = secOfDay mod int64(secondsInHour)
  let minute = rem div int64(secondsInMin)
  let second = rem mod int64(secondsInMin)
  result = DateTime(
    year: civil.y,
    month: Month(civil.m),
    monthday: int32(civil.d),
    hour: int32(hour),
    minute: int32(minute),
    second: int32(second),
    nanosecond: t.nanosecond,
    weekday: weekdayFromDays(d),
    yearday: dayOfYear(civil.y, civil.m, civil.d))

proc initDateTime*(year: int; month: Month; monthday: int32;
                   hour: int32 = 0'i32; minute: int32 = 0'i32;
                   second: int32 = 0'i32;
                   nanosecond: int32 = 0'i32): DateTime =
  ## Creates a new `DateTime` in UTC.
  let d = daysFromCivil(year, int(month), int(monthday))
  result = DateTime(
    year: year,
    month: month,
    monthday: monthday,
    hour: hour,
    minute: minute,
    second: second,
    nanosecond: nanosecond,
    weekday: weekdayFromDays(d),
    yearday: dayOfYear(year, int(month), int(monthday)))

proc toTime*(dt: DateTime): Time =
  ## Converts a `DateTime` to a `Time`. The `DateTime` is assumed to be in UTC.
  let days = daysFromCivil(dt.year, int(dt.month), int(dt.monthday))
  let secs = days * int64(secondsInDay) +
             int64(dt.hour) * int64(secondsInHour) +
             int64(dt.minute) * int64(secondsInMin) +
             int64(dt.second)
  result = Time(seconds: secs, nanosecond: dt.nanosecond)

proc now*(): DateTime {.tags: [TimeEffect].} =
  ## Returns the current UTC date and time.
  result = utc(getTime())

# --- formatting ---

proc pad2(v: int32): string =
  result = ""
  if v < 10'i32: result.add '0'
  result.add $int(v)

proc pad4(v: int): string =
  let s = $v
  result = ""
  var pad = 4 - s.len
  while pad > 0:
    result.add '0'
    dec pad
  result.add s

proc `$`*(dt: DateTime): string =
  ## Converts a `DateTime` to ISO-8601 format: `YYYY-MM-DDTHH:MM:SS`.
  result = pad4(dt.year)
  result.add '-'
  result.add pad2(int32(dt.month))
  result.add '-'
  result.add pad2(dt.monthday)
  result.add 'T'
  result.add pad2(dt.hour)
  result.add ':'
  result.add pad2(dt.minute)
  result.add ':'
  result.add pad2(dt.second)

proc `$`*(t: Time): string =
  $utc(t)

proc `$`*(d: Duration): string =
  ## Formats a `Duration` as `Ns M.N` (seconds.nanoseconds).
  result = $d.seconds
  if d.nanosecond != 0'i32:
    result.add '.'
    let ns = $int(d.nanosecond)
    var pad = 9 - ns.len
    while pad > 0:
      result.add '0'
      dec pad
    result.add ns
  result.add 's'

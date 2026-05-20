## Regression tests for std/times. Covers construction, arithmetic,
## civil-date math, and formatting across the Unix epoch boundary.

import std/[assertions, syncio, times]

# --- initTime, fromUnix, toUnix ---

block:
  let t = initTime(1_700_000_000'i64, 0)
  assert t.toUnix() == 1_700_000_000'i64
  assert t.seconds() == 1_700_000_000'i64
  assert t.nanosecond() == 0'i32

block:
  let t = fromUnix(0'i64)
  assert t.toUnix() == 0'i64
  let dt = utc(t)
  assert dt.year == 1970
  assert dt.month == mJan
  assert dt.monthday == 1'i32
  assert dt.hour == 0'i32
  assert dt.minute == 0'i32
  assert dt.second == 0'i32
  assert dt.weekday == dThu    # 1970-01-01 was a Thursday.
  assert dt.yearday == 0'i32

# --- Negative-nanosecond normalization ---

block:
  let t = initTime(10'i64, -1'i64)
  # -1 ns normalizes to +(10^9 - 1) ns with seconds decremented by 1.
  assert t.seconds() == 9'i64
  assert t.nanosecond() == 999_999_999'i32

# --- initDuration with mixed units ---

block:
  let d = initDuration(seconds = 1, milliseconds = 2, microseconds = 3,
                       nanoseconds = 4)
  # 1s + 2ms + 3us + 4ns = 1_002_003_004 ns
  assert d.inNanoseconds == 1_002_003_004'i64
  assert d.inMicroseconds == 1_002_003'i64
  assert d.inMilliseconds == 1_002'i64
  assert d.inSeconds == 1'i64

block:
  let d = initDuration(weeks = 1, days = 2, hours = 3, minutes = 4)
  # (7+2)*86400 + 3*3600 + 4*60 = 777_600 + 10_800 + 240 = 788_640 s
  assert d.inSeconds == 788_640'i64

# --- Time + Duration / Time - Duration ---

block:
  let t0 = initTime(100'i64, 500_000_000'i64)
  let d = initDuration(seconds = 2, nanoseconds = 600_000_000'i64)
  let t1 = t0 + d
  # 100.5 + 2.6 = 103.1
  assert t1.seconds() == 103'i64
  assert t1.nanosecond() == 100_000_000'i32

  let t2 = t1 - d
  assert t2.seconds() == t0.seconds()
  assert t2.nanosecond() == t0.nanosecond()

# --- Time subtraction gives Duration ---

block:
  let a = initTime(1'i64, 0)
  let b = initTime(3'i64, 500_000_000'i64)
  let d = b - a
  assert d.inMilliseconds == 2_500'i64

# --- Comparison operators ---

block:
  let a = initTime(5'i64, 100'i64)
  let b = initTime(5'i64, 200'i64)
  let c = initTime(6'i64, 0'i64)
  let aCopy = initTime(5'i64, 100'i64)
  assert a == aCopy
  assert a < b
  assert a <= b
  assert a <= aCopy
  assert not (b < a)
  assert b < c
  assert not (c < a)

block:
  let d1 = initDuration(seconds = 1)
  let d2 = initDuration(seconds = 1, nanoseconds = 1)
  let d3 = initDuration(seconds = 2)
  assert d1 < d2
  assert d2 < d3
  assert d1 <= d1
  assert d1 == initDuration(milliseconds = 1000)

# --- getDaysInMonth ---

block:
  assert getDaysInMonth(mJan, 2024) == 31'i32
  assert getDaysInMonth(mFeb, 2024) == 29'i32    # leap year
  assert getDaysInMonth(mFeb, 2023) == 28'i32
  assert getDaysInMonth(mFeb, 1900) == 28'i32    # not a leap year (divisible by 100, not 400)
  assert getDaysInMonth(mFeb, 2000) == 29'i32    # leap year (divisible by 400)
  assert getDaysInMonth(mApr, 2024) == 30'i32
  assert getDaysInMonth(mDec, 2024) == 31'i32

# --- isLeapYear ---

block:
  assert isLeapYear(2000)
  assert isLeapYear(2024)
  assert not isLeapYear(2023)
  assert not isLeapYear(1900)
  assert not isLeapYear(1800)

# --- initDateTime / toTime round-trip ---

block:
  let dt = initDateTime(2024, mMar, 15'i32, 12'i32, 34'i32, 56'i32, 0'i32)
  assert dt.year == 2024
  assert dt.month == mMar
  assert dt.monthday == 15'i32
  assert dt.hour == 12'i32
  assert dt.minute == 34'i32
  assert dt.second == 56'i32
  assert dt.weekday == dFri    # 2024-03-15 was a Friday.

  let t = toTime(dt)
  let dt2 = utc(t)
  assert dt2.year == dt.year
  assert dt2.month == dt.month
  assert dt2.monthday == dt.monthday
  assert dt2.hour == dt.hour
  assert dt2.minute == dt.minute
  assert dt2.second == dt.second
  assert dt2.weekday == dt.weekday
  assert dt2.yearday == dt.yearday

# --- Pre-1970 / civil-date boundary ---

block:
  # 1969-12-31 23:59:59 UTC -> -1 Unix seconds.
  let dt = initDateTime(1969, mDec, 31'i32, 23'i32, 59'i32, 59'i32, 0'i32)
  let t = toTime(dt)
  assert t.seconds() == -1'i64

  let back = utc(t)
  assert back.year == 1969
  assert back.month == mDec
  assert back.monthday == 31'i32
  assert back.hour == 23'i32
  assert back.minute == 59'i32
  assert back.second == 59'i32

block:
  # Gregorian epoch landmark: 2000-03-01 (first day after Feb in a century leap year).
  let dt = initDateTime(2000, mMar, 1'i32)
  assert dt.weekday == dWed
  let back = utc(toTime(dt))
  assert back.year == 2000
  assert back.month == mMar
  assert back.monthday == 1'i32

# --- Year-day computation ---

block:
  let jan1 = initDateTime(2024, mJan, 1'i32)
  assert jan1.yearday == 0'i32
  let dec31 = initDateTime(2024, mDec, 31'i32)
  assert dec31.yearday == 365'i32    # leap year: 366 days -> yearday 0..365
  let dec31NonLeap = initDateTime(2023, mDec, 31'i32)
  assert dec31NonLeap.yearday == 364'i32

# --- Formatting ---

block:
  let dt = initDateTime(2024, mMar, 15'i32, 9'i32, 5'i32, 7'i32, 0'i32)
  assert $dt == "2024-03-15T09:05:07"

block:
  # Pre-1970 date still formats with 4-digit year.
  let dt = initDateTime(1969, mJan, 2'i32, 3'i32, 4'i32, 5'i32, 0'i32)
  assert $dt == "1969-01-02T03:04:05"

block:
  let d = initDuration(seconds = 2, nanoseconds = 300_000_000'i64)
  assert $d == "2.300000000s"
  assert $initDuration(seconds = 5) == "5s"
  assert $initDuration(nanoseconds = 7) == "0.000000007s"

block:
  # $Time routes through utc + $DateTime.
  let t = initTime(0'i64, 0)
  assert $t == "1970-01-01T00:00:00"

# --- getTime sanity ---

block:
  let t = getTime()
  # Strictly after 2020-01-01 UTC (1_577_836_800) and before 2200-01-01.
  assert t.toUnix() > 1_577_836_800'i64
  assert t.toUnix() < 7_258_118_400'i64

echo "ok"

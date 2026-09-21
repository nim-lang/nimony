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
## calendar breakdown in UTC, the process's local zone, or a `Timezone` of
## the application's own. For monotonic timestamps suitable for measuring
## durations, use `std/monotimes <monotimes.html>`_.

{.feature: "staticContracts".}

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

  ZonedTime* = object
    ## A `Time` with the offset and DST state of some zone at that instant.
    time*: Time
    utcOffset*: int   ## Seconds east of UTC.
    isDst*: bool

  Timezone* = ref object
    ## A time zone, as in Nim 2: a name and the two conversions, from an
    ## instant and from an adjusted time (the wall-clock fields read as if
    ## they were UTC). `utc()` and `local()` are the built-in ones, each a
    ## single instance, so zones compare by identity; `newTimezone` makes
    ## another.
    name: string
    zonedTimeFromTimeImpl: proc (time: Time): ZonedTime {.closure.}
    zonedTimeFromAdjTimeImpl: proc (adjTime: Time): ZonedTime {.closure.}

  DateTime* = object  ## A calendar date and time of day in some `Timezone`.
    year*: int
    month*: Month
    monthday*: int32     ## Day of the month, 1..31.
    hour*: int32         ## 0..23
    minute*: int32       ## 0..59
    second*: int32       ## 0..59
    nanosecond*: int32   ## 0..999_999_999
    weekday*: WeekDay
    yearday*: int32      ## 0..365
    # Set only by this module's conversions, so that the zone and the offset
    # always agree; read through `utcOffset`, `isDst` and `timezone`.
    utcOffset: int
    isDst: bool
    timezone: nil Timezone

# --- basic helpers for Time / Duration ---

func initTime*(seconds: int64; nanoseconds: int64 = 0): Time =
  ## Creates a new `Time` from a Unix timestamp.
  const nanosPerSec = 1_000_000_000'i64
  var s = seconds + nanoseconds div nanosPerSec
  var n = nanoseconds mod nanosPerSec
  if n < 0:
    n = n + nanosPerSec
    s = s - 1
  result = Time(seconds: s, nanosecond: int32(n))

func fromUnix*(unix: int64): Time =
  ## Convert a Unix timestamp (seconds since 1970-01-01 UTC) to a `Time`.
  initTime(unix, 0)

func toUnix*(t: Time): int64 =
  ## Converts `t` to a Unix timestamp.
  t.seconds

func seconds*(t: Time): int64 {.inline.} = t.seconds
func nanosecond*(t: Time): int32 {.inline.} = t.nanosecond

func initDuration*(seconds: int64 = 0; nanoseconds: int64 = 0;
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

func inSeconds*(d: Duration): int64 = d.seconds
func inMilliseconds*(d: Duration): int64 =
  d.seconds * 1_000'i64 + int64(d.nanosecond) div 1_000_000'i64
func inMicroseconds*(d: Duration): int64 =
  d.seconds * 1_000_000'i64 + int64(d.nanosecond) div 1_000'i64
func inNanoseconds*(d: Duration): int64 =
  d.seconds * 1_000_000_000'i64 + int64(d.nanosecond)

func `==`*(a, b: Time): bool =
  a.seconds == b.seconds and a.nanosecond == b.nanosecond
func `<`*(a, b: Time): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond < b.nanosecond)
func `<=`*(a, b: Time): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond <= b.nanosecond)

func `==`*(a, b: Duration): bool =
  a.seconds == b.seconds and a.nanosecond == b.nanosecond
func `<`*(a, b: Duration): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond < b.nanosecond)
func `<=`*(a, b: Duration): bool =
  a.seconds < b.seconds or
    (a.seconds == b.seconds and a.nanosecond <= b.nanosecond)

func `-`*(a, b: Time): Duration =
  ## Returns the duration between two times.
  var s = a.seconds - b.seconds
  var n = int64(a.nanosecond) - int64(b.nanosecond)
  if n < 0:
    n = n + 1_000_000_000'i64
    s = s - 1
  result = Duration(seconds: s, nanosecond: int32(n))

func `+`*(t: Time; d: Duration): Time =
  ## Adds a duration to a time.
  var s = t.seconds + d.seconds
  var n = int64(t.nanosecond) + int64(d.nanosecond)
  if n >= 1_000_000_000'i64:
    n = n - 1_000_000_000'i64
    s = s + 1
  result = Time(seconds: s, nanosecond: int32(n))

func `-`*(t: Time; d: Duration): Time =
  ## Subtracts a duration from a time.
  var s = t.seconds - d.seconds
  var n = int64(t.nanosecond) - int64(d.nanosecond)
  if n < 0:
    n = n + 1_000_000_000'i64
    s = s - 1
  result = Time(seconds: s, nanosecond: int32(n))

# --- Current time via C library ---

when defined(wasm32) and defined(standalone):
  proc getTime*(): Time {.tags: [TimeEffect].} =
    ## Freestanding wasm has no wall clock: epoch zero, honestly. A real
    ## host clock (Date.now via an env import) arrives with the ward-bridge
    ## host-imports mechanism; this is the same placement as monotimes'
    ## wasmMonoTicks.
    result = Time(seconds: 0, nanosecond: 0)

elif defined(posix):
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

func isLeapYear*(year: int): bool =
  ## Returns true if `year` is a leap year in the proleptic Gregorian calendar.
  (year mod 4 == 0 and year mod 100 != 0) or year mod 400 == 0

func getDaysInMonth*(month: Month; year: int): int32 =
  ## Get the number of days in `month` of `year`.
  case month
  of mFeb:
    result = if isLeapYear(year): 29'i32 else: 28'i32
  of mApr, mJun, mSep, mNov:
    result = 30'i32
  else:
    result = 31'i32

func civilFromDays(z: int64): tuple[y: int; m: int; d: int] =
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

func daysFromCivil(y, m, d: int): int64 =
  # Days since 1970-01-01 for the given civil date.
  let yy = if m <= 2: y - 1 else: y
  let era = (if yy >= 0: yy else: yy - 399) div 400
  let yoe = int64(yy - era * 400)
  let mm = if m > 2: m - 3 else: m + 9
  let doy = int64((153 * mm + 2) div 5 + d - 1)
  let doe = yoe * 365'i64 + yoe div 4'i64 - yoe div 100'i64 + doy
  result = int64(era) * 146097'i64 + doe - 719468'i64

func dayOfYear(y: int; m: Month; d: int): int32 =
  const cumulative: array[12, int32] = [
    0'i32, 31'i32, 59'i32, 90'i32, 120'i32, 151'i32,
    181'i32, 212'i32, 243'i32, 273'i32, 304'i32, 334'i32]
  # an enum conversion is not range checked, so `Month(13)` is a value too
  let mi = ord(m) - 1
  if mi < 0 or mi >= cumulative.len: return int32(d - 1)
  var r = cumulative[mi] + int32(d - 1)
  if mi > 1 and isLeapYear(y):
    r = r + 1'i32
  result = r

func weekdayFromDays(daysSinceEpoch: int64): WeekDay =
  # 1970-01-01 was a Thursday.
  var w = (daysSinceEpoch + 3'i64) mod 7'i64
  if w < 0: w = w + 7'i64
  result = WeekDay(int(w))

func civilDateTime(adj: Time): DateTime =
  ## The calendar fields for an adjusted time: `adj` read as if it were UTC.
  let days = adj.seconds div int64(secondsInDay)
  var secOfDay = adj.seconds mod int64(secondsInDay)
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
    nanosecond: adj.nanosecond,
    weekday: weekdayFromDays(d),
    yearday: dayOfYear(civil.y, Month(civil.m), civil.d),
    utcOffset: 0,
    isDst: false,
    timezone: nil)

func toAdjTime(dt: DateTime): Time =
  ## The wall-clock fields read as if they were UTC.
  let days = daysFromCivil(dt.year, int(dt.month), int(dt.monthday))
  let secs = days * int64(secondsInDay) +
             int64(dt.hour) * int64(secondsInHour) +
             int64(dt.minute) * int64(secondsInMin) +
             int64(dt.second)
  result = Time(seconds: secs, nanosecond: dt.nanosecond)

func toTime*(dt: DateTime): Time =
  ## Converts a `DateTime` to the instant it names: its fields, less its
  ## `utcOffset`.
  result = toAdjTime(dt) - initDuration(seconds = int64(dt.utcOffset))

func utcOffset*(dt: DateTime): int {.inline.} =
  ## Seconds east of UTC, DST included: `+12:00` is `43200`, so
  ## `local = utc + utcOffset`. (Nim 2's `utcOffset` counts west; this one
  ## follows ISO 8601.)
  dt.utcOffset

func isDst*(dt: DateTime): bool {.inline.} =
  ## Whether DST was in effect at this instant in `dt`'s zone.
  dt.isDst

func timezone*(dt: DateTime): nil Timezone {.inline.} =
  ## The zone the fields are expressed in; nil only for a `default(DateTime)`.
  dt.timezone

# --- Time zones ---
#
# A zone answers two questions, both as a `ZonedTime`: what offset applies
# at an instant, and what instant a wall-clock reading names. `utc()` is
# trivial; `local()` asks the platform's `localtime`, deriving the offset
# rather than reading `tm_gmtoff`, a GNU/BSD extension Windows lacks.

proc newTimezone*(name: string;
                  zonedTimeFromTimeImpl: proc (time: Time): ZonedTime {.closure.};
                  zonedTimeFromAdjTimeImpl: proc (adjTime: Time): ZonedTime {.closure.}
                 ): Timezone =
  ## Creates a zone from its name and its two conversions.
  Timezone(name: name, zonedTimeFromTimeImpl: zonedTimeFromTimeImpl,
           zonedTimeFromAdjTimeImpl: zonedTimeFromAdjTimeImpl)

proc name*(zone: Timezone): string = zone.name

proc zonedTimeFromTime*(zone: Timezone; time: Time): ZonedTime =
  ## The zone's offset and DST state at the instant `time`.
  zone.zonedTimeFromTimeImpl(time)

proc zonedTimeFromAdjTime*(zone: Timezone; adjTime: Time): ZonedTime =
  ## The instant named by a wall-clock reading (`adjTime`: the fields read
  ## as if they were UTC), with the offset that applied to it.
  zone.zonedTimeFromAdjTimeImpl(adjTime)

proc `$`*(zone: Timezone): string = zone.name

proc utcTzInfo(t: Time): ZonedTime =
  ZonedTime(time: t, utcOffset: 0, isDst: false)

when defined(wasm32) and defined(standalone):
  proc localZonedTimeFromTime(t: Time): ZonedTime =
    ## Freestanding wasm has no tz database: local is UTC, as for `getTime`.
    utcTzInfo(t)
  proc localZonedTimeFromAdjTime(adj: Time): ZonedTime = utcTzInfo(adj)

else:
  type
    CTime {.importc: "time_t", header: "<time.h>".} = int64
      ## `localtime_r` takes a pointer to one, so the width must be C's.
    Tm {.importc: "struct tm", header: "<time.h>".} = object
      ## Only the fields read here; C owns the layout.
      tm_sec: cint
      tm_min: cint
      tm_hour: cint
      tm_mday: cint
      tm_mon: cint      ## 0..11
      tm_year: cint     ## years since 1900
      tm_isdst: cint

  when defined(windows):
    proc localtimeS(res: ptr Tm; t: ptr CTime): cint {.
      importc: "localtime_s", header: "<time.h>".}
    proc tzsetImpl() {.importc: "_tzset", header: "<time.h>".}

    proc brokenDownLocal(tt: var CTime; tmv: var Tm): bool =
      ## UCRT's `localtime_s` swaps the arguments and returns an errno_t.
      result = localtimeS(addr tmv, addr tt) == cint(0)

  else:
    proc localtimeR(t: ptr CTime; res: ptr Tm): pointer {.
      importc: "localtime_r", header: "<time.h>".}
    proc tzsetImpl() {.importc: "tzset", header: "<time.h>".}

    proc brokenDownLocal(tt: var CTime; tmv: var Tm): bool =
      result = localtimeR(addr tt, addr tmv) != nil

  var tzReady = false

  proc ensureTz() =
    ## `localtime_r` need not call `tzset` (glibc's does not), and before the
    ## first `tzset` the zone reads as UTC. Idempotent, so a race is harmless.
    if not tzReady:
      tzsetImpl()
      tzReady = true

  proc localOffsetAndDst(unix: int64): tuple[offset: int, dst: bool] =
    ## Seconds east of UTC and the DST flag at `unix`: the local fields
    ## re-encoded as UTC, minus the instant. `(0, false)` when the platform
    ## cannot answer, which degrades to UTC.
    ensureTz()
    var tt = CTime(unix)
    var tmv = default(Tm)
    if not brokenDownLocal(tt, tmv):
      return (0, false)
    let asIfUtc = daysFromCivil(int(tmv.tm_year) + 1900, int(tmv.tm_mon) + 1,
                                int(tmv.tm_mday)) * int64(secondsInDay) +
                  int64(tmv.tm_hour) * int64(secondsInHour) +
                  int64(tmv.tm_min) * int64(secondsInMin) +
                  int64(tmv.tm_sec)
    result = (int(asIfUtc - unix), tmv.tm_isdst > cint(0))

  proc localZonedTimeFromTime(t: Time): ZonedTime =
    let (off, dst) = localOffsetAndDst(t.seconds)
    ZonedTime(time: t, utcOffset: off, isDst: dst)

  proc localZonedTimeFromAdjTime(adj: Time): ZonedTime =
    ## A wall-clock reading near a DST transition may be ambiguous or
    ## nonexistent; the offset a day either side decides, as in Nim 2.
    var adjUnix = adj.seconds
    let (pastOff, _) = localOffsetAndDst(adjUnix - int64(secondsInDay))
    let (futureOff, _) = localOffsetAndDst(adjUnix + int64(secondsInDay))
    var off = pastOff
    if pastOff != futureOff:
      if pastOff < futureOff:
        # The clocks went forward: a reading in the gap is pushed past it.
        adjUnix = adjUnix - int64(secondsInHour)
      adjUnix = adjUnix - int64(pastOff)
      off = localOffsetAndDst(adjUnix).offset
    let utcUnix = adj.seconds - int64(off)
    let (finalOff, dst) = localOffsetAndDst(utcUnix)
    ZonedTime(time: initTime(utcUnix, int64(adj.nanosecond)),
              utcOffset: finalOff, isDst: dst)

let utcInstance = newTimezone("Etc/UTC", utcTzInfo, utcTzInfo)
let localInstance = newTimezone("LOCAL", localZonedTimeFromTime,
                                localZonedTimeFromAdjTime)

proc utc*(): Timezone =
  ## The UTC zone, named `Etc/UTC`.
  utcInstance

proc local*(): Timezone =
  ## The process's zone (`TZ`, else the system default), named `LOCAL`. A
  ## process that inherits UTC answers UTC.
  localInstance

proc initDateTime(zt: ZonedTime; zone: Timezone): DateTime =
  result = civilDateTime(zt.time + initDuration(seconds = int64(zt.utcOffset)))
  result.utcOffset = zt.utcOffset
  result.isDst = zt.isDst
  result.timezone = zone

proc inZone*(time: Time; zone: Timezone): DateTime =
  ## The `DateTime` for the instant `time` in `zone`, so
  ## `toTime(inZone(t, zone)) == t`.
  initDateTime(zone.zonedTimeFromTime(time), zone)

proc inZone*(dt: DateTime; zone: Timezone): DateTime =
  ## The same instant, expressed in `zone`.
  inZone(toTime(dt), zone)

proc initDateTime*(year: int; month: Month; monthday: int32;
                   hour: int32 = 0'i32; minute: int32 = 0'i32;
                   second: int32 = 0'i32;
                   nanosecond: int32 = 0'i32;
                   zone: Timezone = utc()): DateTime =
  ## Creates a `DateTime` from a wall-clock reading in `zone`.
  let d = daysFromCivil(year, int(month), int(monthday))
  let adj = Time(seconds: d * int64(secondsInDay) +
                          int64(hour) * int64(secondsInHour) +
                          int64(minute) * int64(secondsInMin) + int64(second),
                 nanosecond: nanosecond)
  initDateTime(zone.zonedTimeFromAdjTime(adj), zone)

proc utc*(t: Time): DateTime =
  ## Converts a `Time` to a `DateTime` in UTC.
  inZone(t, utc())

proc local*(t: Time): DateTime =
  ## Converts a `Time` to a `DateTime` in the process's zone.
  inZone(t, local())

proc utc*(dt: DateTime): DateTime = inZone(dt, utc())
proc local*(dt: DateTime): DateTime = inZone(dt, local())

proc now*(): DateTime {.tags: [TimeEffect].} =
  ## Returns the current UTC date and time.
  result = utc(getTime())

# --- formatting ---

func pad2(v: int32): string =
  result = ""
  if v < 10'i32: result.add '0'
  result.add $int(v)

func pad4(v: int): string =
  let s = $v
  result = ""
  var pad = 4 - s.len
  while pad > 0:
    result.add '0'
    dec pad
  result.add s

func `$`*(dt: DateTime): string =
  ## Converts a `DateTime` to ISO-8601: `YYYY-MM-DDTHH:MM:SS` followed by
  ## `Z` in UTC (or with no zone) and `±HH:MM` in any other zone.
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
  var z = dt.timezone
  var isUtc = true
  if z != nil:
    isUtc = z.name == "Etc/UTC"
  if isUtc:
    result.add 'Z'
  else:
    var off = dt.utcOffset
    if off < 0:
      result.add '-'
      off = -off
    else:
      result.add '+'
    result.add pad2(int32(off div secondsInHour))
    result.add ':'
    result.add pad2(int32((off mod secondsInHour) div secondsInMin))

func `$`*(t: Time): string =
  ## A `Time` names an instant, so it renders in UTC, with `Z`.
  $civilDateTime(t)

func `$`*(d: Duration): string =
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

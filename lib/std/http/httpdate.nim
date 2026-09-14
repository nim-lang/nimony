# (c) 2026 Andreas Rumpf
#
# HTTP dates: the one timestamp format the protocol names, in both directions.
# See doc/internals/http.md.
#
#   var buf = default(array[HttpDateLen, char])
#   discard nowHttpDate(buf)
#   m.addHeader(hDate, buf)          # no allocation, one conversion per second
#
# HTTP has exactly one date format to *write* — IMF-fixdate, RFC 9110 §5.6.7:
#
#   Sun, 06 Nov 1994 08:49:37 GMT
#
# Fixed width, fixed field order, always GMT, and always the English day and
# month abbreviations regardless of anyone's locale. That is what makes it 29
# bytes that can be written into a buffer rather than formatted, and it is why
# there is no format string here: there is nothing to choose.
#
# It has three formats to *read*, because RFC 9110 requires a recipient to
# accept the two obsolete ones — an origin server that rejects the RFC 850
# form in an `If-Modified-Since` does not fail loudly, it silently serves a
# full body to a cache that had a good copy.
#
# Nothing here does IO and nothing here allocates on the path a server takes
# per response; `std/times` does the calendar arithmetic and this module does
# the six bytes of spelling on either side of it.

import ../times

const
  HttpDateLen* = 29
    ## Every IMF-fixdate is exactly this long — the format has no
    ## variable-width field. So a caller sizes its buffer from the type rather
    ## than from a guess, and `writeHttpDate` never has to answer "how much
    ## would it have needed".

  DayNames: array[7, string] =
    ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
  MonthNames: array[12, string] =
    ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
  LongDayNames: array[7, string] =
    ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday",
     "Sunday"]
    ## Indexed by `ord`, not by `WeekDay`/`Month`: `Month` starts at 1, so an
    ## enum-indexed array and a 12-element literal do not line up, and the two
    ## conventions in one file is one of them that gets used in the wrong
    ## place.

# ------------------------------------------------------------- writing ----

proc put2(dest: var openArray[char]; i: int; v: int) {.inline.} =
  dest[i] = char(ord('0') + (v div 10) mod 10)
  dest[i + 1] = char(ord('0') + v mod 10)

proc put4(dest: var openArray[char]; i: int; v: int) {.inline.} =
  dest[i] = char(ord('0') + (v div 1000) mod 10)
  dest[i + 1] = char(ord('0') + (v div 100) mod 10)
  dest[i + 2] = char(ord('0') + (v div 10) mod 10)
  dest[i + 3] = char(ord('0') + v mod 10)

proc put3(dest: var openArray[char]; i: int; s: string) {.inline.} =
  dest[i] = s[0]
  dest[i + 1] = s[1]
  dest[i + 2] = s[2]

proc writeHttpDate*(dest: var openArray[char]; t: Time): int =
  ## Write `t` as an IMF-fixdate into the front of `dest`. Returns
  ## `HttpDateLen`, or `0` when `dest` is smaller than that and nothing was
  ## written.
  ##
  ## The year is clamped to four digits: HTTP has no spelling for a fifth, and
  ## a header that is 30 bytes where every parser expects 29 is worse than one
  ## that is wrong about the year 10000.
  if dest.len < HttpDateLen: return 0
  let dt = utc(t)
  put3 dest, 0, DayNames[ord(dt.weekday)]
  dest[3] = ','
  dest[4] = ' '
  put2 dest, 5, int(dt.monthday)
  dest[7] = ' '
  put3 dest, 8, MonthNames[ord(dt.month) - 1]
  dest[11] = ' '
  put4 dest, 12, (if dt.year < 0 or dt.year > 9999: 9999 else: dt.year)
  dest[16] = ' '
  put2 dest, 17, int(dt.hour)
  dest[19] = ':'
  put2 dest, 20, int(dt.minute)
  dest[22] = ':'
  put2 dest, 23, int(dt.second)
  dest[25] = ' '
  dest[26] = 'G'
  dest[27] = 'M'
  dest[28] = 'T'
  result = HttpDateLen

proc formatHttpDate*(t: Time): string =
  ## `t` as an IMF-fixdate. The allocating form, for the places that want a
  ## string anyway; `writeHttpDate` is the one a response takes per request.
  var buf = default(array[HttpDateLen, char])
  discard writeHttpDate(buf, t)
  result = newString(HttpDateLen)
  for i in 0..<HttpDateLen: result[i] = buf[i]

# --------------------------------------------------------- the cached now --

var cachedSecond {.threadvar.}: int64
var cachedDate {.threadvar.}: array[HttpDateLen, char]
var cachedValid {.threadvar.}: bool

proc nowHttpDate*(dest: var openArray[char]): int =
  ## The current instant as an IMF-fixdate. Returns `HttpDateLen`, or `0` if
  ## `dest` is too small.
  ##
  ## Cached for the second it names, which is not an optimisation so much as
  ## an admission: the format has one-second resolution, so every response
  ## inside the same second must produce the same 29 bytes, and doing the
  ## calendar arithmetic again to arrive at them is work whose result was
  ## already known. A server answering 50k requests a second does 1 conversion
  ## rather than 50k.
  ##
  ## The cache is per thread, so it needs no lock and can never be torn. Two
  ## lanes each keeping their own 29 bytes is cheaper than one they have to
  ## agree about, and they agree anyway — they are reading the same clock.
  if dest.len < HttpDateLen: return 0
  let t = getTime()
  let s = t.toUnix
  if not cachedValid or s != cachedSecond:
    discard writeHttpDate(cachedDate, fromUnix(s))
    cachedSecond = s
    cachedValid = true
  for i in 0..<HttpDateLen: dest[i] = cachedDate[i]
  result = HttpDateLen

# ------------------------------------------------------------- reading ----

proc digit(c: char): int {.inline.} =
  if c >= '0' and c <= '9': ord(c) - ord('0') else: -1

proc num(buf: openArray[char]; i: Natural; n: int): int =
  ## Exactly `n` digits at `i`, or `-1`. Exactly, not at least: a two-digit
  ## field that accepts three silently reinterprets every field after it.
  if i + n > buf.len: return -1   # `i >= 0` comes from its type
  result = 0
  for k in 0..<n:
    let d = digit(buf[i + k])
    if d < 0: return -1
    result = result * 10 + d

proc monthFrom(buf: openArray[char]; i: Natural): int =
  ## The 1-based month for a three-letter abbreviation at `i`, or `-1`.
  ## Case-sensitive on purpose: the abbreviations are spelled one way in the
  ## grammar, and accepting `nOv` here means accepting it nowhere else.
  if i + 3 > buf.len: return -1
  for m in 0..11:
    let s = MonthNames[m]
    if buf[i] == s[0] and buf[i + 1] == s[1] and buf[i + 2] == s[2]:
      return m + 1
  result = -1

proc isGmt(buf: openArray[char]; i: Natural): bool {.inline.} =
  i + 3 <= buf.len and buf[i] == 'G' and buf[i + 1] == 'M' and buf[i + 2] == 'T'

proc timeOfDay(buf: openArray[char]; i: Natural; h, mi, s: var int): bool =
  ## `HH:MM:SS` at `i`.
  if i + 8 > buf.len: return false
  h = num(buf, i, 2)
  mi = num(buf, i + 3, 2)
  s = num(buf, i + 6, 2)
  if h < 0 or mi < 0 or s < 0: return false
  if buf[i + 2] != ':' or buf[i + 5] != ':': return false
  # 60 is a leap second, which HTTP inherits from the grammar and no calendar
  # here can represent; it is accepted and folded into the next minute by the
  # normalisation `toTime` does anyway.
  result = h <= 23 and mi <= 59 and s <= 60

proc assemble(y, m, d, h, mi, s: int; t: var Time): bool =
  if m < 1 or m > 12: return false
  if d < 1 or d > int(getDaysInMonth(Month(m), y)): return false
  t = toTime(initDateTime(y, Month(m), int32(d), int32(h), int32(mi), int32(s)))
  result = true

proc parseImf(buf: openArray[char]; t: var Time): bool =
  ## `Sun, 06 Nov 1994 08:49:37 GMT` — fixed width, so this is a shape test
  ## and six field reads rather than a scan.
  if buf.len < HttpDateLen: return false
  if buf[3] != ',' or buf[4] != ' ' or buf[7] != ' ' or buf[11] != ' ' or
     buf[16] != ' ' or buf[25] != ' ': return false
  if not isGmt(buf, 26): return false
  let d = num(buf, 5, 2)
  let m = monthFrom(buf, 8)
  let y = num(buf, 12, 4)
  var h = 0
  var mi = 0
  var s = 0
  if not timeOfDay(buf, 17, h, mi, s): return false
  if d < 0 or m < 0 or y < 0: return false
  result = assemble(y, m, d, h, mi, s, t)

proc parseRfc850(buf: openArray[char]; t: var Time): bool =
  ## `Sunday, 06-Nov-94 08:49:37 GMT`. The day name is variable-width, so this
  ## one has to find the comma; everything after it is fixed again.
  ##
  ## The two-digit year is the reason this format was obsoleted. RFC 9110
  ## §5.6.7 fixes the reading: a year that would put the date more than 50
  ## years in the future is in the previous century.
  var c = -1
  for i in 0..<buf.len:
    if buf[i] == ',':
      c = i
      break
  if c < 0: return false
  var ok = false
  for w in 0..6:
    if LongDayNames[w].len == c:
      var same = true
      for k in 0..<c:
        if buf[k] != LongDayNames[w][k]: same = false
      if same: ok = true
  if not ok: return false
  let p = c + 2                     # past ", ", where the day-of-month starts
  # From `p`: DD-Mmm-YY HH:MM:SS GMT — 22 bytes, every field fixed.
  if p + 22 > buf.len: return false
  if buf[c + 1] != ' ' or buf[p + 2] != '-' or buf[p + 6] != '-' or
     buf[p + 9] != ' ' or buf[p + 18] != ' ': return false
  if not isGmt(buf, p + 19): return false
  let d = num(buf, p, 2)
  let m = monthFrom(buf, p + 3)
  let yy = num(buf, p + 7, 2)
  var h = 0
  var mi = 0
  var s = 0
  if not timeOfDay(buf, p + 10, h, mi, s): return false
  if d < 0 or m < 0 or yy < 0: return false
  let nowYear = utc(getTime()).year
  var y = (nowYear div 100) * 100 + yy
  if y > nowYear + 50: y = y - 100
  result = assemble(y, m, d, h, mi, s, t)

proc parseAsctime(buf: openArray[char]; t: var Time): bool =
  ## `Sun Nov  6 08:49:37 1994` — C's `asctime`, where a one-digit day is
  ## space-padded rather than zero-padded, which is the only irregular field.
  if buf.len < 24: return false
  if buf[3] != ' ' or buf[7] != ' ' or buf[10] != ' ' or buf[19] != ' ':
    return false
  let m = monthFrom(buf, 4)
  var d = num(buf, 8, 2)
  if d < 0 and buf[8] == ' ': d = num(buf, 9, 1)
  let y = num(buf, 20, 4)
  var h = 0
  var mi = 0
  var s = 0
  if not timeOfDay(buf, 11, h, mi, s): return false
  if d < 0 or m < 0 or y < 0: return false
  result = assemble(y, m, d, h, mi, s, t)

proc parseHttpDate*(buf: openArray[char]; t: var Time): bool =
  ## Read one HTTP date. `buf` is the whole header value — there is no cursor
  ## to advance, because a date header holds one date and nothing follows it.
  ##
  ## Accepts all three formats RFC 9110 §5.6.7 names, and `t` is untouched
  ## when it accepts none. A recipient MUST accept the obsolete two: a server
  ## that rejects the RFC 850 form in an `If-Modified-Since` does not fail
  ## visibly, it just stops answering 304 to the caches that send it.
  var b = 0
  var e = buf.len
  while b < e and (buf[b] == ' ' or buf[b] == '\t'): inc b
  while e > b and (buf[e - 1] == ' ' or buf[e - 1] == '\t'): dec e
  if e - b < 20: return false
  let v = toOpenArray(buf, b, e - 1)
  if parseImf(v, t): return true
  if parseRfc850(v, t): return true
  result = parseAsctime(v, t)

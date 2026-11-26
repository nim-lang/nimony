#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements helper procs for parsing and generating cookies.
##
## This is a port of Nim's `std/cookies` to Nimony. `parseCookies` reads the
## `Cookie` request header; `setCookie` creates a `Set-Cookie` response header.
## Cookie expiration dates use the fixed HTTP date format, so no general
## `strformat`-style time formatting is needed.

import std/[assertions, options, strtabs, times]

type
  SameSite* {.pure.} = enum ## The SameSite cookie attribute.
                            ## `Default` means that `setCookie`
                            ## does not set the `SameSite` attribute.
    Default, None, Lax, Strict

proc parseCookies*(s: string): StringTableRef =
  ## Parses cookies into a string table.
  ##
  ## The proc is meant to parse the Cookie header set by a client, not the
  ## "Set-Cookie" header set by servers.
  runnableExamples:
    import std/strtabs
    let cookieJar = parseCookies("a=1; foo=bar")
    assert cookieJar["a"] == "1"
    assert cookieJar["foo"] == "bar"

  result = newStringTable(modeCaseInsensitive)
  var i = 0
  while true:
    while i < s.len and (s[i] == ' ' or s[i] == '\t'):
      inc(i)
    let keystart = i
    while i < s.len and s[i] != '=':
      inc(i)
    let keyend = i - 1
    if i >= s.len:
      break
    inc(i) # skip '='
    let valstart = i
    while i < s.len and s[i] != ';':
      inc(i)
    result[substr(s, keystart, keyend)] = substr(s, valstart, i - 1)
    if i >= s.len:
      break
    inc(i) # skip ';'

func pad2(v: int32): string =
  result = ""
  if v < 10:
    result.add('0')
  result.add($v)

func pad4(v: int): string =
  result = ""
  let s = $v
  var pad = 4 - s.len
  while pad > 0:
    result.add('0')
    dec pad
  result.add(s)

func weekdayName(day: WeekDay): string =
  case day
  of dMon: "Mon"
  of dTue: "Tue"
  of dWed: "Wed"
  of dThu: "Thu"
  of dFri: "Fri"
  of dSat: "Sat"
  of dSun: "Sun"

func monthName(month: Month): string =
  case month
  of mJan: "Jan"
  of mFeb: "Feb"
  of mMar: "Mar"
  of mApr: "Apr"
  of mMay: "May"
  of mJun: "Jun"
  of mJul: "Jul"
  of mAug: "Aug"
  of mSep: "Sep"
  of mOct: "Oct"
  of mNov: "Nov"
  of mDec: "Dec"

func formatCookieDate(dt: DateTime): string =
  ## Formats a date as required by the `Expires` cookie attribute:
  ## `Thu, 01 Jan 1970 00:00:01 GMT`.
  weekdayName(dt.weekday) & ", " & pad2(dt.monthday) & " " &
    monthName(dt.month) & " " & pad4(dt.year) & " " &
    pad2(dt.hour) & ":" & pad2(dt.minute) & ":" & pad2(dt.second) & " GMT"

func setCookie*(key, value: string, domain = "", path = "",
                expires = "", noName = false,
                secure = false, httpOnly = false,
                maxAge = none[int](), sameSite = SameSite.Default): string =
  ## Creates a command in the format of
  ## `Set-Cookie: key=value; Domain=...; ...`
  ##
  ## Cookies can be vulnerable. Consider setting `secure = true`,
  ## `httpOnly = true` and `sameSite = SameSite.Strict`.
  result = ""
  if not noName:
    result.add("Set-Cookie: ")
  result.add(key & "=" & value)
  if domain != "":
    result.add("; Domain=" & domain)
  if path != "":
    result.add("; Path=" & path)
  if expires != "":
    result.add("; Expires=" & expires)
  if secure:
    result.add("; Secure")
  if httpOnly:
    result.add("; HttpOnly")
  if maxAge.isSome:
    result.add("; Max-Age=" & $maxAge.unsafeGet)

  if sameSite != SameSite.Default:
    if sameSite == SameSite.None:
      assert secure,
        "Cookies with SameSite=None must specify the Secure attribute!"
    result.add("; SameSite=" & $sameSite)

proc setCookie*(key, value: string, expires: DateTime,
                domain = "", path = "", noName = false,
                secure = false, httpOnly = false,
                maxAge = none[int](), sameSite = SameSite.Default): string =
  ## Creates a command in the format of
  ## `Set-Cookie: key=value; Domain=...; ...`
  result = setCookie(key, value, domain, path,
                     formatCookieDate(expires.utc),
                     noName, secure, httpOnly, maxAge, sameSite)

proc setCookie*(key, value: string, expires: Time,
                domain = "", path = "", noName = false,
                secure = false, httpOnly = false,
                maxAge = none[int](), sameSite = SameSite.Default): string =
  ## Creates a command in the format of
  ## `Set-Cookie: key=value; Domain=...; ...`
  result = setCookie(key, value, domain, path,
                     formatCookieDate(expires.utc),
                     noName, secure, httpOnly, maxAge, sameSite)

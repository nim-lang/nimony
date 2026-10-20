#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## `HttpHeaders`: header lines in their HTTP/1.1 wire format, `Name: value\r\n`
## in order (nim-lang/nimony#2635).
##
## Construction, iteration and case-insensitive lookup are available without
## importing a client. `std/httpclient` re-exports this module.

import std/strutils

type
  HttpHeaders* = distinct string
    ## `Name: value\r\n` lines, in order. Repeated names are just lines;
    ## lookup is a case-insensitive linear scan.

func isTokenChar*(c: char): bool {.inline.} =
  ## RFC 9110 `tchar`: what a header name is made of.
  c in {'!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~',
        '0'..'9', 'a'..'z', 'A'..'Z'}

template headers*(s: string): HttpHeaders {.plugin: "../deps/httpheaders".}
  ## A header block literal, checked while the program is compiled:
  ## `headers"Accept: application/json\r\n"`. A malformed block is a
  ## compile-time error. Values known only at run time go through `add`.

proc add*(h: var HttpHeaders; name, value: string) {.raises.} =
  ## Appends `name: value`. `BadOperation` for a name outside the token
  ## characters or a CR/LF in the value.
  if name.len == 0: raise BadOperation
  for c in name:
    if not isTokenChar(c): raise BadOperation
  for c in value:
    if c in {'\r', '\n'}: raise BadOperation
  var s = string(h)
  s.add name
  s.add ": "
  s.add value
  s.add "\r\n"
  h = HttpHeaders(s)

iterator pairs*(h: HttpHeaders): (string, string) =
  ## Every name and value in wire order, preserving duplicate fields and name
  ## spelling. Values have surrounding whitespace removed.
  let s = string(h)
  var i = 0
  while i < s.len:
    let eol = s.find("\r\n", i)
    let stop = if eol < 0: s.len else: eol
    let colon = s.find(':', i)
    if colon > i and colon < stop:
      yield (s.substr(i, colon - 1), s.substr(colon + 1, stop - 1).strip())
    i = stop + 2

iterator values*(h: HttpHeaders; name: string): string =
  ## Every value of `name`, case-insensitively, in wire order.
  for (n, v) in pairs(h):
    if cmpIgnoreCase(n, name) == 0: yield v

proc get*(h: HttpHeaders; name: string): string =
  ## The first value of `name`, case-insensitively; `""` if absent.
  result = ""
  for v in values(h, name): return v

proc has*(h: HttpHeaders; name: string): bool =
  ## Whether `name` is present, case-insensitively, even if its value is empty.
  result = false
  for (n, v) in pairs(h):
    if cmpIgnoreCase(n, name) == 0: return true

func spelledLineEnds*(s: string): string =
  ## `headers"A: b\r\n"` is a raw string literal: its `\r\n` arrives spelled
  ## out, as four characters. Both spellings mean the line end.
  result = newStringOfCap(s.len)
  var i = 0
  while i < s.len:
    if s[i] == '\\' and i + 3 < s.len and s[i+1] == 'r' and s[i+2] == '\\' and
       s[i+3] == 'n':
      result.add "\r\n"
      i += 4
    else:
      result.add s[i]
      inc i

func headerBlockError*(s: string): string =
  ## Why `s` is not a header block, or `""` when it is one: every line
  ## `token: value\r\n`, no stray CR or LF (an injected line), nothing after
  ## the last line end.
  result = ""
  var i = 0
  var line = 1
  while i < s.len:
    var nameLen = 0
    while i < s.len and s[i] != ':':
      if not isTokenChar(s[i]):
        return "line " & $line & ": a header name may not contain " &
               (if s[i] == '\r' or s[i] == '\n': "a line break"
                else: "'" & s.substr(i, i) & "'")
      inc i
      inc nameLen
    if i >= s.len: return "line " & $line & ": missing ':'"
    if nameLen == 0: return "line " & $line & ": empty header name"
    inc i                                     # ':'
    while i < s.len and s[i] != '\r':
      if s[i] == '\n': return "line " & $line & ": a bare LF inside a value"
      inc i
    if i + 1 >= s.len or s[i+1] != '\n':
      return "line " & $line & ": every line must end in \\r\\n"
    i += 2
    inc line

func validHeaderBlock*(s: string): bool {.inline.} = headerBlockError(s).len == 0

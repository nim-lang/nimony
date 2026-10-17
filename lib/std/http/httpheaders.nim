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
## Shared by `std/httpclient` and the `headers"..."` plugin
## (`lib/std/deps/httpheaders.nim`), so that a literal is judged at compile
## time by exactly the rules `add` applies at run time.

type
  HttpHeaders* = distinct string
    ## `Name: value\r\n` lines, in order. Repeated names are just lines;
    ## lookup is a case-insensitive linear scan.

func isTokenChar*(c: char): bool {.inline.} =
  ## RFC 9110 `tchar`: what a header name is made of.
  c in {'!', '#', '$', '%', '&', '\'', '*', '+', '-', '.', '^', '_', '`', '|', '~',
        '0'..'9', 'a'..'z', 'A'..'Z'}

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

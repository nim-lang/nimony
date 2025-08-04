const
  Whitespace* = {' ', '\t', '\v', '\r', '\l', '\f'}
    ## All the characters that count as whitespace (space, tab, vertical tab,
    ## carriage return, new line, form feed).

proc endsWith*(s: string; c: char): bool {.inline.} =
  if s.len > 0: s[s.len-1] == c else: false

proc strlen*(x: cstring): int {.importc: "strlen", header: "<string.h>".}

proc `$`*(x: cstring): string =
  let L = int strlen(x)
  result = newString(L)
  for i in 0..<result.len:
    result[i] = x[i]

proc `$`*(x: char): string =
  result = newString(1)
  result[0] = x

func continuesWith*(s, prefix: string; start: int): bool =
  if prefix.len > s.len-start:
    return false
  for i in 0 ..< prefix.len:
    if s[i+start] != prefix[i]:
      return false
  return true

func startsWith*(s, prefix: string): bool =
  continuesWith s, prefix, 0

proc toLowerAscii*(c: char): char {.inline.} =
  if c >= 'A' and c <= 'Z': char(int(c) - int('A') + int('a'))
  else: c

func replace*(s: string; sub, by: char): string =
  result = newString(s.len)
  var i = 0
  while i < s.len:
    if s[i] == sub: result[i] = by
    else: result[i] = s[i]
    inc i

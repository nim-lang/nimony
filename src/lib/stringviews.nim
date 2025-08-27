#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## A better stdlib would offer this...

import std / hashes

type
  pchar* = ptr UncheckedArray[char]
  StringView* = object
    p*: pchar
    len*: int

template `[]`*(s: StringView; i: int): char = s.p[i]

when not defined(nimony):
  {.pragma: untyped.}

template eqImpl() {.untyped.} =
  if a.len == b.len:
    for i in 0..<a.len:
      if a[i] != b[i]: return false
    result = true
  else:
    result = false

proc `==`*(a, b: StringView): bool = eqImpl()
proc `==`*(a: StringView; b: string): bool = eqImpl()

proc startsWith*(s: StringView; prefix: string): bool =
  let prefixLen = prefix.len
  let sLen = s.len
  var i = 0
  while true:
    if i >= prefixLen: return true
    if i >= sLen or s[i] != prefix[i]: return false
    inc(i)
  return false

proc add*(s: var string; b: StringView) =
  let l = s.len
  s.setLen l + b.len
  for i in 0..<b.len:
    s[l+i] = b[i]

when not defined(nimony):
  proc rawData(s: string): ptr UncheckedArray[char] {.inline.} =
    if s.len == 0:
      nil
    else:
      cast[ptr UncheckedArray[char]](addr(s[0]))

proc `$`*(s: StringView): string =
  result = newString(s.len)
  if s.len > 0:
    copyMem result.rawData, s.p, s.len

proc toStringViewUnsafe*(s: string): StringView =
  ## Watch out that the string lives longer than the string view!
  if s.len != 0:
    result = StringView(p: cast[pchar](s.rawData), len: s.len)
  else:
    result = StringView(p: nil, len: 0)

proc hash*(a: StringView): Hash {.inline.} =
  when defined(nimony):
    hash borrowCStringUnsafe(cast[cstring](a.p), a.len)
  else:
    hash toOpenArray(a.p, 0, a.len-1)

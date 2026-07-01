#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Base64 encoder/decoder (RFC 4648).
##
## `encode` produces standard base64 by default, or the URL- and
## filename-safe alphabet (`-`/`_` instead of `+`/`/`) when `safe = true`.
## `decode` accepts either alphabet and ignores `=` padding and whitespace.

const
  cb64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  cb64safe = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

func encode*[T: byte|char](s: openArray[T]; safe = false): string =
  ## Encodes `s` (a sequence of `char`s or `byte`s) to a base64 string.
  runnableExamples:
    assert encode("foo") == "Zm9v"
    assert encode("foob") == "Zm9vYg=="
  let alphabet = if safe: cb64safe else: cb64
  result = ""
  let n = s.len
  var i = 0
  while i + 3 <= n:
    let a = ord(s[i])
    let b = ord(s[i + 1])
    let c = ord(s[i + 2])
    result.add alphabet[(a shr 2) and 63]
    result.add alphabet[((a shl 4) or (b shr 4)) and 63]
    result.add alphabet[((b shl 2) or (c shr 6)) and 63]
    result.add alphabet[c and 63]
    i += 3
  let rem = n - i
  if rem == 1:
    let a = ord(s[i])
    result.add alphabet[(a shr 2) and 63]
    result.add alphabet[(a shl 4) and 63]
    result.add '='
    result.add '='
  elif rem == 2:
    let a = ord(s[i])
    let b = ord(s[i + 1])
    result.add alphabet[(a shr 2) and 63]
    result.add alphabet[((a shl 4) or (b shr 4)) and 63]
    result.add alphabet[(b shl 2) and 63]
    result.add '='

func encodeMime*(s: string; lineLen = 75; newLine = "\r\n"; safe = false): string =
  ## Encodes `s` to base64 (RFC 2045 MIME): the output is wrapped into lines of
  ## at most `lineLen` characters, separated by `newLine`.
  runnableExamples:
    assert encodeMime("foobar", lineLen = 4) == "Zm9v\r\nYmFy"
  let e = encode(s, safe)
  result = ""
  var i = 0
  while i < e.len:
    if i > 0: result.add newLine
    var j = 0
    while j < lineLen and i < e.len:
      result.add e[i]
      inc i
      inc j

func decodeByte(c: char): int =
  ## Maps a base64 character to its 6-bit value, or -1 for padding/whitespace/
  ## invalid input. Accepts both the standard (`+`/`/`) and URL-safe (`-`/`_`)
  ## alphabets.
  case c
  of 'A'..'Z': ord(c) - ord('A')
  of 'a'..'z': ord(c) - ord('a') + 26
  of '0'..'9': ord(c) - ord('0') + 52
  of '+', '-': 62
  of '/', '_': 63
  else: -1

func decode*(s: string): string =
  ## Decodes a base64 string (standard or URL-safe) back to the original bytes.
  ## `=` padding and any whitespace/invalid characters are ignored.
  runnableExamples:
    assert decode("Zm9v") == "foo"
    assert decode("Zm9vYg==") == "foob"
  result = ""
  var buf = 0
  var bits = 0
  for i in 0 ..< s.len:
    let v = decodeByte(s[i])
    if v < 0: continue
    buf = (buf shl 6) or v
    bits += 6
    if bits >= 8:
      bits -= 8
      result.add char((buf shr bits) and 0xFF)

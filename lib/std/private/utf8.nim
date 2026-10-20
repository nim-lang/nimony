## Strict UTF-8 scanning shared by `std/unicode`.
##
## The scanner reports where valid input ends and why it stopped, so callers
## can tell a malformed sequence from a valid but truncated one and carry the
## latter over to the next chunk.

type Utf8Scan* = object
  validUpTo*: int ## -1 on success; otherwise the start of the invalid sequence.
  errorLen*: int ## 0 for incomplete input; 1..3 for a malformed sequence.

func utf8CharWidth*(b: int): int {.inline.} =
  ## Number of bytes in the sequence introduced by `b`, or 0 if `b` cannot
  ## start a valid sequence.
  if b < 0x80: 1
  elif b in 0xC2..0xDF: 2
  elif b in 0xE0..0xEF: 3
  elif b in 0xF0..0xF4: 4
  else: 0

func isContinuation(b: int): bool {.inline.} =
  b in 0x80..0xBF

func scanUtf8*[T: char | byte](data: openArray[T]): Utf8Scan =
  ## Scans `data` for the first sequence that is not well-formed UTF-8.
  ## The `validUpTo`/`errorLen` pair separates malformed bytes from a valid
  ## but incomplete suffix, which a streaming caller can carry over.
  var index = 0
  while index < data.len:
    let first = int(data[index])
    if first < 0x80:
      inc index
      while index < data.len and int(data[index]) < 0x80:
        inc index
    else:
      let width = utf8CharWidth(first)
      if width == 0:
        return Utf8Scan(validUpTo: index, errorLen: 1)
      if data.len - index < 2:
        return Utf8Scan(validUpTo: index, errorLen: 0)
      let second = int(data[index + 1])
      # The lead byte restricts the second byte to rule out overlong forms,
      # surrogates and values above U+10FFFF; the rest are plain continuations.
      let secondOk =
        case first
        of 0xE0: second in 0xA0..0xBF
        of 0xED: second in 0x80..0x9F
        of 0xF0: second in 0x90..0xBF
        of 0xF4: second in 0x80..0x8F
        else: isContinuation(second)
      if not secondOk:
        return Utf8Scan(validUpTo: index, errorLen: 1)
      var tail = 2
      while tail < width:
        if data.len - index <= tail:
          return Utf8Scan(validUpTo: index, errorLen: 0)
        if not isContinuation(int(data[index + tail])):
          return Utf8Scan(validUpTo: index, errorLen: tail)
        inc tail
      index += width
  result = Utf8Scan(validUpTo: -1)

#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## PEM armour (RFC 7468) around SECRET data, for `std/crypto/rsasign`.
##
## `std/base64` decodes through a table indexed by the character, and
## `strutils.find` through a skip table indexed by the text it searches;
## both leak, through the cache, which characters a private key consists of.
## The routines here use neither: every character of a body goes through the
## same arithmetic, conditions become all-ones/zero `uint32` masks, and loops
## run over the whole input. What can still shape the control flow and the
## output position is the layout of the body — its length and where its line
## breaks, spaces and `=` padding are — which is not secret.

{.feature: "staticContracts".}

import std/volatile

func inRange(c: int32; lo, hi: char): uint32 {.inline.} =
  ## All ones when `lo <= c <= hi`, else zero.
  let d = (c - int32(ord(lo))) or (int32(ord(hi)) - c)   # negative iff not
  result = (uint32(d) shr 31) - 1'u32

func isZeroMask(x: uint32): uint32 {.inline.} =
  ## All ones when `x == 0`, else zero.
  result = 0'u32 - (((x or (0'u32 - x)) shr 31) xor 1'u32)

proc wipe*(s: var seq[byte]) =
  ## Overwrites every byte of `s` with zero, through volatile stores the C
  ## compiler may not drop as dead.
  for i in 0 ..< s.len:
    volatileStore(addr s[i], 0'u8)

func ctFindByte*(s: openArray[char]; start: int; c: char): int =
  ## The first index at or after `start` holding `c`, or `s.len`. Reads all
  ## of `s[start ..^ 1]` whatever it finds, and branches on none of it.
  result = s.len
  var found = 0'u32
  for i in max(start, 0) ..< s.len:
    let hit = inRange(int32(ord(s[i])), c, c) and not found
    result = int((uint32(i) and hit) or (uint32(result) and not hit))
    found = found or hit

func ctContinuesWith*(s: openArray[char]; start: int; lit: string): bool =
  ## Whether `s` holds `lit` at `start`, comparing every byte of `lit`.
  result = false
  if start < 0 or lit.len > s.len - start: return
  var diff = 0'u32
  for i in 0 ..< lit.len:
    let j = start + i
    if j >= 0 and j < s.len:
      diff = diff or (uint32(ord(s[j])) xor uint32(ord(lit[i])))
  result = isZeroMask(diff) != 0'u32

func decode6(x: int32; valid: var uint32): uint32 {.inline.} =
  ## The 6-bit value of one standard-alphabet base64 letter; `valid` is all
  ## ones when `x` is one.
  let upper = inRange(x, 'A', 'Z')
  let lower = inRange(x, 'a', 'z')
  let digit = inRange(x, '0', '9')
  let plus = inRange(x, '+', '+')
  let slash = inRange(x, '/', '/')
  valid = upper or lower or digit or plus or slash
  result = (upper and uint32(x - int32(ord('A')))) or
           (lower and uint32(x - int32(ord('a')) + 26)) or
           (digit and uint32(x - int32(ord('0')) + 52)) or
           (plus and 62'u32) or (slash and 63'u32)

proc ctBase64Decode*(s: openArray[char]; res: var seq[byte]): bool =
  ## Decodes a PEM body: the standard alphabet with the `=` padding it must
  ## have, and line breaks (CR, LF), tabs and spaces anywhere. `false` for
  ## anything else, for a misplaced or missing `=`, and for nonzero bits
  ## left over after the last byte (a non-canonical encoding). `res` is empty
  ## on `false`; on `true` it holds the secret and is the caller's to `wipe`.
  res = newSeq[byte](s.len div 4 * 3 + 4)
  var acc = 0'u32          # the last decoded bits, low `nbits` of them pending
  var nbits = 0'u32
  var o = 0                # output position
  var letters = 0
  var pads = 0
  var bad = 0'u32          # all ones once anything was wrong
  var padSeen = 0'u32
  for i in 0 ..< s.len:
    let x = int32(ord(s[i]))
    var isLetter = 0'u32
    let v = decode6(x, isLetter)
    let isPad = inRange(x, '=', '=')
    let isSpace = inRange(x, '\t', '\n') or inRange(x, '\r', '\r') or inRange(x, ' ', ' ')
    bad = bad or not (isLetter or isPad or isSpace) or (padSeen and isLetter)
    padSeen = padSeen or isPad
    acc = (acc shl (6'u32 and isLetter)) or (v and isLetter)
    nbits = nbits + (6'u32 and isLetter)
    letters = letters + int(1'u32 and isLetter)
    pads = pads + int(1'u32 and isPad)
    # a byte is complete when 8 or more bits are pending: write it always,
    # keep it by moving `o` on only then
    let full = 0'u32 - (nbits shr 3)          # nbits <= 13, so 0 or 1 shifted
    nbits = nbits - (8'u32 and full)
    if o >= 0 and o < res.len:
      res[o] = byte((acc shr (nbits and 31'u32)) and 0xFF'u32)
    o = o + int(1'u32 and full)
  # the bits after the last whole byte must be zero padding
  bad = bad or not isZeroMask(acc and ((1'u32 shl (nbits and 31'u32)) - 1'u32))
  let tail = letters mod 4
  let padOk = (tail == 0 and pads == 0) or (tail == 2 and pads == 2) or
              (tail == 3 and pads == 1)
  if bad != 0'u32 or not padOk or o < 0 or o > res.len:
    wipe(res)
    res.setLen(0)
    result = false
  else:
    # the slot after the last byte may hold a partial one: clear it first
    for i in o ..< res.len:
      volatileStore(addr res[i], 0'u8)
    res.setLen(o)
    result = true

#
#
#            Nim's Runtime Library
#        (c) Copyright 2013 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Nim OID support. An OID is a global ID that consists of a timestamp,
## a unique counter and a random value. This combination should suffice to
## produce a globally distributed unique ID.
##
## This implementation calls `initRand()` for the first call of
## `genOid`.

import std/[atomics, endians, hashes, random, times]

{.feature: "staticContracts".}

type
  Oid* = object ## An OID.
    time: int64
    fuzz: int32
    count: int32

func `==`*(oid1, oid2: Oid): bool {.inline.} =
  ## Compares two OIDs for equality.
  oid1.time == oid2.time and oid1.fuzz == oid2.fuzz and
    oid1.count == oid2.count

func hash*(oid: Oid): Hash =
  ## Generates the hash of an OID for use in hashtables.
  var h: Hash = 0
  h = h !& hash(oid.time)
  h = h !& hash(oid.fuzz)
  h = h !& hash(oid.count)
  !$h

func hexbyte*(hex: char): int {.inline,
    ensures: 0 <= result and result <= 15.} =
  ## Returns the value of a hexadecimal digit, or 0 when `hex` is invalid.
  case hex
  of '0'..'9': result = ord(hex) - ord('0')
  of 'a'..'f': result = ord(hex) - ord('a') + 10
  of 'A'..'F': result = ord(hex) - ord('A') + 10
  else: result = 0
  {.assume: 0 <= result and result <= 15.}

func parseOid*(str: cstring): Oid =
  ## Parses an OID.
  result = Oid()
  # The text stores the low 32 bits of `time`, then `fuzz` and `count`;
  # the layout skips the high half of `time`.
  var bytes = [0'u8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
  for i in 0 ..< 12:
    bytes[i] = uint8((hexbyte(str[2 * i]) shl 4) or hexbyte(str[2 * i + 1]))
  copyMem(cast[pointer](cast[int](addr(result.time)) + 4), addr bytes[0], 12)

func `$`*(oid: Oid): string =
  ## Converts an OID to a string.
  const hex = "0123456789abcdef"
  result = newStringOfCap(24)
  var o = oid
  var bytes = cast[cstring](cast[pointer](cast[int](addr(o)) + 4))
  for i in 0 ..< 12:
    let b = bytes[i].ord
    result.add hex[(b and 0xF0) shr 4]
    result.add hex[b and 0xF]

let
  t = getTime().toUnix

var
  seed = initRand(t)
  incr: int = seed.rand(int.high)

let fuzz = cast[int32](seed.rand(high(int)))

proc genOid*(): Oid =
  ## Generates a new OID.
  runnableExamples:
    doAssert ($genOid()).len == 24
  var time = getTime().toUnix
  var count = cast[int32](atomicFetchAdd(incr, 1) + 1)
  var beTime = 0'i64
  var beCount = 0'i32
  bigEndian64(addr beTime, addr time)
  bigEndian32(addr beCount, addr count)
  Oid(time: beTime, fuzz: fuzz, count: beCount)

func generatedTime*(oid: Oid): Time =
  ## Returns the generated timestamp of the OID.
  var tmp = 0'i64
  var dummy = oid.time
  bigEndian64(addr tmp, addr dummy)
  fromUnix(tmp)

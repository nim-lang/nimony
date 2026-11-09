#
#
#            Nim's Runtime Library
#        (c) Copyright 2012 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Helpers for converting values between different byte orders
## (`endian`:idx:).
##
## Endianness is the order of bytes of a value in memory. Big-endian means that
## the most significant byte is stored at the smallest memory address, while
## little-endian means that the least-significant byte is stored at the
## smallest address. See also https://en.wikipedia.org/wiki/Endianness.
##
## The `*Endian*` routines copy `inp` to `outp`. Their raw-pointer signature
## cannot express the buffer sizes, so the caller must provide at least
## 2, 4, or 8 readable/writable bytes respectively.
##
## Unstable API.

runnableExamples:
  var a = [1'u8, 2, 3, 4]
  var b: array[4, uint8]
  swapEndian32(addr b, addr a)
  doAssert b == [4'u8, 3, 2, 1]

{.feature: "staticContracts".}

func bswap16(x: uint16): uint16 {.intrinsic: "Bswap".}
func bswap32(x: uint32): uint32 {.intrinsic: "Bswap".}
func bswap64(x: uint64): uint64 {.intrinsic: "Bswap".}

func swapEndian64*(outp, inp: pointer) {.inline.} =
  ## Copies `inp` to `outp`, reversing the byte order.
  ## Both buffers are supposed to contain at least 8 bytes.
  var tmp = 0'u64
  copyMem(addr tmp, inp, 8)
  tmp = bswap64(tmp)
  copyMem(outp, addr tmp, 8)

func swapEndian32*(outp, inp: pointer) {.inline.} =
  ## Copies `inp` to `outp`, reversing the byte order.
  ## Both buffers are supposed to contain at least 4 bytes.
  var tmp = 0'u32
  copyMem(addr tmp, inp, 4)
  tmp = bswap32(tmp)
  copyMem(outp, addr tmp, 4)

func swapEndian16*(outp, inp: pointer) {.inline.} =
  ## Copies `inp` to `outp`, reversing the byte order.
  ## Both buffers are supposed to contain at least 2 bytes.
  var tmp = 0'u16
  copyMem(addr tmp, inp, 2)
  tmp = bswap16(tmp)
  copyMem(outp, addr tmp, 2)

when defined(bigEndian):
  func littleEndian64*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 64-bit little-endian order.
    ## Both buffers are supposed to contain at least 8 bytes.
    swapEndian64(outp, inp)

  func littleEndian32*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 32-bit little-endian order.
    ## Both buffers are supposed to contain at least 4 bytes.
    swapEndian32(outp, inp)

  func littleEndian16*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 16-bit little-endian order.
    ## Both buffers are supposed to contain at least 2 bytes.
    swapEndian16(outp, inp)

  func bigEndian64*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 64-bit big-endian order.
    ## Both buffers are supposed to contain at least 8 bytes.
    copyMem(outp, inp, 8)

  func bigEndian32*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 32-bit big-endian order.
    ## Both buffers are supposed to contain at least 4 bytes.
    copyMem(outp, inp, 4)

  func bigEndian16*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 16-bit big-endian order.
    ## Both buffers are supposed to contain at least 2 bytes.
    copyMem(outp, inp, 2)
else:
  func littleEndian64*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 64-bit little-endian order.
    ## Both buffers are supposed to contain at least 8 bytes.
    copyMem(outp, inp, 8)

  func littleEndian32*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 32-bit little-endian order.
    ## Both buffers are supposed to contain at least 4 bytes.
    copyMem(outp, inp, 4)

  func littleEndian16*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 16-bit little-endian order.
    ## Both buffers are supposed to contain at least 2 bytes.
    copyMem(outp, inp, 2)

  func bigEndian64*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 64-bit big-endian order.
    ## Both buffers are supposed to contain at least 8 bytes.
    swapEndian64(outp, inp)

  func bigEndian32*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 32-bit big-endian order.
    ## Both buffers are supposed to contain at least 4 bytes.
    swapEndian32(outp, inp)

  func bigEndian16*(outp, inp: pointer) {.inline.} =
    ## Copies `inp` to `outp`, storing it in 16-bit big-endian order.
    ## Both buffers are supposed to contain at least 2 bytes.
    swapEndian16(outp, inp)

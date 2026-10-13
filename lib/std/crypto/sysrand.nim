#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## Cryptographically secure random bytes from the operating system.
##
## Key material must come from here and never from `std/random`, whose
## generator is fast, seedable and therefore predictable.

{.feature: "staticContracts".}

import std/syncio

proc urandom*(dest: var openArray[byte]): bool =
  ## Fills `dest` from the kernel's CSPRNG (`/dev/urandom`, which on Linux
  ## never blocks once the pool is initialized and never runs dry). `false`
  ## if it could not be read; a caller must not proceed with a key then.
  result = false
  if dest.len == 0: return true
  var f: File
  if not open(f, "/dev/urandom", fmRead): return
  let n = readBuffer(f, addr dest[0], dest.len)
  close(f)
  result = n == dest.len

# `volatileLoad`/`volatileStore` — the interface is Nim's `std/volatile`, so the
# names and signatures are the ones existing source already writes. What differs
# is that the access IS the intrinsic here, rather than a cast a C compiler is
# asked to honour.
#
# The guarantee is structural and not a value: every access below yields exactly
# what an ordinary read would, so a test can only check that the program still
# MEANS what it says. What it pins is that the two intrinsics exist, typecheck
# generically, round-trip through the pointee's own width, and compile on the C
# backend as well as the native ones.

import std / [volatile, syncio]

var cell32: uint32 = 0
var cell8: uint8 = 0

proc roundTrip =
  volatileStore(addr cell32, 0x11223344'u32)
  echo volatileLoad(addr cell32)
  # A sub-word cell is accessed at ITS width, not widened to a machine word: for
  # a device register the neighbouring byte is another register.
  volatileStore(addr cell8, 0xAB'u8)
  echo volatileLoad(addr cell8)

proc throughAPointer(p: ptr uint32): uint32 =
  # Generic over the cell type, so a helper that takes the pointer works too.
  volatileStore(p, volatileLoad(p) + 1'u32)
  result = volatileLoad(p)

roundTrip()
echo throughAPointer(addr cell32)

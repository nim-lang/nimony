# `..` requires `b < high(int)`. `high(s)` is `len(s) - 1`, and `len`'s
# `.ensures` bounds it by `MaxContainerLen`, so `0 .. s.high` proves for a
# `string` as for a `seq`. The string `high` is a non-generic routine of
# another module, read through `let tmp = len(s); result = tmp - 1`; the
# bound of `len(s)` has to come along. The unprovable side is
# `tests/nimony/errmsgs/tstringhigh_past.nim`.

{.feature: "staticContracts".}

import std/syncio
import deps / mhighs

func ownHigh(s: string): int {.alwaysInline.} = len(s) - 1

proc stringHigh(s: string): int =
  result = 0
  for i in 0 .. s.high: inc result

proc seqHigh(s: seq[int]): int =
  result = 0
  for i in 0 .. s.high: inc result

proc lenMinusOne(s: string): int =
  result = 0
  for i in 0 .. s.len - 1: inc result

proc ownAccessor(s: string): int =
  result = 0
  for i in 0 .. ownHigh(s): inc result

proc importedAccessor(s: string): int =
  result = 0
  for i in 0 .. lastIndex(s): inc result

proc importedOffset(s: string): int =
  # `len(s) + MaxContainerLen` is at most `high(int) - 1`
  result = 0
  for i in roomy(s) - 2 .. roomy(s): inc result

echo stringHigh("abc"), " ", stringHigh(""), " ", seqHigh(@[1, 2]), " ",
  lenMinusOne("abcd"), " ", ownAccessor("ab"), " ", importedAccessor("xyz"),
  " ", importedOffset("")

# Native-codegen golden: an `.inline` boolean proc used directly as an `if`
# and `while` condition must fuse into arkham's compare-and-branch (`cmp;jcc`)
# — the shoggoth inliner splices the comparison straight into the guard, so no
# boolean temp is materialised and re-tested. See the checked-in
# `tinlinecond.<target>.asm.nif` (one golden per target — machine code is the
# point): `classify`/`loop` contain `(cmp …)(jg …)` with no call to `isLe` and
# no 0/1 materialisation.

import std/syncio

proc isLe(a, b: int): bool {.inline.} =
  result = a <= b

proc classify(a, b: int): int =
  if isLe(a, b):
    result = 1
  else:
    result = 2

proc loop(n: int): int =
  result = 0
  var i = 0
  while isLe(i, n):
    result = result + classify(i, 2)
    inc i

echo loop(3)

# Driver for the incremental suite's compile-time evaluation phase: `Head` is
# folded by the compile-time evaluator, whose program links against the
# nimcache's `std/writenif`, and `Width` by a program that carries this
# module's `width`. The test machinery edits the slice bound and the body of
# `width` and restores this file in place, so anything depending on its exact
# content lives in `src/hastur/incrementaltests.nim`.
import std/syncio

func width(): int = 3

const
  Digits = "0123456789abcdef"
  Head = "head-" & Digits[0 ..< 4]
  Width = width()

echo Head
echo "width-", Width

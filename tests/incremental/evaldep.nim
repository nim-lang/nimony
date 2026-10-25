# Driver for the incremental suite's stale-writenif phase: `Head` is folded by
# the compile-time evaluator, whose program links against the nimcache's
# `std/writenif`. The test machinery edits the slice bound and restores this
# file in place, so anything depending on its exact content lives in
# `src/hastur/incrementaltests.nim`.
import std/syncio

const
  Digits = "0123456789abcdef"
  Head = "head-" & Digits[0 ..< 4]

echo Head

# `getStackTrace()` on the native backend, end to end: nifasm's per-proc table,
# the `{.naked.}` seed proc that hands back the CALLER's frame, and the walk
# over both — see `lib/std/stacktraces`.
#
# The test asserts the ORDER of the frames rather than the exact text: the
# innermost frame comes first, and every caller follows in turn. That is the
# property the walk can get wrong (a frame skipped, the chain reversed, the
# trace stopping after one), and it is checked without pinning names the
# name-prettifier may reasonably change.
#
# It runs on the C backend too, where `stackTracesAvailable` is false and the
# answer is the empty string — a stack trace there means reading the C
# compiler's unwind tables, which is a different implementation rather than a
# missing branch of this one. Same verdict line either way, so one `.output`
# covers both.

import std/[syncio, stacktraces]

proc indexOf(s, sub: string): int =
  result = -1
  if sub.len == 0 or sub.len > s.len: return
  for i in 0 .. s.len - sub.len:
    var ok = true
    for j in 0 ..< sub.len:
      if s[i+j] != sub[j]:
        ok = false
        break
    if ok: return i

proc leaf(): string {.noinline.} =
  result = getStackTrace()

proc middle(): string {.noinline.} =
  result = leaf()

proc outer(): string {.noinline.} =
  result = middle()

let tr = outer()

when stackTracesAvailable:
  let a = indexOf(tr, "leaf")
  let b = indexOf(tr, "middle")
  let c = indexOf(tr, "outer")
  let d = indexOf(tr, "main")
  if a == 0 and b > a and c > b and d > c:
    echo "stack trace ok"
  else:
    echo "unexpected stack trace:"
    echo tr
else:
  if tr.len == 0:
    echo "stack trace ok"
  else:
    echo "expected no stack trace, got:"
    echo tr

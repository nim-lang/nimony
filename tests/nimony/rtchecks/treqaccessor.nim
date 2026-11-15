# `lo(b.h)` of the callee is `uint8(q.h.x)`, which is 255 here, so the
# `.requires` guard must stay and fail at run time. Reading the accessor's
# conversion with the caller's facts about its own `s` (the same symbol as
# `lo`'s parameter: `filler` makes the numbering line up) dropped the guard.

import std/syncio
import deps/mreqaccessor

proc filler(s, z: I) = discard

proc p(s: I; q: J) =
  if s.x >= 0 and s.x < 10 and q.h.x < 10:
    g(q)

p(I(x: 0), J(h: I(x: -1)))

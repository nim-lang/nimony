# The proven side of `tests/nimony/errmsgs/tsubstframe.nim`: with the facts
# about the argument itself, an imported accessor's temps and a callee's
# `.requires` read with the arguments in place still prove, while the
# caller's own `s`, `b` and `a` share the callee's symbols.

{.feature: "staticContracts".}

import std/syncio
import deps/[msubstframe, msubstframe2]

proc filler(s: uint8; a: int) = discard

proc temp(s, t: uint8) =
  if t < 10:
    {.assert: nxt(t) > t.}
    echo nxt(t), " > ", t

proc nestedTemps(s, t: uint8) =
  if t < 10:
    {.assert: nxt2(t) > t.}
    echo nxt2(t), " > ", t

proc argPath(b, c: J) =
  if b.h.x < 10:
    g(b.h, c)

proc offsetPath(a, q: J; i: int) =
  # `pred(a.h)` of the callee is `q.h.x - 1`
  if q.h.x > 5 and q.h.x < 100 and i < 3 and i > 0:
    gp(q, i)

proc nested(s, t: uint8) =
  # accessors applied to accessors, across two modules: each argument is
  # read once, in the caller's terms
  if t < 10:
    {.assert: base2(base2(t)) < 10.}
    {.assert: idm(base2(t)) < 10.}
    {.assert: base2(idm(t)) < 10.}
    echo base2(base2(t)), " ", idm(base2(t)), " ", base2(idm(t))
  if base2(t) < 10:
    {.assert: nxt3(t) > t.}
    echo nxt3(t), " > ", t

filler(0, 0)
temp(255, 9)
nestedTemps(255, 9)
argPath(J(h: I(x: 3)), J(h: I(x: 100)))
offsetPath(J(h: I(x: 0)), J(h: I(x: 50)), 2)
nested(0, 9)

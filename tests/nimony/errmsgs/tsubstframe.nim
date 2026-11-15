# A callee's symbols and the caller's are numbered apart, so the same symbol
# can stand for both. The prover reads the callee's contract or an imported
# accessor's body with the arguments in place of the parameters: an
# accessor's temp (`x.0 = base(s)`) is in the accessor's terms and needs the
# substitution too, an argument is in the caller's terms and must not get it
# again, and an accessor applied to a path of the callee (`lo(b.h)`) has its
# parameter stand for that path in the caller's terms. Every claim below is
# false for some input; with a symbol read in the wrong routine each was
# proven from the caller's facts about its own `s`, `b` or `a`.
#
# The bugs need the numbering to line up: `filler` and the order of the
# routines here make the callers' `s.1`, `s.3`, `b.0` and `a.1` the symbols
# of `deps/msubstframe.nim`'s `nxt`, `lo`, `g` and `gp`. The proven
# neighbours are in `tests/nimony/contracts/tsubstframe.nim`.

{.feature: "staticContracts".}

import deps/msubstframe

proc filler(s: uint8; a: int) = discard

proc temp(s, t: uint8) =
  # `nxt(255) == 0`
  if s < 10:
    {.assert: nxt(t) > t.}

proc nestedTemps(s, t: uint8) =
  if s < 10:
    {.assert: nxt2(t) > t.}

proc argPath(b, c: J) =
  # `a.x` is `b.h.x`, not `c.h.x`
  if c.h.x < 10:
    g(b.h, c)

proc accessorPath(s: I; q: J) =
  # `lo(b.h)` is `uint8(q.h.x)`, not `uint8(s.x)`
  if s.x >= 0 and s.x < 10 and q.h.x < 10:
    gl(q)

proc offsetPath(a, q: J; i: int) =
  # `pred(a.h)` is `q.h.x - 1`, not `a.h.x - 1`
  if a.h.x > 5 and a.h.x < 100 and i < 3 and i > 0:
    gp(q, i)

filler(0, 0)
temp(0, 255)
nestedTemps(0, 255)
argPath(J(h: I(x: 100)), J(h: I(x: 0)))
accessorPath(I(x: 0), J(h: I(x: -1)))
offsetPath(J(h: I(x: 50)), J(h: I(x: 0)), 2)

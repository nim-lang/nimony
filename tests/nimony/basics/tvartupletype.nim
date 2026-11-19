# `let (a, b): T = x`: the value is typed as `T`, so `(1, 2)` is `(1.0, 2.0)`
# for `(float, float)`, and each name gets its part of it -- also when `T` is
# a named tuple type, and for the names of a nested tuple.
import std/syncio

proc f(): (int, float) = (1, 2.0)

type P = tuple[x, y: float]

let (a, b): (int, float) = f()
var (c, d): (float, float) = (1, 2)
let (e, g): tuple[x: int, y: string] = (3, "x")
let (h, k): P = (1, 2)
let ((m, o), q): ((int, float), int) = ((1, 2), 3)
let ((r, s): (int, int), t) = ((1, 2), 3)
let ((u, v), w): (P, int) = ((1, 2), 3)
let (x, y) = (7, 8)
echo a, " ", b, " ", c, " ", d, " ", e, " ", g, " ", h, " ", k
echo m, " ", o, " ", q, " ", r, " ", s, " ", t, " ", u, " ", v, " ", w, " ", x, " ", y

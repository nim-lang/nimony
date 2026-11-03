# A const initializer that calls a proc which calls further procs of the same
# module: the compile-time sub-compile must declare the callees first.
import std/syncio
type V = object
  x, y: float64
proc mk(x, y: float64): V = V(x: x, y: y)
proc add(a, b: V): V = mk(a.x + b.x, a.y + b.y)
proc twice(a: V): V = add(a, a)
type E = enum ea, eb
proc pick(): set[E] = {eb}
const c = twice(mk(1.5, 2.0))
const s = pick()
echo c.x, " ", c.y, " ", eb in s

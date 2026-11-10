# Two `ref`s may be one object, and a `var` parameter may be the object behind
# a `ref`: the call-site aliasing check compares the selections after the
# dereferences (doc/language.md, "Call-site aliasing checks").
type
  O = ref object
    s: seq[int]
    t: seq[int]
  P = object
    n: int
  Base = ref object of RootObj
    n: int
  Derived = ref object of Base
    m: int
  Other = ref object
    n: int

proc gotcha(s: var seq[int]; e: var int) =
  s = @[]
  e = 5
proc g(a, b: var int) =
  a = 1
  b = 2

proc refs(r1, r2: O) =
  gotcha(r1.s, r2.s[0])     # Error: `r2` may be `r1`
  gotcha(r1.s, r2.t[0])     # OK: different fields

proc varParam(a: var P; r: ref P) =
  g(a.n, r.n)               # Error: `a` may be `r[]`

proc related(b: Base; d: Derived; o: Other) =
  g(b.n, d.n)               # Error: a `Derived` is a `Base`
  g(b.n, o.n)               # OK: unrelated types

proc sameRoot(x: O) =
  gotcha(x.s, x.t[0])       # OK: different fields

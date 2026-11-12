# A write to one `var` parameter may change another: a caller in a
# `lenientAliasing` module may pass one variable twice
# (doc/language.md, "Aliasing"). The index check must stay.
{.feature: "lenientAliasing".}
{.feature: "assumeSync".}
import std/syncio

type Obj = object
  n: int
var arr: array[10, int]

proc two(a, b: var Obj): int =
  result = 0
  if a.n >= 0 and a.n < 10:
    b.n = 1000000000
    result = arr[a.n]

var o = Obj(n: 3)
echo two(o, o)

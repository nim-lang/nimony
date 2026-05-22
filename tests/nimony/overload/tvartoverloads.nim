{.feature: "varToverloads".}

import std/syncio

proc foo(x: int): string = "T"
proc foo(x: var int): string = "varT"

var v = 0
let l = 0

echo foo(v)     # mutable lvalue: tiebreaker picks the `var T` overload
echo foo(l)     # immutable: `var T` candidate is rejected at sigmatch
echo foo(42)    # literal: same, only the `T` candidate is left

proc viaVarParam(p: var int) = echo foo(p)
proc viaLetParam(p: int) = echo foo(p)

viaVarParam(v)
viaLetParam(99)

type Box = object
  n: int

var b = Box(n: 1)
let lb = Box(n: 2)
echo foo(b.n)    # path through `var` object: still a mutable lvalue
echo foo(lb.n)   # path through `let` object: not mutable

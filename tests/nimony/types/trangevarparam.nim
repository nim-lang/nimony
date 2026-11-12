# A `var`/`out` parameter of a range type takes an argument of that same range
# type, generic `var T` instantiates at the range type, a range value still
# passes by value to its base type, and a template's `var int` parameter takes
# a range argument (it is substituted, not written through: the expansion's
# assignments are range-checked as written). The refused forms are in
# `tests/nimony/errmsgs/trangevarargs.nim`.
import std/syncio

type
  R = range[0..7]
  R2 = range[0..7]

proc setR(x: var R; v: R) = x = v
proc outR(x: out R) = x = 6
proc setG[T](x: var T; v: T) = x = v
proc plusOne(x: int): int = x + 1
proc setIt(x: var int; v: int) = x = v

var r: R = 0
setR(r, 5)
echo r
outR(r)
echo r
var r2: R2 = 1
setR(r2, 4)        # the same range under another name
setG(r2, R2(3))
echo r2
var a: R = 1
var b: R = 5
swap(a, b)
echo a, " ", b
var n: Natural = 2
var p: Natural = 9
swap(n, p)
echo n, " ", p
echo plusOne(r)
# the style for a callee that writes an `int`: a local copy, range-checked back
var tmp = int(n)
setIt(tmp, 7)
if tmp >= 0: n = tmp
echo n

template bump(x: var int) = x = x + 1
template bumpRange(x: var range[0..10]) = x = x + 1
var t: R = 3
bump(t)
echo t
var s: range[2..5] = 2
bumpRange(s)
echo s

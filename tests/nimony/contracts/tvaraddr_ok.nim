# What stays legal for a `var` parameter: its location leaves as the routine's
# `var T` result, and its address is used for the call (doc/language.md,
# "Aliasing").
import std/syncio

type Box = object
  a, b: int

proc forward(x: var int): var int = x
proc second(x: var Box): var int = x.b
proc use(p: ptr int) = p[] = 7

proc ok(x: var int; b: var Box) =
  use(addr x)                  # a call argument
  let p = addr x               # a local pointer
  p[] = 3
  use p
  forward(x) = 4
  second(b) = 5
  copyMem(addr b.a, addr b.b, sizeof(int))

var i = 0
var bx = Box(a: 1, b: 2)
ok(i, bx)
echo i, " ", bx.a, " ", bx.b

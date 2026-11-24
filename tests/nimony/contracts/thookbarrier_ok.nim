# A hook written as a `func` is `.noSideEffect`, so the `=destroy` of `r` is
# no barrier: what is known about the `var` parameter `x` survives it and
# proves `needsPos`'s contract. With a `proc` hook this does not compile.

{.feature: "staticContracts".}

type
  Res = object
    p: nil ptr int

func `=destroy`(r: Res) =
  if r.p != nil: dealloc(r.p)
proc `=wasMoved`(r: var Res) = r.p = nil

func needsPos(x: int) {.requires: x > 0.} = discard

proc use(x: var int) =
  if x > 0:
    block:
      var r = Res(p: nil)
      discard r
    needsPos(x)

var a = 1
use(a)

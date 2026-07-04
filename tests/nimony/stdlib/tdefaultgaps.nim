# default() gaps surfaced by the sumi nim3 port: cstring fields and
# Opt[T] fields inside object construction / default() expansion, plus
# Opt.getOrQuit (the no-{.raises.} counterpart of unsafeGet, mirroring
# tables.getOrQuit).
{.feature: "lenientnils".}

import std/[syncio, assertions, opt]

type
  WithCString = object
    name: cstring          # default(cstring) — was unresolvable
    n: int

  WithOpt = object
    idx: Opt[uint32]       # default(Opt[T]) — sum types had no default
    label: string

block: # cstring field defaults to nil through T() and default(T)
  let a = WithCString()
  assert a.name == nil and a.n == 0
  let b = default(WithCString)
  assert b.name == nil

block: # Opt field defaults to None through T() and default(T)
  let a = WithOpt()
  assert a.idx.isNone and a.label == ""
  let b = default(WithOpt)
  assert b.idx.isNone
  let c = default(Opt[int])
  assert c.isNone

block: # getOrQuit returns the value on Some (None terminates, untested here)
  let x = some(42'u32)
  assert x.getOrQuit() == 42'u32
  var o = WithOpt(idx: some(7'u32))
  assert o.idx.getOrQuit() == 7'u32

echo "ok"

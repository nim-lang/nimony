# In a type position a name means its one TYPE candidate: `Symbol` is both an
# enum field (`mchoicekind`) and a type (`mchoicesym`), as with nifcore's
# `NifKind.Symbol` next to nifasm's `Symbol`.
import std/syncio
import deps/[mchoicekind, mchoicesym]

type
  Holder = object
    s: Symbol
    all: seq[Symbol]

proc describe(s: Symbol): string = s.name

proc main =
  let a = Symbol(name: "a")
  var h = Holder(s: a, all: @[a, Symbol(name: "b")])
  var local: Symbol = h.all[1]
  echo describe(h.s), " ", h.all.len, " ", local.name
  let k: Kind = Symbol     # the enum field, narrowed by the expected type
  echo ord(k)

main()

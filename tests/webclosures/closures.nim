## Closures called through wasm's `call_indirect`: every way a value reaches
## a closure slot, called from the declaring module and from an importer.
import std/syncio
import closlib

type Local = object
  f: proc (n: int): int {.closure.}

proc named(n: int): int = n + 1

proc capturing(k: int): Holder =
  result = Holder(f: proc (n: int): int {.closure.} = n + k)

proc main =
  let a = Holder(f: proc (n: int): int = n + 2)   # capture-less lambda
  let b = Holder(f: named)                          # named proc
  let c = capturing(3)
  var d = Holder(f: named)
  d.f = proc (n: int): int = n + 4                  # assigned lambda
  let e = Local(f: proc (n: int): int = n + 5)      # same-module type
  echo a.f(10), " ", b.f(10), " ", c.f(10), " ", d.f(10), " ", e.f(10)
  echo callHere(a, 10), " ", callHere(b, 10), " ", callHere(c, 10), " ", callHere(d, 10)

main()

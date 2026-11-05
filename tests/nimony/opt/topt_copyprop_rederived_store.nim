# A store through a pointer re-derived from another one is a write.
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_copyprop_rederived_store.nim
# Expected (see .output): `stored true` and `got true`.
#
# `Table.[]=` hands its `sink` value to `rawPut`, which moves it into the
# entry: `var tmp = v; =wasMoved(v); data.add (k, tmp)`. Inlined, the
# `=wasMoved` of the nested `Box` is two pointer temps,
# `d = haddr(v.b)` and `d2 = haddr((*d).p)`, and a store `*d2 = nil`.
# copyprop's read-only addr-taken classification (`computeStableAT`) saw `d`
# used only as the base of the `(deref d)` inside `d2`'s address and took it
# for a load; `d2` itself was not traced back to `v`. So `v` counted as never
# written, `tmp` was replaced by `v` after the store, and the table kept a nil
# ref. Every `std/dns` query under `--opt:speed` lost its reply this way: the
# waiter's channel sender came back nil from the pending table.

import std/[syncio, tables]

type
  Obj = ref object
    x: int
  Box = object
    p: nil Obj
  Entry = object
    b: Box

proc main() =
  var t = initTable[int, Entry]()
  let b = Box(p: Obj(x: 7))
  t[1] = Entry(b: b)
  for k, v in pairs(t):
    echo "stored ", v.b.p != nil
  let g = t.getOrDefault(1)
  echo "got ", g.b.p != nil

main()

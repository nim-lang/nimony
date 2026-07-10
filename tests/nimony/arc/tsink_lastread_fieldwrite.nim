## Regression: a `ref` consumed by an object constructor and then written
## through afterwards (`x.field = v`) must NOT be treated as its own last use.
##
## The move analyser used to `skip` an assignment's whole left-hand side, so a
## later `x.field = v` never counted as a read of the base `x`. A prior sink of
## `x` into an object-constructor field was then wrongly taken as the last use:
## `x` got `=wasMoved` to nil at the constructor and the following `x.field = v`
## dereferenced the emptied (nil) pointer — a segfault. Writing `x.field`
## reads/derefs the base, so the constructor must COPY (dup), keeping `x` live.
##
## The embedded destroy canary also proves each object is destroyed exactly once
## (a wrongful move would leak the extra reference or double-free).

import std / [syncio, assertions]

type
  InnerObj = object
    tag: int
    note: string
  Inner = ref InnerObj
  Outer = ref object
    inner: Inner
    id: int

var gDestroyed = 0
proc `=destroy`(x: InnerObj) =
  inc gDestroyed

proc build(): Outer =
  let inner = Inner(tag: 1, note: "orig")
  result = Outer(inner: inner, id: 7)   # `inner` consumed here — yet used again below
  inner.note = "changed"                # field WRITE through `inner`: reads the base
  inner.tag = 42

proc main() =
  block:
    let o = build()
    assert o.inner != nil
    assert o.inner.tag == 42
    assert o.inner.note == "changed"
    assert o.id == 7
  # `o` (and its single `inner`) left scope: exactly one destroy, no double-free.
  assert gDestroyed == 1
  echo "destroyed ", gDestroyed

main()

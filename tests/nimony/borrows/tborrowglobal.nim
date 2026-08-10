# A `lent` result may borrow from a GLOBAL: a global outlives every borrow of
# it, so its lifetime cannot be the thing that goes wrong. This is what lets an
# accessor fall back to a module-level default and still return `lent` — the
# shape `nifcore.pool` / `nifcore.tags` use, where returning the `ref` by value
# instead costs an RC increment at every one of their millions of calls.
#
# The other arm borrows through a `ref` field, which is the pre-existing
# `someIndirection` case. Borrowing from a LOCAL is still rejected.
import std / syncio

type
  Box = ref object
    v: int
  Inner = ref object
    b: Box
  Holder = object
    inner: Inner

var fallback = Box(v: -1)

proc get(h: Holder): lent Box =
  if h.inner != nil and h.inner.b != nil:
    return h.inner.b
  return fallback

proc main =
  let full = Holder(inner: Inner(b: Box(v: 7)))
  echo get(full).v
  var empty = Holder()
  echo get(empty).v

main()

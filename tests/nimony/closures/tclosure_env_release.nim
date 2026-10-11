# A closure's environment must release what it captured when the last
# reference to the closure goes away. The env is a `ref` to a compiler-made
# object deriving from RootObj, and the closure value only knows it as
# `(ref RootObj)`: its `=destroy` decremented the count and freed the block,
# but never ran the env's OWN destructor (RootObj has no fields, so the
# payload counted as trivial) — every captured string, ref or resource in
# every closure leaked. `=destroy(RootObj)` is a (empty) virtual method now,
# so the `ref RootObj` hook dispatches through the env's vtable.
{.feature: "lenientnils".}
import std/syncio
import deps/mclosureenvleak

proc touch(what: string; r: Resource) = echo what, r.name

proc fire(h: Holder) =
  # its own scope: the temp the call takes on the closure value dies here,
  # not at the end of the caller
  h.onEvent()

proc attach(h: var Holder; tag: string) =
  var res = Resource(name: tag)
  let cb = proc() {.closure.} = touch("event ", res)
  h.onEvent = cb

proc main() =
  var h = Holder()
  attach(h, "alpha")
  fire(h)
  clear(h) # last reference: the env goes, and with it "alpha"
  echo "cleared"
  attach(h, "beta")
  let copy = h.onEvent # a second reference
  clear(h)
  copy() # still alive through `copy`
  echo "scope end next"

main()
echo "ok"

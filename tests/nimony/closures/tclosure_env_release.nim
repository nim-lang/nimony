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

# A captured `sink` parameter moves into the env; the env's `=destroy` must
# release it like any other captured value (#348).
type Boxed = ref object
  r: Resource

proc useSink(b: sink Boxed) =
  let p = proc () {.closure.} = touch("sink ", b.r)
  p()

useSink(Boxed(r: Resource(name: "gamma")))
echo "after sink"

# ... and so must the frame of a closure iterator its own `sink` parameter.
iterator ownSink(b: sink Boxed): string {.closure.} =
  yield b.r.name

for s in ownSink(Boxed(r: Resource(name: "delta"))): echo s
echo "after iter"

import std/syncio
import deps/mhookcyc_a # a `{.cyclic.}` group is checked by its own driver

type
  Wrapper[T] = object
    val: T
  Box[T] = object
    item: T

converter toBox[T](w: Wrapper[T]): Box[T] =
  Box[T](item: w.val)

proc takeBox[T](b: Box[T]) = discard

type Holder[T] = object
  w: Wrapper[T]

proc `=destroy`[T](h: var Holder[T]) {.sideEffect.} =
  takeBox(h.w) # the first use of `Box[float32]` is in this hook instance

proc `=wasMoved`[T](h: var Holder[T]) = discard
var x = Holder[float32](w: Wrapper[float32](val: 1.0f))
echo x.w.val

# A type first used in a hook instance has hooks of its own, reached here
# through two generic procs: instantiating them takes a third round.
type
  Inner[T] = object
    v: T
  Outer[T] = object
    v: T

proc `=destroy`[T](x: var Inner[T]) {.sideEffect.} = echo "inner destroyed"
proc `=wasMoved`[T](x: var Inner[T]) = discard
proc g[T](x: T) =
  var i = Inner[T](v: x)
proc f[T](x: T) = g(x)

proc `=destroy`[T](h: var Outer[T]) {.sideEffect.} = f(h.v)
proc `=wasMoved`[T](h: var Outer[T]) = discard

proc main =
  var o = Outer[int](v: 3)
main()
useHolder()

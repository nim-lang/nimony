import std/syncio

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

proc `=destroy`[T](h: var Holder[T]) =
  takeBox(h.w) # Déclenche toBox[T] implicitement

proc `=wasMoved`[T](h: var Holder[T]) = discard
var x = Holder[float32](w: Wrapper[float32](val: 1.0f))
echo x.w.val
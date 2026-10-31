import std/syncio
import mcyc5_a {.cyclic.}

type
  Box*[T] = object
    x*: T

proc `=destroy`*[T](b: Box[T]) =
  echo "destroyed"

proc `=wasMoved`*[T](b: var Box[T]) =
  discard

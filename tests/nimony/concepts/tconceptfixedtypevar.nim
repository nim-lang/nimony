## Inside a checked generic the checked type's own typevars are fixed types: a
## container concept's parameter used only in a parameter position is inferred
## to be one of them, whether it is constrained or not.

import std/syncio

type
  Box[T] = object
    a: T

func put[T](b: var Box[T]; v: T) = b.a = v

type
  Puttable[T] = concept
    func put(x: var Self, v: T)

func store[T; C: Puttable[T]](a: var C; v: T) = put(a, v)

func free[T](a: var Box[T]; v: T) = store(a, v)
func constrained[T: SomeFloat](a: var Box[T]; v: T) = store(a, v)

var x = Box[int](a: 1)
free(x, 2)
var y = Box[float](a: 1.0)
constrained(y, 2.5)
echo x.a, " ", y.a

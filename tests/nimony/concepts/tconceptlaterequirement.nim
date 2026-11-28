## A container concept's parameter inferred by one requirement is seen by the
## requirements after it, also inside a checked generic whose own typevar is
## constrained (nim-lang/nimony#3206).

import std/syncio

type
  Box[T] = object
    a: T

func get[T](b: Box[T]): T = b.a
func put[T](b: var Box[T]; v: T) = b.a = v

type
  Gettable[T] = concept
    func get(x: Self): T
    func put(x: var Self, v: T)

func roundtrip[T; C: Gettable[T]](a: C): C =
  result = a
  put(result, get(a))

func viaGeneric[T: SomeFloat](a: Box[T]): Box[T] = roundtrip(a)

echo viaGeneric(Box[float](a: 2.5)).a

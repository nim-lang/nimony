# An override may have fewer side effects than the method it overrides, never
# more: a `func` may call a `.noSideEffect` method, and the override is what
# runs. Whatever the order of the declarations, through a class that does
# not override, across modules, and with generic classes and methods.
import std/syncio
import deps / mnoeffectbase

type
  A = ref object of RootObj
  B = ref object of A
  C = ref object of B
  Square = ref object of Shape
  G[T] = ref object of RootObj
    x: T
  H = ref object of G[int]
  K[T] = ref object of A

method m(a: A) {.base, noSideEffect.} = discard
method m(c: C) = echo "x"              # through B, which does not override

method early(c: C) = echo "x"          # before the method it overrides
method early(a: A) {.base, noSideEffect.} = discard

method area(s: Square): int = (echo "area"; 1)  # the base is imported

method g[T](x: G[T]) {.base, noSideEffect.} = discard
method g(h: H) = echo "x"              # the base is generic

method m[T](k: K[T]) = echo "x"        # the override is generic

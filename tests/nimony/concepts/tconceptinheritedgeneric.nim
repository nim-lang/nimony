import std/assertions

## Inherited concepts (`concept of`) check requirements structurally. Generic
## implementation routines must match concept requirements once the concrete
## type is known.

type
  Foo = concept
    proc `-`(a, b: Self): Self
    proc abs(a: Self): Self
    proc max(a, b: Self): Self

  WrappedFoo = concept of Foo

type DirectBox*[T: Foo] = object
  v: T

type WrappedBox*[T: WrappedFoo] = object
  v: T

type DirectFloat32 = DirectBox[float32]
type WrappedFloat32 = WrappedBox[float32]

when float32 is WrappedFoo:
  type CheckWrapped = WrappedFloat32
else:
  {.error: "float32 must satisfy WrappedFoo".}

assert DirectFloat32(v: 1.0'f32).v == 1.0'f32
assert WrappedFloat32(v: 2.0'f32).v == 2.0'f32

import std / [syncio, assertions]

type
  RootObj {.inheritable.} = object

type
  GenericObj[T] = object of RootObj
    x: T
  InheritGeneric1[T] = object of GenericObj[T]
    y: T
  InheritGeneric2 = object of GenericObj[int]
    z: string

type Writeable = concept
  proc write(f: File; x: Self): string

method foo[T: Writeable](x: GenericObj[T]) =
  echo "at base method: ", x.x
  echo "base: ", x of GenericObj[T]
  echo "inherited 1: ", x of InheritGeneric1[T]
  #echo "inherited 2: ", x of InheritGeneric2

method foo[T: Writeable](x: InheritGeneric1[T]) =
  echo "at inherited 1 method: ", x.x, ", ", x.y
  echo "base: ", x of GenericObj[T]
  echo "inherited 1: ", x of InheritGeneric1[T]
  #echo "inherited 2: ", x of InheritGeneric2

method foo(x: InheritGeneric2) =
  echo "at inherited 2 method: ", x.x, ", ", x.z
  echo "base: ", x of GenericObj[int]
  #echo "inherited 1: ", x of InheritGeneric1[int]
  echo "inherited 2: ", x of InheritGeneric2

# object constructors with inherited fields do not work yet
foo(GenericObj[int](x: 1))
foo(InheritGeneric1[int](x: 2, y: 3))
foo(GenericObj[float](x: 4.56))
foo(InheritGeneric1[float](x: 7.89, y: 10.11))
foo(InheritGeneric2(x: 12, z: "abc"))

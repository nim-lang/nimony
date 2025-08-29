import std / [syncio, assertions]

type
  GenericObj[T] = ref object of RootObj
    # object constructors with inherited fields do not work yet
    x: T
  InheritGeneric1[T] = ref object of GenericObj[T]
    y: T
  InheritGeneric2 = ref object of GenericObj[int]
    z: string

type Writeable = concept
  proc write(f: File; x: Self)

method foo[T: Writeable](x: GenericObj[T]) =
  echo "at base method: ", x.x
  echo "base check: ", x of GenericObj[T]
  echo "inherited 1 check: ", x of InheritGeneric1[T]
  echo "inherited 2 check: ", x of InheritGeneric2

method foo[T: Writeable](x: InheritGeneric1[T]) =
  echo "at inherited 1 method: ", x.x, ", ", x.y
  echo "base check: ", x of GenericObj[T]
  echo "inherited 1 check: ", x of InheritGeneric1[T]
  # correctly fails with "never a subtype":
  #echo "inherited 2 check: ", x of InheritGeneric2

method foo(x: InheritGeneric2) =
  echo "at inherited 2 method: ", x.x, ", ", x.z
  echo "base check: ", x of GenericObj[int]
  # correctly fails with "never a subtype":
  #echo "inherited 1 check: ", x of InheritGeneric1[int]
  echo "inherited 2 check: ", x of InheritGeneric2

let baseInt = GenericObj[int](x: 1)
let inherit1Int = InheritGeneric1[int](x: 2, y: 3)
let baseFloat = GenericObj[float](x: 4.56)
let inherit1Float = InheritGeneric1[float](x: 7.89, y: 10.11)
let inherit2 = InheritGeneric2(x: 12, z: "abc")

proc test[T](x: GenericObj[T]) =
  foo(x)

test(baseInt)
test(inherit1Int)
test(baseFloat)
test(inherit1Float)
test(inherit2)
# same test as:
#foo(baseInt)
#foo(GenericObj[int](inherit1Int))
#foo(baseFloat)
#foo(GenericObj[float](inherit1Float))
#foo(GenericObj[int](inherit2))

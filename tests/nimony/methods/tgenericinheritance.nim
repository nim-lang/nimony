import std / [syncio, assertions]

type
  RootObj {.inheritable.} = object

type
  GenericObj[T] = object of RootObj
    # object constructors with inherited fields do not work yet
    #x: T
  InheritGeneric1[T] = object of GenericObj[T]
    y: T
  InheritGeneric2 = object of GenericObj[int]
    z: string

type Writeable = concept
  proc write(f: File; x: Self): string

method foo[T: Writeable](x: GenericObj[T]) =
  echo "at base method"# : ", x.x
  echo "base check: ", x of GenericObj[T]
  echo "inherited 1 check: ", x of InheritGeneric1[T]
  echo "inherited 2 check: ", x of InheritGeneric2

method foo[T: Writeable](x: InheritGeneric1[T]) =
  echo "at inherited 1 method: ", #[x.x, ", ",]# x.y
  echo "base check: ", x of GenericObj[T]
  echo "inherited 1 check: ", x of InheritGeneric1[T]
  # correctly fails with "never a subtype":
  #echo "inherited 2 check: ", x of InheritGeneric2

method foo(x: InheritGeneric2) =
  echo "at inherited 2 method: ", #[x.x, ", ",]# x.z
  echo "base check: ", x of GenericObj[int]
  # correctly fails with "never a subtype":
  #echo "inherited 1 check: ", x of InheritGeneric1[int]
  echo "inherited 2 check: ", x of InheritGeneric2

foo(GenericObj[int](#[x: 1]#))
foo(InheritGeneric1[int](#[x: 2,]# y: 3))
foo(GenericObj[float](#[x: 4.56]#))
foo(InheritGeneric1[float](#[x: 7.89,]# y: 10.11))
foo(InheritGeneric2(#[x: 12,]# z: "abc"))

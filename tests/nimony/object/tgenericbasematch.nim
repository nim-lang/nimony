# issue #2724
import std/syncio
type
  Parent[T] = ref object of RootObj
    v: T
  Mid = ref object of Parent[int]
  Child {.final.} = ref object of Mid
  GChild[T] = ref object of Parent[T]

proc onlyInt(x: Parent[int]): int = x.v
proc anyP[T](x: Parent[T]): T = x.v
echo onlyInt(Child(v: 3)), " ", anyP(Child(v: 4)), " ", onlyInt(GChild[int](v: 5)), " ", anyP(GChild[string](v: "s"))

import std/syncio

type
  RootObj2* {.inheritable.} = object

type GenericObj*[T] = object of RootObj2

method foo*(x: RootObj2): string = "RootObj"
template name(_: typedesc[int]): string = "int"
template name(_: typedesc[float]): string = "float"
method foo*[T](x: GenericObj[T]): string {.untyped.} =
  concat("GenericObj ", name(T))

proc testFoo*(x: RootObj2) =
  echo foo(x)

testFoo(GenericObj[int]())

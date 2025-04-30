import std/syncio

type
  RootObj* {.inheritable.} = object

type GenericObj*[T] = object of RootObj

method foo*(x: RootObj): string = "RootObj"
template name(_: typedesc[int]): string = "int"
template name(_: typedesc[float]): string = "float"
method foo*[T](x: GenericObj[T]): string {.untyped.} =
  concat("GenericObj ", name(T))

proc testFoo*(x: RootObj) =
  echo foo(x)

testFoo(GenericObj[int]())

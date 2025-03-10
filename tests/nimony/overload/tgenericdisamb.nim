import std/syncio

type Foo[T] = object

proc foo[T](x: T) = echo "broad"
proc foo[T](x: Foo[T]) = echo "specific Foo[T]"

# test linearmatch:
proc fooPtr[T](x: T) = echo "broad ptr"
proc fooPtr[T](x: ptr T) = echo "ptr"
proc fooPtr[T](x: ptr Foo[T]) = echo "specific ptr Foo[T]"

proc bar[T](x: Foo[T]) =
  foo(123)
  foo(x)
  var i = 456
  fooPtr(i)
  fooPtr(addr i)
  fooPtr(addr x)

bar(Foo[int]())

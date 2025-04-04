import std/[nifply, syncio]

type
  Foo = object
  Bar = object

echo internalTypeName(Foo)
echo internalTypeName(Bar)

proc test[T](x: T) =
  echo internalTypeName(T)

var x = Foo()
test(x)
var y = Bar()
test(y)

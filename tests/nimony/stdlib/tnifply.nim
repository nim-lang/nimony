import std/[nifply, syncio]

type
  Foo = object
    fooFieldX: int

  Bar = object
    barFieldX: string
    barFieldY: float

echo internalTypeName(Foo)
echo internalTypeName(Bar)

proc test[T](x: T) =
  echo internalTypeName(T)

var x = Foo(fooFieldX: 123)
test(x)
var y = Bar(barFieldX: "bar field", barFieldY: 456)
test(y)

for name, f in internalFieldPairs(x):
  echo name, " = ", f

for name, f in internalFieldPairs(y):
  echo name, " = ", f

proc testInternalFieldPairs[T: object](x: T) =
  for name, f in internalFieldPairs(x):
    echo name, " = ", f

testInternalFieldPairs(x)
testInternalFieldPairs(y)

const DefaultBufSize = 8
block:
  var b = open(DefaultBufSize)
  b.toNif "hello"
  echo b.extract

block:
  var b = open(DefaultBufSize)
  b.toNif 123
  echo b.extract

block:
  var b = open(DefaultBufSize)
  b.toNif 123'u
  echo b.extract

block:
  var b = open(DefaultBufSize)
  b.toNif 123.456
  echo b.extract

block:
  var b = open(DefaultBufSize)
  b.toNif true
  echo b.extract

block:
  var b = open(DefaultBufSize)
  b.toNif false
  echo b.extract

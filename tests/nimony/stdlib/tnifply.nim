import std/[assertions, nifply, syncio]

type
  Foo = object
    fooFieldX: int

  Bar = object
    barFieldX: string
    barFieldY: float

  TestEnum = enum
    teX
    teY
    teZ

  FooBar = object
    foo: Foo
    bar: Bar
    en: TestEnum

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
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif "hello"
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif 123
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif 123'u
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif 123.456
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif true
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif false
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif teX
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  b.toNif Foo(fooFieldX: 123)
  b.toNif Bar(barFieldX: "bar field X", barFieldY: 321)
  b.toNif FooBar(foo: Foo(fooFieldX: 321), bar: Bar(barFieldX: "bar", barFieldY: 567.5), en: teY)
  echo b.extract

block:
  var b = nifBuilderOpen(DefaultBufSize)
  const TestString = "test string"
  b.toNif TestString
  const TestInt = 1234567
  b.toNif TestInt
  const TestUint = 1234567'u
  b.toNif TestUint
  const TestFloat = 0.125
  b.toNif TestFloat
  b.toNif false
  b.toNif true
  b.toNif teX
  b.toNif teY
  b.toNif teZ
  let testFoo = Foo(fooFieldX: 987)
  b.toNif testFoo
  let testBar = Bar(barFieldX: "bar field", barFieldY: 1.75)
  b.toNif testBar
  let testFooBar = FooBar(foo: Foo(fooFieldX: 4567), bar: Bar(barFieldX: "foobar", barFieldY: 2.625), en: teZ)
  b.toNif testFooBar

  var r = nifReaderOpenFromBuffer(b.extract)
  assert r.fromNif(string) == TestString
  assert r.fromNif(int) == TestInt
  assert r.fromNif(uint) == TestUint
  assert r.fromNif(float) == TestFloat
  assert not r.fromNif(bool)
  assert r.fromNif(bool)
  assert r.fromNif(TestEnum) == teX
  assert r.fromNif(TestEnum) == teY
  assert r.fromNif(TestEnum) == teZ
  let readFoo = r.fromNif(Foo)
  assert readFoo.fooFieldX == testFoo.fooFieldX
  let readBar = r.fromNif(Bar)
  assert readBar.barFieldX == testBar.barFieldX
  assert readBar.barFieldY == testBar.barFieldY
  var readFooBar = default FooBar
  r.fromNif readFooBar
  assert readFooBar.foo.fooFieldX == testFooBar.foo.fooFieldX
  assert readFooBar.bar.barFieldX == testFooBar.bar.barFieldX
  assert readFooBar.bar.barFieldY == testFooBar.bar.barFieldY
  assert readFooBar.en == testFooBar.en

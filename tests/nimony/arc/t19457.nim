import std/assertions

proc gcd(x, y: seq[int]): seq[int] =
  var
    a = x
    b = y
  while b[0] > 0:
    let c = @[a[0] mod b[0]]
    a = b
    b = c
  result = a

assert gcd(@[1], @[2]) == @[1]

type
  IrrelevantType = object

proc `=copy`(dest: var IrrelevantType, src: IrrelevantType) =
  discard

type
  Inner = object
    value: string
    someField: IrrelevantType

  Outer = object
    inner: Inner

iterator valueIt(self: Outer): Inner =
  yield self.inner

proc collect(self: Outer): seq[Inner] =
  result = @[]
  for value in self.valueIt():
    result.add value

proc getValues(self: var Outer): seq[Inner] =
  var peers = self.collect()
  result = peers

var outer = Outer()
outer.inner = Inner(value: "hello, world")

assert outer.collect()[0].value == "hello, world"
assert outer.inner.value == "hello, world"
assert outer.getValues()[0].value == "hello, world"

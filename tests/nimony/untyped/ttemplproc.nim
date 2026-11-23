import std / [syncio]

# Proc inside untyped template with inject:
template defineSayHello() {.untyped.} =
  proc sayHello(name: string) {.inject.} =
    echo "hello " & name

defineSayHello()
sayHello("world")

# Proc with return type inside untyped template:
template defineAdder() {.untyped.} =
  proc adder(a, b: int): int {.inject.} =
    result = a + b

defineAdder()
echo adder(3, 4)

# Proc using gensym (default) used inside the same template body:
template useLocalProc(x: int) {.untyped.} =
  proc helper(a: int): int =
    result = a * 2
  echo helper(x)

useLocalProc(5)

# Nested template inside untyped template:
template defineOps() {.untyped.} =
  proc double(x: int): int {.inject.} =
    result = x * 2
  template quadruple(x: int): int {.untyped, inject.} =
    double(double(x))

defineOps()
echo double(5)
echo quadruple(3)

# Proc literals declare no name (#338); the first template is never used:
template unusedLiteral(n: int) {.untyped.} =
  let f = proc (): int = n

template procLiteral(n: int) {.untyped.} =
  let f = proc (a: int): int = a + n
  echo f(1)

procLiteral(3)

# An object constructor's field name binds to no symbol, not to a proc or a
# local of the same name; the field here is inherited (#339):
type
  Top = ref object of RootObj
    value: float
  Leaf = ref object of Top
    c: float
  Other = object

proc value(o: Other): float = 0.0

template makeLeaf(T: untyped; v: float) {.untyped.} =
  echo T(value: v, c: 2.0).value

template makeLeafLocal(T: untyped) {.untyped.} =
  let value = 3.5
  echo T(value: value).value

makeLeaf(Leaf, 1.5)
makeLeafLocal(Leaf)

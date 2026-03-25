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

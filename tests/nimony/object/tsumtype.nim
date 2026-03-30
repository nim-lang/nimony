import std/[syncio, assertions]

type
  Node = ref object
    case
    of AddOpr, SubOpr:
      a, b: Node
    of Value:
      val: int

let v: Node = Value(val: 42)
assert v.val == 42

let add: Node = AddOpr(a: v, b: Value(val: 10))
assert add.a.val == 42
assert add.b.val == 10

proc eval(n: Node): int =
  case n
  of Value(val):
    result = val
  of AddOpr(a, b):
    result = eval(a) + eval(b)
  of SubOpr(a, b):
    result = eval(a) - eval(b)

assert eval(v) == 42
assert eval(add) == 52

let sub: Node = SubOpr(a: Value(val: 100), b: Value(val: 30))
assert eval(sub) == 70

let nested: Node = AddOpr(
  a: SubOpr(a: Value(val: 50), b: Value(val: 20)),
  b: Value(val: 5)
)
assert eval(nested) == 35

echo "tsumtype: OK"

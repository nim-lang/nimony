import std/[syncio, assertions]

type
  Node = ref object
    case
    of AddOpr, SubOpr:
      a, b: Node
    of Value:
      val: int

let v: Node = Value(val: 42)
case v
of Value:
  assert v.val == 42
else: discard

let v2 = Value(val: 42)
case v2
of Value:
  assert v2.val == 42
else: discard

let add: Node = AddOpr(a: v, b: Value(val: 10))
case add
of AddOpr:
  case add.a
  of Value:
    assert add.a.val == 42
  else: discard
  case add.b
  of Value:
    assert add.b.val == 10
  else: discard
else: discard

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

proc depth(n: Node): int =
  case n
  of Value(val):
    result = 0
  of {AddOpr, SubOpr}(a, b):
    let da = depth(a)
    let db = depth(b)
    if da > db:
      result = da + 1
    else:
      result = db + 1

assert depth(v) == 0
assert depth(add) == 1
assert depth(nested) == 2

echo "tsumtype: OK"

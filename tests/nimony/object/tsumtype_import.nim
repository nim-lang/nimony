import std/[syncio, assertions]
import deps/msumtype

let v: Node = Value(val: 42)
assert eval(v) == 42

let add: Node = AddOpr(a: Value(val: 10), b: Value(val: 20))
assert eval(add) == 30

let sub: Node = SubOpr(a: Value(val: 100), b: Value(val: 30))
assert eval(sub) == 70

proc negate(n: Node): Node =
  case n
  of Value(val):
    result = Value(val: -val)
  of AddOpr(a, b):
    result = SubOpr(a: negate(a), b: negate(b))
  of SubOpr(a, b):
    result = AddOpr(a: negate(a), b: negate(b))

assert eval(negate(v)) == -42
assert eval(negate(add)) == 10
assert eval(negate(sub)) == -130

echo "tsumtype_import: OK"

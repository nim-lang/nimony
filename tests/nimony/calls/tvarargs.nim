import std / [syncio, assertions]

proc echoSum(args: varargs[int]): int =
  result = 0
  for a in args: result += a

proc sumWith(base: int; args: varargs[int]): int =
  result = base
  for a in args: result += a

echo echoSum(1, 2, 3)
echo echoSum()
echo sumWith(100, 1, 2, 3)
echo sumWith(0)
assert echoSum(5, 5) == 10
assert sumWith(7, 1, 1, 1, 1, 1) == 12


type
  Obj = object

proc v(o: Obj; keys: varargs[string, `$`]; value: float) =
  discard

var o = Obj()
v(o, "a", "b", 1.0)

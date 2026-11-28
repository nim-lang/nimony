import std/[syncio, assertions]

type
  Res[T, E] = object
    case
    of Ok: v: T
    of Err: e: E

  # `Ok` is also a branch of `Res`: the type before the dot says which.
  Flip = object
    case
    of Ok: n: int
    of Bad: msg: string

  Tree[T] = ref object
    case
    of Leaf: val: T
    of Node: left, right: Tree[T]

  Shape = object
    case
    of Dot: x: int
    of Line: a, b: int

  IntRes = Res[int, string]

proc show(r: Res[int, string]): string =
  case r
  of Ok(v): result = "ok " & $v
  of Err(e): result = "err " & e

proc flipShow(f: Flip): string =
  case f
  of Ok(n): result = "flip ok " & $n
  of Bad(msg): result = "flip bad " & msg

proc total(t: Tree[int]): int =
  case t
  of Leaf(val): result = val
  of Node(left, right): result = total(left) + total(right)

proc area(s: Shape): int =
  case s
  of Dot(x): result = x
  of Line(a, b): result = b - a

# The type is written at the constructor, so a call argument needs no expected
# type from the callee:
assert show(Res[int, string].Ok(v: 5)) == "ok 5"
assert show(Res[int, string].Err(e: "no")) == "err no"

# An alias names the type as well as an instance does:
assert show(IntRes.Ok(v: 6)) == "ok 6"
assert show(IntRes.Err(e: "alias")) == "err alias"

let r = Res[int, string].Ok(v: 7)
assert show(r) == "ok 7"

# `Ok` of `Flip`, not of `Res`:
assert flipShow(Flip.Ok(n: 1)) == "flip ok 1"
assert flipShow(Flip.Bad(msg: "x")) == "flip bad x"

# A `ref` sum type and a type that is not generic:
assert total(Tree[int].Node(left: Tree[int].Leaf(val: 1), right: Tree[int].Leaf(val: 2))) == 3
assert area(Shape.Line(a: 2, b: 9)) == 7
assert area(Shape.Dot(x: 4)) == 4

# `_` stands for a type argument the fields determine, as `Ok(v: 5)` does
# for the `T` of `Res[T, E]`; only what the fields cannot tell is written:
assert show(Res[_, string].Ok(v: 8)) == "ok 8"
assert show(Res[int, _].Err(e: "wild")) == "err wild"

type
  Both[A, B] = object
    case
    of Pair:
      a: A
      b: B
    of Empty: discard

proc pairText(p: Both[int, string]): string =
  case p
  of Pair(a, b): result = $a & b
  of Empty: result = "empty"

assert pairText(Both[_, _].Pair(a: 1, b: "x")) == "1x"
assert pairText(Both[int, _].Pair(a: 2, b: "y")) == "2y"
assert pairText(Both[int, string].Empty()) == "empty"

# A literal takes the type written for it:
proc byteText(p: Both[uint8, string]): string =
  case p
  of Pair(a, b): result = $a & b
  of Empty: result = "empty"

assert byteText(Both[uint8, _].Pair(a: 5, b: "z")) == "5z"

# also for a `ref` sum type:
assert total(Tree[_].Leaf(val: 5)) == 5

echo "tsumtype_qualified: OK"

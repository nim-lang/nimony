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

echo "tsumtype_qualified: OK"

import std/[syncio, assertions]
import deps/msumtype_generic

# Explicit type annotation with imported generic sum type:
let a: Option[int] = Some(val: 42)
assert a.val == 42

let c: Option[int] = None()

# Cross-module type inference from field values:
let d = Some(val: 99)
assert d.val == 99

let e = Some(val: "hello")
assert e.val == "hello"

# Explicit type on constructor:
let b = Option[int](Some(val: 10))
assert b.val == 10

# Either needs explicit annotation since one branch can't infer the other param:
let f: Either[int, string] = Left(a: 42)
let g: Either[int, string] = Right(b: "world")
assert f.a == 42
assert g.b == "world"

# Generic object constructor inference (non-sum type):
let p = Pair(first: 1, second: 2)
assert p.first == 1
assert p.second == 2

let q = Pair(first: "x", second: "y")
assert q.first == "x"
assert q.second == "y"

# Pattern matching in generic procs (cross-module):
assert getOrDefault(a, 0) == 42
assert getOrDefault(c, 99) == 99
assert getOrDefault(d, 0) == 99

# Pattern matching with concept constraints on value sum types:
type
  Addable = concept
    proc `+`(a, b: Self): Self

  Expr[T: Addable] = object
    case
    of Lit:
      val: T
    of Add:
      lhs: T
      rhs: T

proc eval[T: Addable](e: Expr[T]): T =
  case e
  of Lit(val):
    result = val
  of Add(lhs, rhs):
    result = lhs + rhs

let expr1 = Add(lhs: 10, rhs: 20)
assert eval(expr1) == 30
assert eval(Lit(val: 42)) == 42

# Pattern matching with concept constraints on ref sum types:
type
  Tree[T: Addable] = ref object
    case
    of Leaf:
      val: T
    of Branch:
      left: Tree[T]
      right: Tree[T]

proc sum[T: Addable](t: Tree[T]): T =
  case t
  of Leaf(val):
    result = val
  of Branch(left, right):
    result = sum(left) + sum(right)

let t: Tree[int] = Branch(
  left: Leaf(val: 10),
  right: Leaf(val: 20))
assert sum(t) == 30
let leaf: Tree[int] = Leaf(val: 42)
assert sum(leaf) == 42

echo "tsumtype_generic: OK"

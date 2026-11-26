import std/[syncio, assertions, result]
import deps/msumtype_generic

# Explicit type annotation with imported generic sum type:
let a: Option[int] = Some(val: 42)
{.cast(uncheckedAccess).}:
  assert a.val == 42

let c: Option[int] = None()

# Cross-module type inference from field values:
let d = Some(val: 99)
{.cast(uncheckedAccess).}:
  assert d.val == 99

let e = Some(val: "hello")
{.cast(uncheckedAccess).}:
  assert e.val == "hello"

# Explicit type on constructor:
let b = Option[int](Some(val: 10))
{.cast(uncheckedAccess).}:
  assert b.val == 10

# Either needs explicit annotation since one branch can't infer the other param:
let f: Either[int, string] = Left(a: 42)
let g: Either[int, string] = Right(b: "world")
{.cast(uncheckedAccess).}:
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

# Without an expected type the constructor must infer `Tree[int]` -- the `ref`
# alias, not the split-off `Tree.Obj[int]` (#2480):
assert sum(Leaf(val: 7)) == 7
let inferredLeaf = Leaf(val: 5)
assert sum(inferredLeaf) == 5

# A sum type constructor takes the type its context expects, here a routine
# body's last expression.
type
  Res[T, E] = object
    case
    of Ok: v: T
    of Err: e: E

# The return type is the expected type of a routine body's last expression,
# as it already is for `result = ...` and `return ...`.
proc ok[T, E](x: T): Res[T, E] = Ok(v: x)
proc okInt(x: int): Res[int, string] = Ok(v: x)
proc failed(msg: string): Res[int, string] = Err(e: msg)

# Also when every branch of the last expression is a constructor.
proc pick(b: bool): Res[int, string] =
  if b: Ok(v: 5) else: Err(e: "off")

proc grade(n: int): Res[int, string] =
  case n
  of 0: Err(e: "zero")
  else: Ok(v: n)

proc show(r: Res[int, string]): string =
  case r
  of Ok(v): result = "ok " & $v
  of Err(e): result = "err " & e

let r: Res[int, string] = ok(1)
assert show(r) == "ok 1"
assert show(okInt(2)) == "ok 2"
assert show(failed("no")) == "err no"
assert show(pick(true)) == "ok 5"
assert show(pick(false)) == "err off"
assert show(grade(0)) == "err zero"
assert show(grade(7)) == "ok 7"

# `Result`'s fields are private to `std/result`; a pattern binds them in a
# generic and in a template of another module too
# (planetis-m/nimony-bugs-llms#87):
proc okOr[T](r: Result[T, string]; d: T): T =
  case r
  of Ok(v): result = v
  of Err(e): result = d

template errLen(r: Result[int, string]): int =
  var n = -1
  case r
  of Ok(v): n = v
  of Err(e): n = e.len
  n

assert okOr(result.ok[int, string](5), 0) == 5
assert okOr(err[int, string]("x"), 9) == 9
assert errLen(err[int, string]("bad")) == 3

# A pattern names a branch of the selector's own type, not the branch of
# that name of another sum type in scope, `Res` here:
proc flipMsg(f: Flip): string =
  case f
  of Ok(n): result = $n
  of {Err}(msg): result = msg

assert flipMsg(flipErr("flip")) == "flip"

# A constructor names a branch of the expected type, not the first branch
# of that name in scope (`Res`'s):
let flipped: Flip = Err(msg: "flop")
assert flipMsg(flipped) == "flop"
proc flipOk(n: int): Flip = Ok(n: n)
assert flipMsg(flipOk(3)) == "3"
type LocalFlip = object # declared after `Res`
  case
  of Err: lmsg: string
  of Ok: ln: int
let localOk: LocalFlip = Ok(ln: 4)
{.cast(uncheckedAccess).}:
  assert localOk.ln == 4

echo "tsumtype_generic: OK"

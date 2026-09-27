{.feature: "strictnew".}
# Tier 3 of `doc/internals/failure_modes.md`: outside a `.raises` routine an
# allocation has no channel to report failure on, so the construction's result is
# NILABLE and every use has to narrow it first. These are the forms that do.
import std / [syncio, assertions]

type
  Node = ref object
    val: int

  Tree = ref object
    case
    of Pair:
      a, b: Tree
    of Leaf:
      n: int

proc raising(): Node {.raises.} =
  # tier 1: hexer maps the `nil` to `ErrorCode.OutOfMemError` and raises it, which
  # is what lets the body go on treating the result as not-nil
  result = Node(val: 1)

proc viaReturn(): int =
  let p = Node(val: 2)
  if p == nil: return -1
  result = p.val

proc viaAssert(): int =
  let p = Node(val: 3)
  assert p != nil
  result = p.val

proc viaIf(): int =
  result = -1
  let p = Node(val: 4)
  if p != nil:
    result = p.val

proc eval(t: Tree): int =
  case t
  of Leaf(n): result = n
  of Pair(a, b): result = eval(a) + eval(b)

proc sumType(): int =
  # A sum type's branch constructor carries the sum type itself as its expected
  # type, so it needs the same relaxation as a plain `ref` -- otherwise there is
  # no spelling of the construction that narrows.
  let l1 = Leaf(n: 5)
  let l2 = Leaf(n: 6)
  if l1 == nil or l2 == nil: return -1
  let p = Pair(a: l1, b: l2)
  if p == nil: return -1
  result = eval(p)

try:
  echo raising().val
except:
  echo "out of memory"
echo viaReturn()
echo viaAssert()
echo viaIf()
echo sumType()

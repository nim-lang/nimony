# `doc/internals/failure_modes.md`: an allocation panics on out-of-memory unless
# the DESTINATION asks to be handed the `nil` instead, which it does by declaring
# itself `nil T`. So the rule is local -- the declaration says which it is -- and
# needs no new syntax: `nil T` is the same annotation that already governs fields
# and parameters, and the same prover then makes every dereference narrow.
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

proc plain(): int =
  # The default. Nothing to narrow: the construction panics rather than hand back
  # a `nil`, so the result is not-nil and the dereference is free.
  let p = Node(val: 1)
  result = p.val

proc optedIn(): int =
  let p: nil Node = Node(val: 2)
  if p == nil: return -1
  result = p.val

proc viaAssert(): int =
  let p: nil Node = Node(val: 3)
  assert p != nil
  result = p.val

proc mk(v: int): nil Node = Node(val: v)

proc fromConstructor(): int =
  # A constructor hands the `nil` on by declaring a nilable result: one narrowing
  # per call site, and no error channel, no ABI change and no `try`.
  let q = mk(4)
  if q == nil: return -1
  result = q.val

proc viaNew(): int =
  # `system.new` is generic, so its `out T` takes its nilability from the
  # instantiation site -- which is the one case no module-level opt-out can reach.
  var s: nil Node
  new(s)
  if s == nil: return -1
  result = 5

proc eval(t: Tree): int =
  case t
  of Leaf(n): result = n
  of Pair(a, b): result = eval(a) + eval(b)

proc sumType(): int =
  # A sum type's branch constructor takes the destination's nilability too. Note
  # what stays unannotated: the fields `a, b: Tree` are not-nil, so the nested
  # constructions panic and the tree literal needs no temporaries.
  let t: nil Tree = Pair(a: Leaf(n: 6), b: Leaf(n: 7))
  if t == nil: return -1
  result = eval(t)

echo plain()
echo optedIn()
echo viaAssert()
echo fromConstructor()
echo viaNew()
echo sumType()

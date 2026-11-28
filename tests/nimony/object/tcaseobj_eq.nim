import std/assertions

# `==` and `!=` on an object with a `case` section compare the
# discriminator and the fields of the active branch.

type
  K = enum kA, kB
  E = object
    case kind: K
    of kA: discard
    of kB: n: int

  Tree = object
    case kind: K
    of kA: discard
    of kB: n: int
    kids: seq[Tree]

proc same(a, b: Tree): bool =
  a == b

assert E(kind: kA) == E(kind: kA)
assert not (E(kind: kA) != E(kind: kA))
assert E(kind: kA) != E(kind: kB)
assert not (E(kind: kA) == E(kind: kB))
assert E(kind: kB, n: 1) == E(kind: kB, n: 1)
assert E(kind: kB, n: 1) != E(kind: kB, n: 2)
assert not (E(kind: kB, n: 1) == E(kind: kB, n: 2))

assert same(Tree(kind: kA), Tree(kind: kA))
assert not same(Tree(kind: kA), Tree(kind: kB))
assert same(Tree(kind: kB, n: 1, kids: @[Tree(kind: kA)]),
            Tree(kind: kB, n: 1, kids: @[Tree(kind: kA)]))
assert not same(Tree(kind: kB, n: 1, kids: @[Tree(kind: kA)]),
                Tree(kind: kB, n: 1, kids: @[Tree(kind: kB, n: 2)]))

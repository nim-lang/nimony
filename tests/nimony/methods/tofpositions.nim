# `x of T` must not depend on the syntactic position of the expression:
# the direct `of` magic passes the type as a `typedesc` *expression* (the bare
# alias symbol `A.0.`), while a re-semchecked `of` (inside a template such as
# `echo`) passes the expanded type. Both must resolve to the same class.

import std / [syncio, assertions]

type
  Base = ref object of RootObj
    id: int
  Mid = ref object of Base
    y: int
  Leaf = ref object of Mid
    z: int

  ValBase {.inheritable.} = object
    id: int
  ValLeaf = object of ValBase
    z: int

proc takesBool(name: string; got, want: bool) =
  echo (if got == want: "ok   " else: "WRONG"), " ", name

proc testRef(k: Base) =
  let bound = k of Leaf
  takesBool "let-bound", bound, true

  var vbound = k of Leaf
  takesBool "var-bound", vbound, true

  takesBool "proc-arg", k of Leaf, true

  if k of Leaf: echo "ok    if-condition"
  else: echo "WRONG if-condition"

  var spins = 0
  while (k of Leaf) and spins < 1:
    inc spins
  takesBool "while-condition", spins == 1, true

  takesBool "middle class", k of Mid, true
  takesBool "own class", k of Base, true
  echo "echo-inline: ", k of Leaf

proc testNegative(k: Base) =
  takesBool "not a Leaf", k of Leaf, false
  takesBool "is a Mid", k of Mid, true

proc testValue(o: ValBase) =
  let bound = o of ValLeaf
  takesBool "value let-bound", bound, true
  takesBool "value proc-arg", o of ValLeaf, true

var kids: seq[Base] = @[Base(Leaf(id: 1))]
testRef kids[0]
testNegative Base(Mid(id: 2))
testValue ValBase(ValLeaf(id: 3))

let self = Leaf(id: 4)
takesBool "statically exact", self of Leaf, true

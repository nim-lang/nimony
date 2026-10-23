import std / syncio

type
  Tree* = ref object
    v: int
  NodeObj* = object
    v: int
  Node* = ref NodeObj

proc newTree*(v: int): Tree = Tree(v: v)
proc newNode*(v: int): Node = Node(v: v)
proc write*(f: File; t: Tree) = write f, "tree " & $t.v
proc write*(f: File; n: Node) = write f, "node " & $n.v
proc weight*(t: Tree): int = t.v * 10

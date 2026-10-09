# owning edges form a forest: a type whose references are all `owned` or
# `.cursor` cannot be part of a cycle and stays out of the cycle collector.
{.feature: "ownedRefs".}
import std/[syncio, typetraits]
type
  Node = ref object
    next: owned nil Node
    data: int
  Plain = ref object
    next: nil Plain
  A = ref object
    b: owned nil B
  B = ref object
    a: nil A
  Tree = ref object
    kids: seq[owned Tree]
    parent {.cursor.}: nil Tree
  Widget = ref object
    onChange: owned proc () {.closure.}
  Widget2 = ref object
    onChange: proc () {.closure.}
  Env = ref object
    w: owned nil Widget
    x: owned nil Node

echo "Node ", canFormCycles(Node)
echo "Plain ", canFormCycles(Plain)
echo "A ", canFormCycles(A), " B ", canFormCycles(B)
echo "Tree ", canFormCycles(Tree)
echo "Widget ", canFormCycles(Widget), " Widget2 ", canFormCycles(Widget2)
echo "Env ", canFormCycles(Env)

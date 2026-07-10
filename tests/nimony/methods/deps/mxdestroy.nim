## Helper for txdestroy: an RTTI (method-carrying) ref-object hierarchy whose
## LabelObj embeds a canary field. The class itself defines NO user `=destroy` —
## the observable effect comes from the
## COMPILER-SYNTHESIZED object `=destroy` unravelling the canary field. A
## foreign module that destroys a Label must dispatch through the class vtable
## to this synthesized destroy; regression for "method `=destroy_...` not found
## in class" + wrong-slot dispatch.
{.feature: "lenientnils".}

type
  CanaryObj = object
    live: bool

var gDestroyed: int = 0
proc destroyCount*(): int = gDestroyed
proc `=destroy`(x: CanaryObj) =
  if x.live: inc gDestroyed

type
  NodeObj* = object of RootObj
    children*: seq[Node]
  Node* = ref NodeObj
  LabelObj* = object of NodeObj
    canary: CanaryObj
  Label* = ref LabelObj

method draw*(n: Node) {.base.} = discard
method draw*(n: Label) = discard

proc newLabel*(): Label = Label(canary: CanaryObj(live: true))
proc childCount*(n: Node): int = n.children.len

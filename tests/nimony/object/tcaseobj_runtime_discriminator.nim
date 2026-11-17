# A case object constructed with a RUNTIME discriminator names no branch field:
# the branch is not known when the `oconstr` is built, so every branch slot must
# start zeroed. The C back end gets that from its compound initializer; the
# native back end stores exactly what is listed, so the front end has to make
# the zeroing explicit before a partial constructor reaches it.
#
# The shape is `std/xmltree`'s: an element branch owns a `seq` of the same node.
# Without the zeroing, assigning that `seq` destroys whatever the fresh object
# held, which is what crashed the native XML tests. Both the heap path and the
# stack path are covered: a dirty stack slot is as uninitialized as fresh heap
# memory.
import std/syncio

type
  Kind = enum kText, kElement
  Node = ref NodeObj
  NodeObj {.acyclic.} = object
    case kind: Kind
    of kText:
      text: string
    of kElement:
      tag: string
      children: seq[Node]
    clientData: int

proc newNode(kind: Kind): Node {.noinline.} =
  Node(kind: kind)

proc newText(text: string): Node {.noinline.} =
  result = newNode(kText)
  result.text = text

proc newElement(tag: string): Node {.noinline.} =
  result = newNode(kElement)
  result.tag = tag
  result.children = @[]

proc scribble() {.noinline.} =
  # Leave recognizable garbage where `newNode`'s constructor temp will live; a
  # zero-filled stack would hide the bug.
  var a: array[32, int] = default(array[32, int])
  for i in 0 ..< a.len: a[i] = 0x24 + i
  var s = 0
  for i in 0 ..< a.len: s += a[i]
  if s == 0: echo "unreachable"

proc makeLocal(kind: Kind): NodeObj {.noinline.} =
  # Dirty the stack slot the local constructor is about to reuse.
  block:
    var a = default(array[16, int])
    for i in 0 ..< a.len: a[i] = 0x2400 + i
    if a[0] == 0: echo "unreachable"
  var o = NodeObj(kind: kind)
  o.clientData = 7
  case o.kind
  of kText: echo o.text.len
  of kElement: echo o.children.len
  result = o

proc replaceNode(kind: Kind): NodeObj {.noinline.} =
  result = NodeObj(kind: kText)
  result.text = "before"
  result = NodeObj(kind: kind)
  result.clientData = 9
  case result.kind
  of kText: echo result.text.len
  of kElement: echo result.children.len

scribble()
let g = newElement("root")
g.children.add newText("hello")
echo g.kind
echo g.tag
echo g.children.len
echo g.children[0].text
echo g.clientData

let local = makeLocal(kElement)
echo local.kind
echo local.clientData

let replaced = replaceNode(kElement)
echo replaced.kind
echo replaced.clientData

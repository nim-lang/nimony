# A constructor whose case selector is a RUNTIME value cannot name a branch,
# so it cannot be total (`oconstr`, doc/tags.md). Sem lowers it to a call of a
# hidden init proc that dispatches on the selector to one total constructor
# per branch: the branch's declared defaults hold, and the native back end,
# which stores exactly the fields listed, never sees garbage.
import std/syncio
import deps/mruntimecase

type
  Kind = enum kText, kElement, kComment, kPi
  Node = ref NodeObj
  NodeObj {.acyclic.} = object
    case kind: Kind
    of kText, kComment:
      text: string = "txt"
    of kElement:
      tag: string = "div"
      children: seq[Node]
    else:
      target: int = 77
    clientData: int

  Box[T] = object
    case full: bool
    of true: val: T
    of false: why: string = "empty"

  Two = object
    case a: bool
    of true: x: int = 1
    of false: y: int = 2
    case b: bool
    of true: z: string = "z"
    of false: w: string = "w"

proc scribble() {.noinline.} =
  # leave recognizable garbage where the next constructor temp lives
  var a: array[32, int] = default(array[32, int])
  for i in 0 ..< a.len: a[i] = 0x24 + i
  var s = 0
  for i in 0 ..< a.len: s += a[i]
  if s == 0: echo "unreachable"

proc newNode(kind: Kind): Node {.noinline.} = Node(kind: kind, clientData: 1)
proc mkLocal(kind: Kind): NodeObj {.noinline.} = NodeObj(kind: kind)

proc show(n: NodeObj) =
  case n.kind
  of kText, kComment: echo n.kind, " ", n.text, " ", n.clientData
  of kElement: echo n.kind, " ", n.tag, " ", n.children.len, " ", n.clientData
  else: echo n.kind, " ", n.target, " ", n.clientData

for k in Kind:
  scribble()
  let n = newNode(k)
  show n[]
  scribble()
  show mkLocal(k)

# the `seq` of the element branch is fresh, not garbage
scribble()
let root = newNode(kElement)
root.children.add newNode(kText)
echo root.children.len, " ", root.children[0].text

proc replace(kind: Kind): NodeObj {.noinline.} =
  result = NodeObj(kind: kText, text: "before")
  result = NodeObj(kind: kind)
show replace(kElement)

proc mkBox[T](f: bool): Box[T] {.noinline.} = Box[T](full: f)
let bi = mkBox[int](false)
echo bi.why
let bs = mkBox[string](true)
echo bs.val.len

proc mkTwo(a, b: bool): Two {.noinline.} = Two(a: a, b: b)
for a in [true, false]:
  for b in [true, false]:
    let t = mkTwo(a, b)
    echo (if t.a: t.x else: t.y), " ", (if t.b: t.z else: t.w)

proc mkShape(k: ShapeKind; name: string): Shape {.noinline.} =
  Shape(kind: k, name: name)
let s1 = mkShape(skCircle, "c")
let s2 = mkShapeElsewhere(skRect, "r")
echo s1.name, " ", s1.r, " ", s2.name, " ", s2.w, " ", s2.h

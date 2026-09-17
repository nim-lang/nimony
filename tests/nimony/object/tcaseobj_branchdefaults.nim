# A constructor that selects a case branch but sets none of its fields still
# initializes them: `oconstr` is total, and the native back end stores exactly
# the fields listed. Defaults that are not zero make the C back end see it too.
import std/syncio

type
  Kind = enum kInt, kList, kOther, kNone
  Node = ref object
    name: string
    case kind: Kind
    of kInt:
      bits: int = -1
    of kList:
      items: seq[int]
      size: int = 7
    of kNone: discard
    else:
      tag: int = 42
      label: string = "dflt"

proc scribble() {.noinline.} =
  # leave recognizable garbage in the stack slots the constructor reuses
  var a: array[32, int] = default(array[32, int])
  for i in 0 ..< a.len: a[i] = 0x24 + i
  var s = 0
  for i in 0 ..< a.len: s += a[i]
  if s == 0: echo "unreachable"

proc mkList(name: string): Node {.noinline.} = Node(kind: kList, name: name)
proc mkInt(): Node {.noinline.} = Node(kind: kInt)
proc mkOther(): Node {.noinline.} = Node(kind: kOther, name: "o")

for i in 0 .. 1:
  scribble()
  let l = mkList("abc")
  echo l.name, " ", l.items.len, " ", l.size
  scribble()
  let n = mkInt()
  echo n.bits
  scribble()
  let o = mkOther()
  echo o.name, " ", o.tag, " ", o.label

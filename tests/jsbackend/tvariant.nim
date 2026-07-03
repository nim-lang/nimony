## Object variants: the tagged-union layout laid out in linear memory, read
## back through the active branch.
import std/syncio

type
  Kind = enum kInt, kStr
  Node = object
    case kind: Kind
    of kInt: intVal: int
    of kStr: strVal: string

proc describe(n: Node): string =
  case n.kind
  of kInt: "int:" & $n.intVal
  of kStr: "str:" & n.strVal

echo describe(Node(kind: kInt, intVal: 7))
echo describe(Node(kind: kStr, strVal: "hi"))

import std/syncio

type
  Node = ref object
    le, ri: Node
    s: string

proc traverse(root: Node) =
  var it = root
  while it != nil:
    if true:
      echo it.s
      it = it.ri

  var jt = root
  if jt != nil:
    echo jt.s
    jt = jt.ri

traverse(nil)
# The routines of a `ref` type's module come with its values, as they do for
# an object or distinct type: `echo` finds `write`, and `t.weight` is found,
# although this module does not import `mrefbound1`.
import std / syncio
import deps / mrefbound2

proc main =
  let t = makeTree()
  echo t
  echo makeNode()
  echo t.weight

main()

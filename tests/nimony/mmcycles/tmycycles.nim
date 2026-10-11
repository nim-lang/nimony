# The directory is compiled with a runtime given by PATH (see `nimony.args`)
# that is not named `orc` but says `{.enableTrace.}` in its `nimTraceRef`: it must
# get the same compiler support -- traced refs, cell operations, the collector's
# header word -- or the cycle below is never freed.
{.feature: "lenientnils".}
import std/[assertions, syncio]

assert defined(gcMycycles)
assert not defined(gcOrc)
assert cyclesRuntimeMarker == 7

var destroyed = 0

type
  NodeObj = object
    kids: seq[Node]
    parent: Node
  Node = ref NodeObj

proc `=destroy`(n: NodeObj) =
  inc destroyed
  `=destroy`(n.kids)
  `=destroy`(n.parent)

proc tree() =
  var root = Node()
  for i in 0..<10:
    root.kids.add Node(parent: root)

GC_disableOrc()
for i in 0..<10: tree()
echo "before collect: ", destroyed
GC_fullCollect()
echo "after collect: ", destroyed

{.feature: "assumeSync".}  # test program: globals shared freely

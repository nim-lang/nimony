# `--mm:orc` (see `nimony.args`): reference counting plus the cycle collector of
# `lib/std/system/orc.nim`. Every shape of cycle the compiler has to trace --
# a plain ref field, a `seq` of refs, a class hierarchy, a closure environment
# -- is built, dropped and must be freed; cycles still reachable must survive.
{.feature: "lenientnils".}
import std/[syncio, assertions]

assert defined(gcOrc), "--mm:orc must define gcOrc"

var destroyed = 0

type
  # a ref field back to the own type
  PairObj = object
    name: string
    other: nil Pair
  Pair = ref PairObj

  # a `seq` of refs: traced through `seq`'s hand-written `=trace`
  TreeObj = object
    kids: seq[Tree]
    parent: nil Tree
  Tree = ref TreeObj

  # a class hierarchy: the collector traces a cell through its static type's
  # `=trace`, which dispatches to the dynamic type's
  Base = ref object of RootObj
    id: int
    other: Base
  Derived = ref object of Base
    extra: seq[Base]

  # a closure capturing the object that holds it
  Box = ref object
    cb: proc (): int {.closure.}

  # cannot form a cycle: never registered with the collector
  Leaf = ref object
    x: int
    s: string

proc `=destroy`(p: PairObj) =
  inc destroyed
  `=destroy`(p.name)
  `=destroy`(p.other)

proc `=destroy`(t: TreeObj) =
  inc destroyed
  `=destroy`(t.kids)
  `=destroy`(t.parent)

method describe(b: Base): string {.base.} = "base " & $b.id
method describe(d: Derived): string = "derived " & $d.id

proc pairs() =
  var a = Pair(name: "a")
  var b = Pair(name: "b")
  a.other = b
  b.other = a

proc tree() =
  var root = Tree()
  for i in 0..<10:
    var k = Tree(parent: root)
    root.kids.add k

proc classes(n: int) =
  var x = Derived(id: n)
  var y = Derived(id: n+1)
  x.extra.add y
  y.extra.add x
  y.other = x

proc closure(): int =
  result = 0
  var b = Box()
  var counter = 0
  b.cb = proc (): int {.closure.} =
    inc counter
    result = counter + (if b.cb != nil: 1 else: 0)
  result = b.cb()

var alive: seq[Base] = @[]

proc buildAlive() =
  var a = Derived(id: 1)
  var b = Base(id: 2)
  a.other = b
  b.other = a
  a.extra.add b
  alive.add a

proc main() =
  buildAlive()

  GC_disableOrc()
  for i in 0..<100: pairs()
  assert destroyed == 0      # every pair is garbage, but only a cycle
  GC_fullCollect()
  echo "pairs freed: ", destroyed

  destroyed = 0
  for i in 0..<100: tree()
  GC_fullCollect()
  echo "tree nodes freed: ", destroyed

  # the collector runs on its own once enough candidate roots piled up
  GC_enableOrc()
  destroyed = 0
  for i in 0..<10_000: pairs()
  echo "collected without being asked: ", destroyed > 0
  GC_fullCollect()
  echo "pairs freed eventually: ", destroyed

  let before = getOccupiedMem()
  for i in 0..<1000:
    classes(i)
    discard closure()
    discard Leaf(x: i, s: "leaf")
  GC_fullCollect()
  echo "classes and closures leaked: ", getOccupiedMem() - before

  assert alive[0].describe == "derived 1"
  assert alive[0].other.describe == "base 2"
  assert alive[0].other.other == alive[0]
  echo "reachable cycle intact"

main()

{.feature: "assumeSync".}  # test program: globals shared freely

import std/assertions

type
  CellObj = object
    value: int
    freed: ptr int
  Cell = ref CellObj
  Box = object
    id: int
    cell: Cell
  K = enum kNone, kCell, kNode
  Node = ref object
    sub: Tree
  Tree = object
    case k: K
    of kNone: discard
    of kCell: cell: Cell
    of kNode: child: Node
    tail: string
  Base = object of RootObj
    case b: K
    of kNone: discard
    of kCell: bcell: Cell
    of kNode: bchild: ref Derived
  Derived = object of Base
    extra: string

proc `=destroy`(x: CellObj) =
  if x.freed != nil:
    inc x.freed[]

proc `=wasMoved`(x: var CellObj) =
  x.freed = nil

proc checkCopy() =
  var freed = 0
  block:
    let src = Box(id: 42, cell: Cell(value: 5, freed: addr freed))
    var dest = Box(id: 1, cell: Cell(value: 7, freed: addr freed))
    `=copy`(dest, src)
    assert freed == 1
    assert dest.id == 42
    assert dest.cell == src.cell
    assert dest.cell.value == 5
    assert src.cell.value == 5
    `=copy`(dest, dest)
    assert freed == 1
    assert dest.cell.value == 5
  assert freed == 2

proc checkSink() =
  var freed = 0
  block:
    var src = Box(id: 42, cell: Cell(value: 5, freed: addr freed))
    var dest = Box(id: 1, cell: Cell(value: 7, freed: addr freed))
    `=sink`(dest, src)
    `=wasMoved`(src)
    assert freed == 1
    assert dest.id == 42
    assert dest.cell.value == 5
  assert freed == 2

proc checkCaseObject() =
  var freed = 0
  block:
    let src = Tree(k: kCell, cell: Cell(value: 5, freed: addr freed), tail: "t")
    var dest = Tree(k: kNone)
    `=copy`(dest, src)
    assert dest.k == kCell and dest.cell.value == 5 and dest.tail == "t"
    `=copy`(dest, Tree(k: kNone))
    assert freed == 0 and dest.k == kNone
    var moved = Tree(k: kCell, cell: Cell(value: 7, freed: addr freed))
    `=sink`(dest, moved)
    `=wasMoved`(moved)
    `=copy`(dest, dest) # `dest` owns the only reference to its cell
    assert freed == 0 and dest.cell.value == 7
  assert freed == 2

proc checkSourceInOldBranch() =
  # like `result = result.sons[0]`: the old branch owns the source
  var t = Tree(k: kNode, child: Node(sub: Tree(k: kNode, child: Node(), tail: "inner tail held on the heap " & $1)), tail: "outer")
  `=copy`(t, t.child.sub)
  let junk = "JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ" & $2
  assert t.k == kNode and t.tail == "inner tail held on the heap 1" and junk.len == 31
  var d = Derived(b: kNode, bchild: (ref Derived)(b: kNone, extra: "extra field held on the heap " & $3), extra: "outer")
  `=copy`(d, d.bchild[])
  let junk2 = "JJJJJJJJJJJJJJJJJJJJJJJJJJJJJJ" & $4
  assert d.b == kNone and d.extra == "extra field held on the heap 3" and junk2.len == 31

checkCopy()
checkSink()
checkCaseObject()
checkSourceInOldBranch()

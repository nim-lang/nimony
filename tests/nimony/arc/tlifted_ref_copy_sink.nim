import std/assertions

type
  CellObj = object
    value: int
    freed: ptr int
  Cell = ref CellObj
  Box = object
    id: int
    cell: Cell

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

checkCopy()
checkSink()

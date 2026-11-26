# A cast to a type without a destructor (`cast[pointer](x)` of a `ref`) does
# not take ownership of its operand, in a `sink` argument or a constructor
# field: the object is still destroyed.
{.feature: "assumeSync".}
import std/syncio

type
  CellObj = object
    id: int
  Cell = ref CellObj
  Holder = object
    p: pointer

var destroyed = 0
proc `=destroy`(cell: CellObj) = inc destroyed
proc `=wasMoved`(cell: var CellObj) = cell.id = 0

proc keep(p: sink pointer) = discard

proc main() =
  block:
    let a = Cell(id: 1)
    keep cast[pointer](a)   # the last use of `a`
    let b = Cell(id: 2)
    keep cast[pointer](b)   # not the last use of `b`
    echo b.id
    let c = Cell(id: 3)
    let h = Holder(p: cast[pointer](c))
    discard h
  echo "destroyed ", destroyed, " of 3"
main()

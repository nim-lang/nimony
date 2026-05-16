
import std / [syncio]

type
  Grid = object
    data: array[0..3, int]

proc `{}`(g: Grid; x, y: int): int =
  g.data[x * 2 + y]

proc `{}=`(g: var Grid; x, y: int; v: int) =
  g.data[x * 2 + y] = v

var g: Grid
g{0, 0} = 1
g{0, 1} = 2
g{1, 0} = 3
g{1, 1} = 4
echo g{0, 0}
echo g{0, 1}
echo g{1, 0}
echo g{1, 1}

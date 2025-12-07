type
  Tuple0 = tuple[a: int, b: int, c: int]
  Tuple = (int, int, int)

var x0: Tuple = (1, 2, 3)
var x1: Tuple0
var x2 = (1, 2, 3, 34, 5.6)

proc getTup(x: var int): tuple[x: var int] =
  result = (x: x)

var a = 100
getTup(a).x = 300


type
  Array = array[5, int]

var s = [1, 2, 3]
var s2: Array
var s3: Array = [1, 2, 3, 4, 5]

proc foo =
  var x: array[3, int] = [5, 6, 7]
  var m = [5, 6, 7, 8]

foo()

proc foo2(m: int) =
  var x = 1
  var s = (x, 1, 2)
  let m1 = (m, 6)
  const y = 12.3
  let z1 = (y, s, m1)

foo2(12)

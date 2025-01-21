proc g(x: var int): var int =
  result = x

proc foo =
  var s = 12
  let x = g(s)

foo()


proc g1(x: var int): var int =
  result = x

proc g2(x: var int): var int =
  result = x

proc foo1(x: int) =
  var x = x
  var s = g1(x)
  let y = g2(s)
  let n = g2(g1(x))

foo1(12)

proc g21(x: var int): int =
  var m = x
  result = m
  result = x

proc g31[T](x: var T): T =
  result = x

proc g41[T](x: var T): var T =
  result = x

proc foo11(x: int) =
  var x = x
  let s1 = g21(x)
  var s2 = g31(x)
  var s3 = g41(s2)

foo11(12)

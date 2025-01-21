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

# proc g3[T](x: var T): T =
#   result = x

proc foo1(x: int) =
  var x = x
  var s = g1(x)
  let y = g2(s)
  let n = g2(g1(x))
  # let s2 = g3(x)

foo1(12)


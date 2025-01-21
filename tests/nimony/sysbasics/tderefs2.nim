proc g(x: var int): var int =
  result = x

proc foo =
  var s = 12
  let x = g(s)

foo()

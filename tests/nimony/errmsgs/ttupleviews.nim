proc getTup: tuple[x: var int] =
  var a = 100
  result = (a, )
  result = (x: a)

getTup().x = 300
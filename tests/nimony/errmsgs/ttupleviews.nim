proc getTup: tuple[x: var int] =
  var a = 100
  result = (a, )
  result = (x: a)

getTup().x = 300

block:
  var counter = 0
  proc incr() = inc counter
  incr()

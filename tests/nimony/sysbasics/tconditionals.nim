proc foo = discard

var x = if true:
    1
  else:
    foo()
    2
# issue #853

proc foo(x: seq[bool]): lent bool =
  x[0]

var s = @[false]
if foo(s): discard

iterator foo2(x: seq[bool]): lent bool =
  yield x[0]

var s2 = @[false]
for i in foo2(s2):
  if i:
    discard

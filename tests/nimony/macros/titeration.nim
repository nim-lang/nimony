import std / [macros, assertions]

macro sumChildren(body: untyped): untyped =
  var total: BiggestInt = 0
  for child in body:
    total = total + child.intVal
  result = newIntLitNode(total)

macro weightedChildren(body: untyped): untyped =
  var total: BiggestInt = 0
  for index, child in pairs(body):
    total = total + BiggestInt(index + 1) * child.intVal
  result = newIntLitNode(total)

assert sumChildren([1, 2, 3]) == 6
assert weightedChildren([1, 2, 3]) == 14
assert sumChildren([]) == 0

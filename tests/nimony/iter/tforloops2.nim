import std/syncio

iterator foo3(): (int, int, (int, int)) =
  let x = (1, 2, (3, 4))
  yield (1, 2, (3, 4))
  yield x


for i, j, (m, n) in foo3():
  echo i, j, m, n


iterator fooVarTuple(x: var (int, int)): var (int, int) =
  yield x

block:
  var x = (1234, 5678)

  for i in fooVarTuple(x):
    let (a, b) = i
    echo a, b

block:
  # #2555: an inline iterator may use the locals of a top-level block
  var nums: seq[int] = @[1, 2, 3]
  iterator numItems(): int =
    for n in nums: yield n
  for n in numItems(): echo n

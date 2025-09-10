import std/syncio

iterator foo3(): (int, int, (int, int)) =
  let x = (1, 2, (3, 4))
  yield (1, 2, (3, 4))
  yield x


for i, j, (m, n) in foo3():
  echo i, j, m, n
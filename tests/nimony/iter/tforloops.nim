var globalX = 12

iterator foo(x: int): (int, int) =
  # yield (12, x)
  let ff = [(12, globalX)]
  yield ff[0]
  yield ff[0]
  yield (12, x)
  yield (22, 4)

iterator foo1(x: int): int =
  yield 1
  yield x

for m in foo1(2):
  let n = m

# proc foo =
for i, j in foo(2):
  let s = i
  let m = j
  let n = s

for i, j in foo(2):
  let s = i
  let m = j

proc bar() =
  for i, j in foo(2):
    let s = i
    let m = j

  for (i, j) in foo(2):
    let s = i
    let m = j

bar()

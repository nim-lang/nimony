import std / [syncio, assertions]

iterator powers(n: int): int =
  var i = 0
  while i <= n:
    yield i
    yield i*i
    yield i*i*i
    i = i + 1

proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}


for i in powers(5):
  let m = i

  printf("Hello, world: %ld\n", m)

iterator countup(a, b: int): int =
  var i = a
  while i <= b:
    yield i     # establish as the for loop variable
    inc i

for x in countup(1, 5):
  let m = x
  printf("countup start: %ld\n", m)
  if x > 5:
    break
  elif x < 3:
    continue
  printf("countup end: %ld\n", m)

iterator countup2(n: int): int =
  var i = 0
  while i <= n:
    yield i
    inc i

iterator powers2(n: int): int =
  for i in countup2(n):
    yield i
    yield i*i
    yield i*i*i

for i in powers2(6):
  printf("Hello, world: %ld\n", i)

iterator countup3(a: int): int =
  yield 3

iterator powers3(b: int): int =
  for j in countup3(b):
    yield j

for m in powers3(5):
  for n in countup3(4):
    printf("Hello, world: %ld\n", m+n)

while true:
  for i in 0 ..< 1:
    discard i
  break


iterator countup4(a: int): int =
  yield a

iterator powers4(a: int): int =
  for j in countup4(a):
    yield j

for i in powers4(5):
  for j in countup4(4):
    printf("Hello, world: %ld\n", i+j)


for i in 0..<3:
  for j in 0..<4:
    if j == 2:
      echo "left the loop!"
      break
    echo "A i ", i, " j ", j

block:
  for i in 1 ..< 3:
    for j in 2 ..< 4:
      let a = i + j
      var sum: int = 0
      for k in 1 ..< j:
        sum = sum + a
      echo sum

block:
  let i_arr = [-100, -50, 0, 0, 123, 1000]
  for i in items(i_arr):
    echo i

block tbreak:
  var
    x = false
    run = true

  while run:
    run = false
    block myblock:
      if true:
        break myblock
      echo "leaving myblock"
    x = true
  assert(x)

  # bug #1418
  iterator foo: int =
    for x in 0 .. 9:
      for y in [10,20,30,40,50,60,70,80,90]:
        yield x + y

  for p in foo():
    echo p
    break

  iterator permutations: int =
    yield 10

  for p in permutations():
    break

  # regression:
  proc main =
    for x in [true, false]:
      for y in [true, false]:
        echo x, " ", y

  main()


iterator mark*(): tuple[key: int, val: (int, int)] =
  for i in 0..2:
    yield (i, (i, i))


for i, (a, c) in mark():
  echo i, c

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

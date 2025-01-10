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

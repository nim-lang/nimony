proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

template foo(cond: untyped) {.untyped.} =
  let x = 123
  printf("%d\n", x)
  if cond:
    let y = 456
    printf("%d, %d\n", x, y)

foo(true)
when declared(x):
  error
when declared(y):
  error

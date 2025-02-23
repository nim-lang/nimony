proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

proc foo[T](x: T) {.untyped.} =
  printf("%s\n", x)
  let x = 123 # shadows
  printf("%d\n", x)
  if true:
    let y = 456
    printf("%d, %d\n", x, y)

foo(cstring"abc")

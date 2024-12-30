
proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

printf("Hello, world!\n")

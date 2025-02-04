var x1 = -123'i8
var x2 = -123'i16
var x3 = -123'i32
var x4 = -123'i64
var x5 = -123

let s1 = ashr(x1, 1)
let s2 = ashr(x2, 1)
let s3 = ashr(x3, 1)
let s4 = ashr(x4, 1)
let s5 = ashr(x5, 1)

let s6 = x1 shr 1

proc printf(format: cstring) {.importc: "printf", varargs, header: "<stdio.h>", nodecl.}

printf("Hello: %d\n", s5)
printf("Hello: %d\n", s6)

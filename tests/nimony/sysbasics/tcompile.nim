{.compile: "foo.c".}
{.compile("foo.c", "-fno-strict-aliasing")

proc myFunc(): int {.importc: "myFunc".}
proc myFunc2(): int {.importc: "myFunc2".}

let s = myFunc()
let s2 = myFunc2()
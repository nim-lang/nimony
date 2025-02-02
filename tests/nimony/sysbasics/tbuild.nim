{.build("C", "foo.c").}
{.build("C", "foo1.c", "-fno-strict-aliasing").}


proc myFunc(): int {.importc: "myFunc".}
proc myFunc2(): int {.importc: "myFunc2".}

let s = myFunc()
let s2 = myFunc2()

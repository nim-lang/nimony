{.compile: "foo.c".}

proc myFunc(): int {.importc: "myFunc".}

let s = myFunc()
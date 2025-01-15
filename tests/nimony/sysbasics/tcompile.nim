{.compile: "tests/nimony/sysbasics/foo.c".}
# TODO: stores filename in the deps so that relative paths work

proc myFunc(): int {.importc: "myFunc".}

let s = myFunc()
# A syntax error in the code handed to `parseStmt` is a compile error at the
# plugin call.

template parsed(mode: untyped; code: string) {.plugin: "deps/mparsenim".}

parsed stmt, "if x\n  echo 1"

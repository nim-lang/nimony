type typed* {.magic: Stmt.}

proc foo(x: typed) = discard

foo(abc)

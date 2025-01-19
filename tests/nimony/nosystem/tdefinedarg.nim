type untyped* {.magic: Expr.}

proc defined(x: untyped) {.magic: Defined.}

let x = defined(abc)
let y = defined(nimony)
let z = defined(abc.def)

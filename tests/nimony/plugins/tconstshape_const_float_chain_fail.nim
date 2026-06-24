template emitShape(mode: string): untyped {.plugin: "deps/mconstshape".}

emitShape("const-float-chain")

let y = DEG2RAD
discard y

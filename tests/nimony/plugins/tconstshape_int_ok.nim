template emitShape(mode: string): untyped {.plugin: "deps/mconstshape".}

emitShape("const-int")

let y = X
discard y

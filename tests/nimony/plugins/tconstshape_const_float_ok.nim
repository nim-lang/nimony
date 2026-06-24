template emitShape(mode: string): untyped {.plugin: "deps/mconstshape".}

emitShape("const-float")

let y = X
discard y

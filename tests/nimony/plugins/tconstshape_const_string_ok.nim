template emitShape(mode: string): untyped {.plugin: "deps/mconstshape".}

emitShape("const-string")

let y = X
discard y

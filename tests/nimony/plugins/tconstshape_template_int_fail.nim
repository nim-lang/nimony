template emitShape(mode: string): untyped {.plugin: "deps/mconstshape".}

emitShape("template-int")

discard X

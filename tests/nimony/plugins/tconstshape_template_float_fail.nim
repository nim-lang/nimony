template emitShape(mode: string): untyped {.plugin: "deps/mconstshape".}

emitShape("template-float")

discard X

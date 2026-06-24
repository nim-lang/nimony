template emitShape(mode: string): untyped {.plugin: "deps/mconstshape".}

emitShape("template-string")

discard X

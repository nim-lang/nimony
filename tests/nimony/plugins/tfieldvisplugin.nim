import deps / mfieldvistype

# A plugin-backed template receives the call-site argument as raw AST and
# emits it into the code it generates. That argument is written HERE, so a
# private field of `Box` (declared in another module) must be rejected --
# exactly as the direct form is. Regression test for issue #1988.
template pluginCheck*(cond: untyped) {.plugin: "deps/mfieldvisplugin".}

pluginCheck Box(public: 1, secret: 2)

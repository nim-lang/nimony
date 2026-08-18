# `{.naked.}` — like `{.assembler.}`, the machine-level checking belongs to the
# back end (arkham rejects a naked proc that would need a frame: a `{.stack.}`
# local, a callee-saved register, or an allocated body). What sem owns is the
# pragma's own shape, which is what this pins.

var notARoutine {.naked.}: int

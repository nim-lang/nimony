import std/[syncio, assertions]

# `slurp`/`staticRead` reads a file at compile time. `expreval.evalCall` folds
# it natively (via the `.semantics: "slurp"` pragma) instead of shelling out to
# a full nested sub-compile. The path is resolved relative to *this* source
# file, exactly like the sub-compile path it replaces.
const version = slurp("tslurp_data.txt")

assert version == "nimony 0.1.2\n"
assert version.len == 13

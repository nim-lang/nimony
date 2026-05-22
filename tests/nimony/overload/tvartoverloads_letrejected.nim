{.feature: "varToverloads".}

# With the feature on and only a `var T` overload in scope, an immutable
# arg must be rejected at sigmatch with `VarNeeded` — the mirror of Nim 2's
# `kVarNeeded`. Without this, the new tiebreaker would still pick this
# overload and the error would slip to a later pass.

proc takesVar(x: var int) = discard

let l = 0
takesVar(l)

# Regression guard for #2322: the same "template parameter left in type
# position" shape as `ttemplate_param_as_type`, but reaching `typeprops.typeImpl`
# through CONCEPT matching instead of `sigmatch.matchSymbol`.
#
# `twice`'s typevar carries a concept constraint, so binding the explicit
# generic argument goes matchTypevars -> matchesConstraint -> matchConceptSym ->
# matchConceptBody -> sigconcepts.conceptTargetNeedsStrictCheck. That helper
# called `typeImpl` on any symbol that was not a typevar, and the argument here
# is the template parameter `F` (symKind ParamY) -- `assert result.stmtKind ==
# TypeS` fired and killed nimsem.
#
# `def` is a plain (typed) template, so `F` in type position is legitimately
# "not a type"; that diagnostic is correct. The point is that the compiler
# reports it ONCE instead of crashing: `matchTypevars` now refuses to bind a
# typevar to a non-type explicit argument (`sigmatch.notATypeArg`), so
# `twice[F]` is never instantiated with `T := F` and the `twice(x)` overload
# dump is suppressed on top (`semcall.anyArgTypeIsError`). See
# `ttemplate_param_as_type` for the `{.untyped.}` route that is meant to
# support this pattern.
type Addable = concept
  proc `+`(a, b: Self): Self

proc twice[T: Addable](x: T): T = x + x

template def(F: untyped) =
  proc run(x: F) =
    discard twice[F](x)

def(int)

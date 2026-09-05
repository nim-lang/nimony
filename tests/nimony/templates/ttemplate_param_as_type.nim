# Regression guard: a generic routine called inside a template body, with a
# template parameter used as the generic type argument (`GVec2[typ]` /
# `gvec2[typ]`), used to CRASH the compiler during overload resolution —
# `typeprops.typeImpl` asserted `result.stmtKind == TypeS`, but the formal
# symbol resolved to the template parameter `typ` (symKind ParamY), not a type.
#
# `genCtor` is a plain (typed) template, so its body IS type-checked and `typ`
# used in type position is legitimately "not a type" — that error is correct.
# `matchSymbol` must simply not CRASH when a non-type formal reaches it: it now
# rejects with the normal diagnostic instead of asserting in `typeImpl`.
#
# The call itself no longer adds a second diagnostic: `x`/`y` are already typed
# `<type error>`, so the overload dump for `gvec2(x, y)` is suppressed (see
# `semcall.anyArgTypeIsError`) rather than restating an error the two lines
# above already made.
#
# To actually USE this pattern (vmath's genVecConstructor), the template must be
# `{.untyped.}` (or the module `{.feature: "untyped".}`) — only then does the
# body go through the untyped walk (`semTemplBody`) instead of typed sigmatch.
# This test pins the plain-template case: correct "not a type" errors, no crash.
type GVec2[T] = array[2, T]
proc gvec2[T](x, y: T): GVec2[T] = [x, y]
template genCtor(typ: untyped) =
  proc mk(x, y: typ): GVec2[typ] = gvec2[typ](x, y)
genCtor(float32)

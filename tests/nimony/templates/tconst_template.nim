## Regression for the historic `exprexec.nim:492 'false' empty serializer
## created` crash that fired any time a `const` initializer invoked a
## user-defined template. Surfaced 2026-05-01 by porting the SDL3 wrapper.
##
## Root cause: phase 2 (Signatures) publishes the template's body
## verbatim — still in unresolved-Ident form — and `const` initializers
## ALSO run in phase 2 (under `constGuard`). `expandTemplateImpl` only
## substituted formal parameters keyed by SymId, so `Ident x` in the
## not-yet-sem'd body never matched and the re-sem produced
## `(err x . "undeclared identifier: x")`. Fix: also key formal params
## by base name so the Ident-form body substitutes correctly.

import std / syncio

template ID*(x): untyped = x
template MASK*(x): untyped = 1 shl ((x) - 1)

const A = ID(1)
const B = MASK(1)
const C = MASK(3)

# Mirror the SDL3 wrapper's window-pos template/const pattern that
# originally hit this same code path.
const MASK32* = 0x1FFF0000'u
template OR_MASK*(x): untyped = MASK32 or uint(x)
const COMBINED* = OR_MASK(0'u)

proc main =
  echo "A=", A
  echo "B=", B
  echo "C=", C
  echo "COMBINED=", COMBINED

main()

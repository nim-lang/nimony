## Under `.feature: "ignoreStyle".` builtin pragma names match the Nim-1
## `cmpIgnoreStyle` rule (fully case-insensitive, underscores ignored,
## including the first character — distinct from the first-char-sensitive
## rule used for ordinary identifiers).

{.feature: "ignoreStyle".}

import std / [assertions, syncio]

proc a(): int {.noinline.} = 1
proc b(): int {.noInline.} = 2
proc c(): int {.no_inline.} = 3
proc d(): int {.NoInline.} = 4

# Multi-word pragma name: canonical is `noSideEffect`.
proc pureA(): int {.noSideEffect.} = 5
proc pureB(): int {.no_side_effect.} = 6
proc pureC(): int {.NOSIDEEFFECT.} = 7

assert a() + b() + c() + d() == 10
assert pureA() + pureB() + pureC() == 18

echo "OK"

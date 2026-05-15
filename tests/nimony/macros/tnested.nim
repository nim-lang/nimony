# Nested macros: an outer macro calls a helper macro inside its body.
# This works for free via the recursive-sem architecture — when sem checks
# `outer`'s body, it expands `buildLit()` immediately, splicing `42` into
# the body before `outer`'s plugin is generated. `outer`'s plugin then
# computes `42 + 100` at plugin runtime and emits `newIntLitNode(142)`.
import std / [syncio, macros]

macro buildLit(): untyped =
  result = newIntLitNode(42)

macro outer(): untyped =
  let x = buildLit()
  result = newIntLitNode(x + 100)

echo outer()

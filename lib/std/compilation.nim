## Compile-time helpers. `staticRead` is a plain alias for `readFile` and
## works in `const` context via the `executeExpr` sub-compile engine.

import std / syncio

proc staticRead*(filename: string): string {.raises.} =
  # A proc, not a template: the sub-compile resolves a relative path against
  # the directory of the call's file, which must be the caller's, not this one.
  readFile(filename)

proc slurp*(filename: string): string {.raises.} =
  readFile(filename)

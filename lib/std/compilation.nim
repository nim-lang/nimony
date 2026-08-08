## Compile-time helpers. `staticRead` is a plain alias for `readFile` and
## works in `const` context via the `executeExpr` sub-compile engine.

import std / syncio

template staticRead*(filename: string): untyped =
  readFile(filename)

template slurp*(filename: string): untyped =
  staticRead(filename)

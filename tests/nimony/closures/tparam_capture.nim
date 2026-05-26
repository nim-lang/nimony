## Closure proc captures the enclosing proc's PARAMETER (not just
## locals). Exercises the param-init prologue in lambdalifting's
## `treParams`: outer's `p` gets stored into the env at the start of
## outer's body so inner can read it back through its env-param.

import std / syncio

proc outer(p: int) =
  proc inner() {.closure.} =
    echo p
  inner()

outer(42)

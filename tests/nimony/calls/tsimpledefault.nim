
import std / [syncio]

proc myecho(x: int = 4) =
  echo x

myecho()

# a default that calls a proc literal: the call site checks the typed
# `(expr (stmts (proc ...)) anonproc)` callee again
proc addLit(a: int; w = (proc (): int = 3)()): int = a + w
echo addLit(1)

#       Nif library
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Compat shims that let a module compile under both Nim and Nimony.
## Must be `include`d, not `import`ed, because Nim does not export
## custom pragmas across module boundaries.

when defined(nimony):
  {.pragma: canRaise, raises.}
else:
  {.pragma: canRaise.}

when not defined(nimony):
  import std/tables

  proc getOrQuit*[A, B](t: var Table[A, B]; k: A): var B =
    ## Host-Nim shim for the Nimony `tables.getOrQuit` that returns `var B`
    ## and aborts if the key is absent. Callers guard with `hasKey` before
    ## this call, so the missing-key branch is unreachable in practice.
    if not t.hasKey(k): quit "getOrQuit: missing key"
    result = t[k]

  proc getOrQuit*[A, B](t: var OrderedTable[A, B]; k: A): var B =
    if not t.hasKey(k): quit "getOrQuit: missing key"
    result = t[k]

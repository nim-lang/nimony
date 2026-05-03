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

when defined(nimony):
  template onRaiseQuit(call: untyped): untyped =
    ## Calls a `{.raises.}` proc from a non-raising context: wraps in
    ## try/except and aborts with a useful diagnostic on failure. Lets us
    ## keep `raises` from spreading virally through every layer in `sem.nim`
    ## et al. without silently swallowing the raise.
    ## Unexported on purpose — `compat2.nim` is `include`d, not `import`ed,
    ## so each user file gets its own private copy and there is no
    ## cross-module ambiguity.
    try:
      call
    except:
      quit "FAILURE: " & astToStr(call)
else:
  template onRaiseQuit(call: untyped): untyped = call

when not defined(nimony):
  from std/paths import Path

  proc path*(s: string): Path {.inline.} =
    ## Compat shim: Nimony exposes `path(string) -> Path` (its dirs/paths API
    ## takes a `Path`-typed wrapper); on host Nim the equivalent is `Path(s)`.
    ## Having both spellings lets call sites write `createDir(path(s))`
    ## unconditionally, dropping the `when defined(nimony)`/`else` ladder.
    ## `from … import` keeps `paths.getCurrentDir` etc. out of scope so it
    ## doesn't collide with `os.getCurrentDir` in modules that include this.
    Path(s)

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

  proc getOrQuit*[A, B](t: Table[A, B]; k: A): B =
    ## Read-only variant for `let`-bound tables: nimony's `getOrQuit` takes
    ## the table by value and returns `var B`, but host Nim distinguishes
    ## mutable from immutable receivers.
    if not t.hasKey(k): quit "getOrQuit: missing key"
    result = t[k]

  proc getOrQuit*[A, B](t: OrderedTable[A, B]; k: A): B =
    if not t.hasKey(k): quit "getOrQuit: missing key"
    result = t[k]

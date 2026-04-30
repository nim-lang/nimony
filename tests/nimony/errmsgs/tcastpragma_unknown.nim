## Unknown names inside `{.cast(...).}` are rejected, and
## `uncheckedAccess` is not a routine pragma — it is only valid inside a
## `{.cast(uncheckedAccess).}:` block.

{.cast(somePragma).}:
  discard

proc looseUncheckedAssign() {.uncheckedAccess.} = discard

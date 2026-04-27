## Unknown names inside `{.cast(...).}` are rejected, and
## `uncheckedAssign` is not a routine pragma — it is only valid inside a
## `{.cast(uncheckedAssign).}:` block.

{.cast(somePragma).}:
  discard

proc looseUncheckedAssign() {.uncheckedAssign.} = discard

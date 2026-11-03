# Value-level `repr`, following Nim's `system/repr_v2` semantics.
#
# This is intentionally separate from `$`: `repr` quotes strings and chars
# with decimal escapes, prints object type names, and uses `@[...]` for seqs.
#
# The implementation lives in `private/reprimpl`; this module exposes only the
# public `repr` API.

import private/reprimpl

template repr*[T](x: typedesc[T]): string {.untyped.} =
  reprImpl(x)

template repr*[T](x: T): untyped {.untyped.} =
  reprImpl(x)

type Reprable* = concept
  ## A type with a value-level `repr`. Generic containers and user helpers use
  ## this constraint so their bodies can stay closed over `repr`.
  proc repr(x: Self): string

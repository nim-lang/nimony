## Without `.feature: "ignoreStyle".` builtin pragma names stay strict —
## `{.NoInline.}` and `{.no_inline.}` are rejected.

import std / [assertions, syncio]

proc a(): int {.NoInline.} = 1

discard a()

## `std/contextvars` reaches a coroutine's chain through `ctxSlot()`, a
## compiler primitive the backend lowers only inside a `.passive` proc. A
## regular proc owns no frame, so using the module from one is a compile error
## rather than a silent fall back to a thread-wide chain — there is none.

import std / contextvars

var v = newContextVar(1)

proc regular() =
  discard v.get()

regular()

# A deferred plugin call must survive *through* a generic body, not just into
# one: `outer`'s return type is still `distinctBase(U)` when the call to `inner`
# is matched against it, and every instantiation re-drives the plugin from the
# parked `(at …)` node.

import std / [syncio, typetraits]

type
  MyInt = distinct int

proc inner[T](x: T): distinctBase(T) =
  result = distinctBase(T)(x)

proc outer[U](y: U): distinctBase(U) =
  result = inner(y)

echo outer(MyInt(5))
echo outer(7)

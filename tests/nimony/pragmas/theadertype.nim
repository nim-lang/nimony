# A module that uses an imported type only in a proc signature or a local
# still has to include the type's header (lengc used to include it only when
# the type was reached through an object field).
import std/syncio
import deps/mheadertype

proc roundTrip(x: int): int =
  let p: Port = toPort(x)
  result = portVal(p)

echo roundTrip(8080)

# A proven call to a routine of ANOTHER module goes to that module's guard-free
# copy, named by a fixed rule from the routine's own symbol
# (`contracts_fir.bodyOfRequires`) — nothing is looked up, the defining module
# emitted it. This compiles and links only if both sides agree on the name.

import std / syncio
import deps / mhalf

proc provenLiteral(): int =
  result = half(10)

proc provenGuarded(y: int): int =
  result = 0
  if y > 0:
    result = half(y)

proc unproven(y: int): int =
  result = half(y)

echo provenLiteral(), " ", provenGuarded(8), " ", unproven(6)

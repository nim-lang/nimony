# A runtime of the user's own that collects cycles: the compiler does not know
# it by name, it opts in through `{.enableTrace.}` in its `nimTraceRef` -- here the
# one `system/orc` declares, reused wholesale.

const cyclesRuntimeMarker* = 7
  ## Only this file defines it: proves this runtime, not `orc`, got included.

include "../../../../lib/std/system/orc"

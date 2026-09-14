# A plugin that expands at the toplevel into routines calling each other: the
# expansion needs the phases before the body phase too, or a forward
# declaration is never matched with its implementation (every call is
# ambiguous) and a routine cannot call one declared after it.

import std / syncio

template evenOdd() {.plugin: "deps/mforwardplugin".}

evenOdd()

echo isEven(10), " ", isOdd(7), " ", isEven(3)
echo isEven2(4), " ", isOdd2(4)

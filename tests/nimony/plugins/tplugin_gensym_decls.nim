# A type or routine a plugin declares under a `genSym()` name gets a global
# name like any other type or routine: it is lifted to module scope, where a
# local name is neither indexed (#2644) nor unique (#2645).
import std / syncio

template sizeOfFresh(): int {.plugin: "deps/mplugin_gensym_type".}
template value(): int {.plugin: "deps/mplugin_gensym_proc".}

proc first(): int =
  result = 0
  for i in 0..<2: result += value()

proc second(): int =
  result = 0
  for i in 0..<2: result += value()

echo sizeOfFresh()
echo sizeOfFresh()
echo first(), " ", second()

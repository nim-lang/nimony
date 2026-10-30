import std/syncio
import mcyc5_b {.cyclic.}

# `Box` and its `=destroy` live in `mcyc5_b`, whose bodies are checked after
# this one: instantiating `Box[int]` here must already know of the hook.
proc useBox*() =
  var b = Box[int](x: 1)
  echo "using ", b.x

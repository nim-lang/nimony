# The foreign half of tclosure_env_release: a resource with a loud destructor,
# and an object holding a closure in a field of the semchecked (never lowered
# here) `.closure` type.
{.feature: "lenientnils".}
import std/syncio

type
  Resource* = object
    name*: string

proc `=destroy`*(r: Resource) =
  if r.name.len > 0: echo "released ", r.name

type
  Holder* = object
    onEvent*: proc() {.closure.}

proc clear*(h: var Holder) = h.onEvent = nil

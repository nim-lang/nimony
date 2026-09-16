# The foreign half of tclosure_xmod_field_store: an object with a closure
# field, plus the module-local reader that clears the field before calling.
{.feature: "lenientnils".}
import std/syncio

type
  Window* = object
    title*: string
    onConfigured*: proc() {.closure.}

proc newWindow*(title: string): Window = Window(title: title)

proc report*(what, name: string) =
  ## borrow, not move: the closure must not empty its capture by reading it
  echo what, name

proc fire*(w: var Window) =
  ## Take the callback out of the field and run it: the copy must keep the
  ## env alive even though the field is cleared before the call.
  let cb = w.onConfigured
  w.onConfigured = nil
  if cb != nil: cb()

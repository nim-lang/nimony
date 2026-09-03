{.feature: "lenientnils".}
import std/syncio

var hook*: proc () {.closure.} = nil

template fire*() =
  if hook != nil: hook()

proc setup*() =
  hook = proc () {.closure.} = echo "fired"

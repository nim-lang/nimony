# the environment of an owned closure must be acyclic
{.feature: "ownedRefs".}
import std/syncio
type
  Widget = ref object
    onClick: proc () {.closure.}
  Label = ref object
    onChange: owned proc () {.closure.}

proc good(w: Label; s: string) =
  w.onChange = proc () {.closure.} = echo s   # ok: captures a string only

proc bad(w: Widget): owned proc () {.closure.} =
  result = proc () {.closure.} = echo w.onClick == nil

let L = Label(onChange: proc () {.closure.} = echo "init")
good(L, "hi")
L.onChange()
discard bad(Widget(onClick: proc () {.closure.} = discard))

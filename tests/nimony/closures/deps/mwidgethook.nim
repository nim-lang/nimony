import std/syncio

type
  Widget* = ref object
    cb*: proc () {.closure.}

proc makeWidget*(): Widget =
  result = Widget(cb: proc () {.closure.} = echo "cb")

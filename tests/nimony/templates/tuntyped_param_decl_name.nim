# Under `.untyped`, a declaration named by an `untyped` parameter declares the
# caller's identifier, as in Nim 2: `.inject`, `.dirty`, or plain.
import std/syncio

template declareInject(name: untyped) {.untyped.} =
  var name {.inject.} = 0
  inc name

template declareDirty(name: untyped) {.untyped, dirty.} =
  var name = 10
  inc name

template declarePlain(name: untyped; v: int) {.untyped.} =
  let name = v
  var hidden = 100
  block:
    echo name + hidden

proc f(): int =
  declareInject(i)
  declareDirty(j)
  declarePlain(k, 5)
  declarePlain(m, 6)
  result = i + j + k + m

echo f()
declareInject(g)
echo g

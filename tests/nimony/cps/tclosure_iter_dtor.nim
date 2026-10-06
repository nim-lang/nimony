# A `.closure` iterator's locals live in its frame (lambdalifting lifts them
# before the destroyer runs), so the frame is destroyed once with it: when the
# iterator finishes, when the loop breaks, and through an iterator value.
import std/syncio
type Tracked = object
  name: string
proc `=destroy`(t: Tracked) {.noSideEffect.} =
  if t.name.len > 0:
    {.cast(noSideEffect).}: echo "  destroy ", t.name

iterator walk(tag: string; n: int): int {.closure.} =
  var stack = Tracked(name: tag)
  var i = 0
  while i < n:
    yield i
    inc i
  discard stack.name.len

proc directFull() =
  for x in walk("df", 2): discard x
  echo "directFull done"
proc directBreak() =
  for x in walk("db", 5):
    if x == 1: break
  echo "directBreak done"
proc valueFull() =
  let it = walk
  for x in it("vf", 2): discard x
  echo "valueFull done"
proc valueBreak() =
  let it = walk
  for x in it("vb", 5):
    if x == 1: break
  echo "valueBreak done"
directFull()
directBreak()
valueFull()
valueBreak()

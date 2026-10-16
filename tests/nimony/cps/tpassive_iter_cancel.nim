import std/syncio
# A `for` loop over a `.passive` iterator that is left early resumes the
# iterator once more (`=destroy(Join)`): its `yield` returns, and that return
# destroys what the iterator still holds. Each local is destroyed exactly once,
# whichever way the loop ends.

type Tracked = object
  name: string

proc `=destroy`(t: Tracked) {.noSideEffect.} =
  if t.name.len > 0:
    {.cast(noSideEffect).}: echo "  destroy ", t.name

proc step() {.passive.} = discard

iterator walk(tag: string; n: int): int {.passive.} =
  var stack = Tracked(name: tag & ".stack")
  block:
    let tmp = Tracked(name: tag & ".tmp")   # gone before the first yield
    discard tmp.name.len
  var i = 0
  while i < n:
    let item = Tracked(name: tag & ".item" & $i)
    yield i
    {.cast(noSideEffect).}:
      step()
    discard item.name.len
    inc i
  discard stack.name.len

iterator outer(n: int): int {.passive.} =
  let o = Tracked(name: "outer")
  for x in walk("inner", n):
    yield x * 10
  discard o.name.len

proc regularBreak() =
  for x in walk("rb", 5):
    if x == 1: break
  echo "regularBreak done"

proc regularReturn(): int =
  for x in walk("rr", 5):
    if x == 2: return x
  result = -1

proc passiveBreak() {.passive.} =
  for x in walk("pb", 5):
    step()                 # the body suspends before it leaves
    if x == 1: break
  echo "passiveBreak done"

proc passiveFull() {.passive.} =
  for x in walk("pf", 2): discard x
  echo "passiveFull done"

proc nestedBreak() {.passive.} =
  for y in outer(5):
    if y == 10: break
  echo "nestedBreak done"

iterator inl(n: int): int =
  let t = Tracked(name: "inl")
  var i = 0
  while true:
    if i >= n: return
    yield i
    inc i
  discard t.name.len

proc inlineReturn() =
  for x in inl(2): echo "  inl ", x
  echo "inlineReturn done"

regularBreak()
let r = regularReturn()
echo "regularReturn ", r
passiveBreak()
passiveFull()
nestedBreak()
inlineReturn()

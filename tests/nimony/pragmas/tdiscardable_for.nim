# A `for`/`while` body ending in a discarded `{.discardable.}` call that
# returns a `ref` (or a `string`) used to crash finalir (nimony#2906, item 1).

import std/syncio

type Obj = ref object
  v: int

proc make(v: int): Obj {.discardable.} =
  result = Obj(v: v)

proc mkStr(v: int): string {.discardable.} =
  result = $v

proc view() =
  for i in 0 ..< 2:
    make(i)

proc viewThenMore() =
  for i in 0 ..< 2:
    make(i)
  echo "after"

proc nested() =
  for i in 0 ..< 2:
    for j in 0 ..< 2:
      make(i + j)
      mkStr(j)

proc whileLoop() =
  var i = 0
  while i < 2:
    inc i
    make(i)

proc explicitDiscard() =
  for i in 0 ..< 2:
    discard make(i)

view()
viewThenMore()
nested()
whileLoop()
explicitDiscard()
for i in 0 ..< 2:
  make(i)
echo "ok"

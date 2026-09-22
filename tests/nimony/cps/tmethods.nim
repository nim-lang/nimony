import std/syncio
{.feature: "lenientnils".}
type MyObject = ref object of RootObj
type MyObject2 = ref object of MyObject

method a(x: MyObject) {.passive.} =
  echo "heh"

method a(x: MyObject2) {.passive.} =
  echo "heh2"

method inherited(x: MyObject) {.passive.} =
  echo "inherited"

method io(x: MyObject) {.passive.} =
  echo "io"

var cont: Continuation
method sus(x: MyObject) {.passive.} =
  echo "a"
  x.io()
  cont = delay()
  suspend()
  echo "continued"
  cont = delay x.io()

var q = MyObject2()
q.a()
q.inherited()
# `sus` parks and is resumed by this same thread, so it is started as a
# continuation: a plain call from here would run it to completion, and wait for
# a resume that only this thread could perform.
complete(delay q.sus())
complete(cont)
complete(cont)

type
  Base = ref object of RootObj
  Derived = ref object of Base

method mysupermethod(x: Base) {.passive.}  =
  echo "base"
method mysupermethod(x: Derived) {.passive.}  =
  echo "derived"

var d: Base = Derived()
var b = Base()
var x: seq[Base] = @[d, b]
for i in x:
  i.mysupermethod()

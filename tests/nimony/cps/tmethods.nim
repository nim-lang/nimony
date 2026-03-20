import std/syncio

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
cast[MyObject](q).a()
q.a()
q.inherited()
q.sus()
cont.complete()
cont.complete()
import std/syncio

type
  E {.importc.} = ref object of RootObj
  R {.importc.} = ref object
  P {.importc.} = ptr object

var e: nil E = nil
var r: nil R = nil
var p: nil P = nil
echo e == nil, " ", r == nil, " ", p == nil

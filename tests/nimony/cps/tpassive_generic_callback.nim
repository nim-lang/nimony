import std/syncio

type
  Callback[T] = proc(value: T) {.nimcall, passive.}

proc invoke[T](callback: Callback[T]; value: T) {.untyped, passive.} =
  callback(value)

proc printValue(value: int) {.nimcall, passive.} =
  echo value

proc main {.passive.} =
  invoke(printValue, 42)

main()

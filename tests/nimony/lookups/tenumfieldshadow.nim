import std/assertions
import deps/menumfieldshadow

func lower*[T]() = discard

assert tagRole(lower) == 1

proc innerShadow =
  let lower = 99
  assert lower == 99

innerShadow()

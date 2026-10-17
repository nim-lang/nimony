type
  State* = object
    value*: int
    r*: range[0..10]
  Outer* = object
    s*: State

proc check*(m: State) {.requires: m.value >= 0.} =
  discard
proc checkVar*(m: var State) {.requires: m.value >= 0.} =
  discard
proc checkRange*(m: State) {.requires: m.r >= 0 and m.r < 11.} =
  discard
proc checkNested*(o: Outer) {.requires: o.s.value >= 0.} =
  discard
proc checkTwo*(a: State; b: int) {.requires: a.value < b.} =
  discard

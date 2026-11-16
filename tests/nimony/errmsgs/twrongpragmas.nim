
import std / [syncio]




proc main() {.taga: [RootEffect, IOEffect], raisea: [], deprecatea: "Use other instead", sideEffeca.} =
  echo "This one is real"

main()

var variableWithPragmaOnlyForObj {.inheritable.} = 1
proc procWithPragmaOnlyForObj() {.final, union.} = discard

type
  FooNonObj1 {.inheritable.} = int
  FooNonObj2 {.final, union, packed.} = int
  FooNonObj3 {.view.} = int

# a proc type in a parameter list: no `result` is in scope there
proc takesFn(g: proc (x: int): int {.nimcall, ensures: result >= 0.}): int = g(1)

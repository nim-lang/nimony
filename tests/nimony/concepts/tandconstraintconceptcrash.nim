## `T: SomeInteger and IntegerArithmetic` reaching the constraint-mismatch
## diagnostic. Minimal input (reduced from `lib/std/math.nim`) for a native
## backend miscompile: a nimsem built by `nimony n` segfaults here instead of
## reporting the error as soon as `sigmatch.conceptRoutineAvailable` gains a
## seventh parameter, so that one of its arguments is passed on the stack.
## The requirement-free `concept of` child and the `and` constraint are both
## needed to reach it.

type
  Arithmetic* = concept
    func `+`(x, y: Self): Self

  IntegerArithmetic* = concept of Arithmetic

func euclDiv*[T: SomeInteger and IntegerArithmetic](x, y: T): T {.inline.} =
  result = result + abs(y)

import std/assertions

## `mutualGenericMatch` compares overload formals crosswise (formal A vs
## formal B). When `int` is not a member of the typevar constraint, neither
## cross-comparison is more specific, so tie-breaking returns `NobodyWins`
## even though the call argument is plainly `int`.
##
## Contrast: `tconceptgenericoverload.nim` uses a weaker constraint where
## `int` satisfies `T`; crosswise comparison can still break the tie there.

type
  PlusEq* = concept
    proc `+`*(a, b: Self): Self

  StrictElem* = concept of PlusEq
    proc tag*(t: typedesc[Self]): Self

func tag*(t: typedesc[float64]): float64 = 0.0

type Box*[T: StrictElem] = object
  v*: T

func pick*[T: StrictElem](b: Box[T], x: T): Box[T] = b
func pick*[T: StrictElem](b: Box[T], x: int): Box[T] = b

func useInt*[T: StrictElem](b: Box[T], e: int): Box[T] =
  pick(b, e)

assert useInt(Box[float64](v: 1.0), 2).v == 1.0

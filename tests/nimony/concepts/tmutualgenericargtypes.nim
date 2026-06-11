import std/assertions

## `mutualGenericMatch` compares overload formals crosswise (formal A vs
## formal B). When `int` is not a member of the typevar constraint, neither
## cross-comparison is more specific, so tie-breaking returns `NobodyWins`
## even though the call argument is plainly `int`.
##
## Comparing each formal against the **call-site argument type** instead
## would pick `pick(..., int)` because that overload exact-matches `int`.
##
## Contrast: `tconceptgenericoverload.nim` uses a weaker constraint where
## `int` satisfies `T`; crosswise comparison can still break the tie there.

type
  AdditiveSemigroup* = concept
    proc `+`*(a, b: Self): Self
    proc `==`*(a, b: Self): bool

  FloatPart* = concept of AdditiveSemigroup
    proc pi*(t: typedesc[Self]): Self

func pi*(t: typedesc[float64]): float64 = 3.141592653589793

type Box*[T: FloatPart] = object
  v*: T

func pick*[T: FloatPart](b: Box[T], x: T): Box[T] = b
func pick*[T: FloatPart](b: Box[T], x: int): Box[T] = b

func useIntIndex*[T: FloatPart](b: Box[T], e: int): Box[T] =
  ## Generic body: `e` has type `int`, but overload resolution currently
  ## reports an ambiguous call to `pick` instead of selecting the `int`
  ## parameter overload.
  pick(b, e)

assert int isnot FloatPart
assert float64 is FloatPart

proc checkConcrete() =
  var b = Box[float64](v: 1.0)
  assert useIntIndex(b, 2).v == 1.0

checkConcrete()

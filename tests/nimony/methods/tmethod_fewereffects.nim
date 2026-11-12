# The valid side of `tmethod_sideeffect.nim`: an override may be
# `.noSideEffect` where the overridden method is not, a `.noSideEffect` chain
# can be called from a `func`, and a method with another signature is a method
# of its own.
import std/syncio

type
  A = ref object of RootObj
  B = ref object of A

method m(a: A): int {.base.} = (echo "A"; 1)
method m(b: B): int {.noSideEffect.} = 2

method n(a: A): int {.base, noSideEffect.} = 3
method n(b: B): int {.noSideEffect.} = 4

method other(a: A; x: int) {.base, noSideEffect.} = discard
method other(b: B; x: string) = echo "other ", x

func pure(a: A): int = n(a)

let a: A = A()
let b: A = B()
echo m(a), m(b), pure(a), pure(b)
other(B(), "signature")

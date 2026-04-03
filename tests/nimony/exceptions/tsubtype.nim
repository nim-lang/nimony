import std/syncio

type
  BaseError = object of RootObj
    value: int

  DerivedError = object of BaseError
    extra: int

# Should compile: DerivedError is a subtype of BaseError
proc canRaiseDerived(): int {.raises: BaseError.} =
  var e = DerivedError(value: 1, extra: 2)
  raise e
  result = 0

echo "Subtype checking for exception types works!"

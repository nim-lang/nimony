import std/syncio

type
  BaseError = object of RootObj
    value: int

  DerivedError = object of BaseError
    extra: int

# Should NOT compile: BaseError is not a subtype of DerivedError
proc shouldFail(): int {.raises: DerivedError.} =
  var e = BaseError(value: 1)
  raise e  # Error: BaseError is not a subtype of DerivedError
  result = 0

echo "This should not compile"

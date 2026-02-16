import std/syncio

type
  BaseError = object of RootObj
    value: int

  DerivedError = object of BaseError
    extra: int

# Should be able to raise DerivedError in a function that declares raises: BaseError
proc foo(): int {.raises: BaseError.} =
  var e = DerivedError(value: 1, extra: 2)
  raise e
  result = 0

echo "DerivedError accepted where BaseError is declared"

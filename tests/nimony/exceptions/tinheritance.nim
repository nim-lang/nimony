import std/syncio

type
  BaseError = object of RootObj
    msg: string

  DerivedError = object of BaseError
    code: int

# Should be able to raise DerivedError in a function that declares raises: BaseError
proc foo(): int {.raises: BaseError.} =
  var e = DerivedError(msg: "derived error", code: 42)
  raise e
  result = 0

echo "Subtype (DerivedError) can be raised where BaseError is declared"

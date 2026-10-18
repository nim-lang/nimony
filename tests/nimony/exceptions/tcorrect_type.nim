import std/syncio

type
  MyError = ref object of Exception
    code: int

proc foo(): int {.raises: MyError.} =
  raise MyError(code: 1)
  result = 42

echo "Type checking works correctly!"

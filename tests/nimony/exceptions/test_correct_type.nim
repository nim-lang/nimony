import std/syncio

type
  MyError = distinct int32

proc foo(): int {.raises: MyError.} =
  raise MyError(1)
  result = 42

echo "Type checking works correctly!"

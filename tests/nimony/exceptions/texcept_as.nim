import std/[syncio, assertions]

type
  MyError = distinct int32

proc foo(): int {.raises: MyError.} =
  raise MyError(42)
  result = 0

proc test() =
  try:
    let x = foo()
    echo "x = ", x
  except MyError as e:
    # Check that e has the correct type MyError, not ErrorCode
    echo "Caught MyError: ", int32(e)
    assert int32(e) == 42
    echo "SUCCESS: except Type as e works with arbitrary types!"

test()

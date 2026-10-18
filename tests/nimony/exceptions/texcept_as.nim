import std/[syncio, assertions]

type
  MyError = ref object of Exception
    code: int

proc foo(): int {.raises: MyError.} =
  raise MyError(code: 42)
  result = 0

proc test() =
  try:
    let x = foo()
    echo "x = ", x
  except MyError as e:
    # Check that e has the correct type MyError, not ErrorCode
    echo "Caught MyError: ", e.code
    assert e.code == 42
    echo "SUCCESS: except Type as e works with arbitrary types!"

test()

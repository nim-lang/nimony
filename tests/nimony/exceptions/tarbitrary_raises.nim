import std/[assertions, syncio]

# Test that arbitrary types can be used in .raises pragma

type
  MyError = distinct int32

# Test 1: .raises without type defaults to ErrorCode
proc foo1() {.raises.} =
  discard

# Test 2: .raises with custom type
proc foo2(): int {.raises: MyError.} =
  result = 42

# Test 3: Calling raises proc
proc callsRaises() {.raises: MyError.} =
  let x = foo2()
  assert x == 42

echo "Testing arbitrary types in .raises pragma"
try:
  callsRaises()
  echo "Success: arbitrary types work in .raises"
except:
  echo "FAIL: unexpected exception"
  assert false

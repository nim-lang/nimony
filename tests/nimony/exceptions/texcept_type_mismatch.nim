import std/syncio

type
  ErrorA = distinct int32
  ErrorB = distinct int32

proc foo(): int {.raises: ErrorA.} =
  raise ErrorA(1)
  result = 0

proc test() =
  try:
    let x = foo()
  except ErrorB as e:  # Type mismatch: raises ErrorA but catches ErrorB
    echo "Caught: ", int32(e)

test()

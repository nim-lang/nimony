
import std / [syncio]

proc main(a, b: int) =
  {.keepOverflowFlag.}:
    let x = a + b
    echo overflowFlag()

proc main32(a, b: int32) =
  {.keepOverflowFlag.}:
    let x = a + b
    echo overflowFlag()

main(1, high(int))
main32(1'i32, high(int32))

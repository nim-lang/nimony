
import std / [syncio]

proc main(a, b: int) =
  {.keepOverflowFlag.}:
    let x = a + b
    echo overflowFlag()

main(1, high(int))

# A suspension point inside a `{.keepOverflowFlag.}:` block in a `.passive`
# proc. The block cannot be dropped like a `cast` block, so its body is split at
# the state labels; without that the resume state was empty and the program
# hung.
import std/syncio

proc six(): int {.passive.} = 6

proc one(): int {.passive.} =
  result = 0
  {.keepOverflowFlag.}:
    discard six()
    inc result

proc two(a, b: int): int {.passive.} =
  result = 0
  {.keepOverflowFlag.}:
    let x = a + b
    echo overflowFlag()
    result += six()
    let y = a + b
    echo overflowFlag()
    result += six()

proc main() {.passive.} =
  echo one()
  echo two(1, high(int))

main()

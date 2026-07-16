# issue #2097
import std/assertions

proc testClosures(clsr1, clsr2: proc (): int {.closure.}; select: bool) =
  assert clsr1() == 3
  assert (if select: clsr1 else: clsr2)() == 7

proc main =
  var a = 3
  var b = 7
  proc clsr1(): int {.closure.} = a
  proc clsr2(): int {.closure.} = b

  testClosures(clsr1, clsr2, false)

  proc nonClsr(): int = 7

  testClosures(clsr1, nonClsr, false)

main()

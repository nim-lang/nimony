import std / syncio

iterator countup(a, b: int): int {.closure.} =
  var i = a
  while i <= b:
    yield i
    inc i

proc main() =
  for x in countup(1, 100):
    if x > 3:
      break
    echo "iter ", x
  echo "after loop"

  # Re-enter the loop after a break; the previous coroutine frame must have
  # been freed by `finalizeCoroutine` in the trampoline's finally clause.
  for y in countup(10, 12):
    echo "second ", y
  echo "done"

main()

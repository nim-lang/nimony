import std / syncio

iterator countup(a, b: int): int {.closure.} =
  var i = a
  while i <= b:
    yield i
    inc i

proc main() =
  for x in countup(1, 5):
    echo x

main()

# `-d:maxMem=1` (see `nimony.args`) caps what this thread may hand out, which is
# how the out-of-memory paths are reached on purpose. Inside a `.raises`
# routine a failed `new` raises `OutOfMemError`, so the program recovers.
import std / [syncio]

type Node = ref object
  data: array[64, int]

proc grab(n: int): seq[Node] {.raises.} =
  result = @[]
  for i in 0 ..< n:
    result.add Node(data: default(array[64, int]))

proc main() =
  try:
    let s = grab(100_000)
    echo "allocated ", s.len
  except ErrorCode as e:
    echo "caught: ", e
  echo "still running"

main()

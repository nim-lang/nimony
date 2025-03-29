
import std / [syncio]

type
  TA = array[0..3, int]

proc main(a: TA) =
  for i in 0..4:
    echo a[i]
    flushFile(stdout)

main([1, 2, 3, 4])

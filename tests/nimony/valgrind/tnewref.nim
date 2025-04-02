
import std / [syncio]

proc main =
  var x: ref int
  new x
  x[] = 56
  let y = x[]
  echo y

main()

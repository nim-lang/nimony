
import std / [syncio]

proc main =
  var x: ref int
  new x
  x[] = 56
  echo x[]

main()

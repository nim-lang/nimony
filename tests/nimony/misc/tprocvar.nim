import std / syncio

type
  Callback = proc (a, b: int)

proc theProc(a, b: int) =
  echo a, " ", b

proc main =
  var c: Callback = theProc
  c(1, 3)

main()

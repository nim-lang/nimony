import std / syncio

type
  MyS = distinct string

proc write*(f: File; s: MyS) = write f, string(s)

proc main =
  var s90 = MyS"abc"
  echo s90

main()

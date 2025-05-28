import std / syncio

proc byvar[T](x: T) = echo "T"
proc byvar(x: int) = echo "int"

proc main =
  let x = 4
  byvar(x)

  byvar 'x'

main()

import std / syncio

proc byvar(x: var int) = discard

proc main =
  let x = 4
  x = 23

main()

proc lets(b: bool) =
  let x: int
  if b:
    x = 23
  else:
    x = 445

main()
lets(true)

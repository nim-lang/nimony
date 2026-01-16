import std/syncio

template myif(thenPart: untyped) =
  thenPart

myif:
  echo "foo"

template myif(cond: bool; thenPart, elsePart: untyped) =
  if cond:
    thenPart
  else:
    elsePart

myif true:
  echo "bar"
do:
  echo "baz"
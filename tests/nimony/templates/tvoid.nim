
import std/[syncio]

# bug #1421

template myif(thenPart: void) =
  thenPart

myif:
  echo "Bartholomew"


template myifB(cond: bool; thenPart, elsePart: void) =
  if cond:
    thenPart
  else:
    elsePart

myifB true:
  echo "Kuma"
do:
  echo "baz"


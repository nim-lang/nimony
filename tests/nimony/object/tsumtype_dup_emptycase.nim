import std/syncio

type
  Bad = object
    case
    of A:
      x: int
    case
    of B:
      y: int

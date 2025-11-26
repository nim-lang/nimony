import std/syncio

type
  Direction = enum
    Left
    Right

  Edge = enum
    Left
    Top


proc direction(dir: Direction) =
  echo dir

proc edge(edge: Edge) =
  echo edge


direction Direction.Left
direction Right
edge Edge.Left
edge Top

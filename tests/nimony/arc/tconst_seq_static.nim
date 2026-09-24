import std/syncio

type
  Shape* = object
    extents*: seq[int]

const shape23* = Shape(extents: @[2, 3])

func rank*(s: Shape): int =
  s.extents.len

type
  Box*[S: static[Shape]] = object
    n*: int

func staticRank*[S: static[Shape]](_: typedesc[Box[S]]): int =
  S.extents.len

block staticTypeMaterialization:
  let canonical = staticRank(Box[shape23])
  if canonical != rank(shape23):
    quit 1

echo "ok"

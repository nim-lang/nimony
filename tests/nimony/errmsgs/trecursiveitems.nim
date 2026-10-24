# issue #2666: the implicit items call resolves to the enclosing iterator via the converter
type
  ArrayShape = object
    extents: seq[int]

func arrayShape(extents: seq[int]): ArrayShape =
  ArrayShape(extents: extents)

func usesOpenArray(x: ArrayShape; axes: openArray[int]): bool =
  discard x
  axes.len >= 0

converter toArrayShape(extents: seq[int]): ArrayShape =
  arrayShape(extents)

iterator items(x: ArrayShape): int =
  for extent in x.extents:
    yield extent

proc main() =
  let shape = arrayShape(@[2, 3, 4])
  discard usesOpenArray(shape, @[0, 1, 2])
  var sum = 0
  for e in shape:
    sum += e
  discard sum
main()

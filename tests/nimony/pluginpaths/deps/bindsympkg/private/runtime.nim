type
  Box* = object
    value*: int

proc makeBox*(): Box =
  result = Box(value: 42)

proc accept*(box: Box) =
  discard box.value

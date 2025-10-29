type
  # Base of our OOP hierarchy
  Shape {.inheritable.} = object

  Circle = object of Shape
    r: float

  Rect = object of Shape
    w, h: float

proc main =
  # Polymorphic container with mixed dynamic types
  var shapes: seq[Shape] = @[
    Circle(r: 3.0).Shape,
    Rect(w: 4.0, h: 5.0).Shape,
  ]

main()
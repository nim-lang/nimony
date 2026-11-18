# The same constructor shape as `tcaseobj_runtime_discriminator`'s `mkShape`,
# built in a second module: both modules mint the same hidden init proc and
# DCE keeps one.

type
  ShapeKind* = enum skCircle, skRect
  Shape* = object
    name*: string
    case kind*: ShapeKind
    of skCircle: r*: int = 3
    of skRect: w*, h*: int = 2

proc mkShapeElsewhere*(k: ShapeKind; name: string): Shape {.noinline.} =
  Shape(kind: k, name: name)

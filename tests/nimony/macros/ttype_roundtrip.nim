# Type declarations must survive a macro (#3044 item 7).
#
# The NIF <-> NimNode codec mapped `fld`, `param` and `typevar` all to
# `nnkIdentDefs` and wrote that back as `param`, so an object's fields came
# back as parameters. A `case` branch's `(ranges ...)` has no NimNodeKind and
# came back as an empty node. A node read from NIF now keeps its tag.
#
# Also, nimsem checks a call's arguments before it knows the callee takes them
# `untyped`. A sum type declared in such an argument left its hidden
# discriminator enum, and its branch constructors, behind for an owner type
# that is never emitted.
import std / [syncio, macros]

macro passthrough(body: untyped): untyped =
  result = body

macro fromText(): untyped =
  result = parseStmt("""
type
  Size = object
    w, h: int
""")

passthrough:
  type
    Point = object
      x: int
      y: int

passthrough:
  type
    Shape = object
      case
      of Circle:
        r: int
      of Square:
        side: int

fromText()

let p = Point(x: 1, y: 2)
echo p.x + p.y

let s: Shape = Circle(r: 2)
case s
of Circle(r): echo r
of Square(side): echo side

let z = Size(w: 3, h: 4)
echo z.w * z.h

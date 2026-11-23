# A sum type declared in an `untyped` template argument (#3044 item 7).
# nimsem checks the argument before it knows the parameter is `untyped`, then
# expands the template from the argument as written. The first check's hidden
# discriminator enum and branch constructors must not survive it: the enum's
# owner is never emitted, and in a routine the constructors made `Square`
# ambiguous between the two checks' types.
import std / [syncio]

template passthrough(body: untyped): untyped =
  body

passthrough:
  type
    Shape = object
      case
      of Circle:
        r: int
      of Square:
        side: int

proc main =
  passthrough:
    type
      Local = object
        case
        of Dot:
          d: int
        of Line:
          len: int
  let l: Local = Line(len: 5)
  case l
  of Dot(d): echo d
  of Line(len): echo len

let s: Shape = Circle(r: 2)
case s
of Circle(r): echo r
of Square(side): echo side
main()

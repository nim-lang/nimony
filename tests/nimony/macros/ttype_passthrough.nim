# Type declarations survive a macro or an `untyped` template unchanged, and a
# sum type in a block argument is declared once (#3044 item 7).
import std / [syncio, macros]

macro passthrough(body: untyped): untyped =
  result = body

template tpassthrough(body: untyped): untyped =
  body

macro parsed(): untyped =
  result = parseStmt("""
type
  Seg = object
    a, b: int
  Op = object
    case
    of Push:
      val: int
    of Pop:
      discard
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

tpassthrough:
  type
    Tok = object
      case
      of Num:
        n: int
      of Word:
        w: string

parsed()

let p = Point(x: 1, y: 2)
echo p.x + p.y

let s: Shape = Circle(r: 2)
case s
of Circle(r): echo 3 * r * r
of Square(side): echo side * side

let t: Tok = Word(w: "hi")
case t
of Num(n): echo n
of Word(w): echo w

echo Seg(a: 1, b: 2).b
let o: Op = Push(val: 5)
case o
of Push(val): echo val
of Pop(): echo "pop"

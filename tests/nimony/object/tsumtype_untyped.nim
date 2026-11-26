# Sum type patterns in bodies checked only when instantiated or expanded.
import std/[syncio, result, opt]
import deps/msumtype_untyped

type
  Shape = object
    case
    of Circle, Dot:
      r: int
    of Rect:
      w, h: int

let v = 100 # a binding of the same name must shadow it

proc unwrap[T](x: T): T {.untyped.} =
  let r = ok[T, string](x)
  case r
  of Ok(v): result = v
  of Err(e): result = x

proc isSome[T](x: Opt[T]): bool {.untyped.} =
  case x
  of Some(_): result = true
  of None(): result = false

proc area[T](s: Shape; k: T): int {.untyped.} =
  case s
  of {Circle, Dot}(r): result = r * r * k
  of Rect(w, h): result = w * h * k

template unwrapT(x: int): int {.untyped.} =
  var res = 0
  case ok[int, string](x)
  of Ok(v): res = v
  of Err(e): res = -1
  res

echo unwrap(3)
echo isSome(some(3)), " ", isSome(none[int]())
echo area(Dot(r: 4), 1), " ", area(Rect(w: 2, h: 3), 2)
echo unwrapT(5)
echo labelOf(bar), " ", labelOf(baz), " ", labelOfT(bar), " ", labelOfT(foo)
echo unwrapK(7)

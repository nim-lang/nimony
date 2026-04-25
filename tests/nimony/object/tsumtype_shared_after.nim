import std/[syncio, assertions]

## Shared fields may appear after the sum-type `case` section (not only before).

type
  Thing = object
    case
    of Circle:
      radius: float
    of Rect:
      w, h: float
    tag: int
    lbl: string

let c: Thing = Circle(radius: 2.5, tag: 42, lbl: "c")
case c
of Circle:
  assert c.radius == 2.5
else: discard
assert c.tag == 42
assert c.lbl == "c"

let r: Thing = Rect(w: 3.0, h: 4.0, tag: 0, lbl: "r")
case r
of Rect:
  assert r.w == 3.0
  assert r.h == 4.0
else: discard
assert r.tag == 0
assert r.lbl == "r"

proc describe(t: Thing): string =
  case t
  of Circle(radius):
    result = "circle"
  of Rect(w, h):
    result = "rect"

assert describe(c) == "circle"
assert describe(r) == "rect"
# what purpose of checking it twice? ok...
case c
of Circle:
  assert c.radius == 2.5
else: discard

case r
of Rect:
  assert r.w == 3.0
else: discard

echo "tsumtype_shared_after: OK"

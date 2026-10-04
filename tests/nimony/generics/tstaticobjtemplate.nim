import std/syncio
type Tag = object
  n: int
type Box[S: static[Tag]] = object

template sameBox[S1, S2: static[Tag]](x: Box[S1], y: Box[S2]): bool =
  when Box[S1] is Box[S2]:
    true
  else:
    false

# issue #2583: field symbols inside a static object value are not typevars
func wrapped[S1, S2: static[Tag]](x: Box[S1], y: Box[S2]): bool =
  sameBox(x, y)

const a = Tag(n: 1)
const b = Tag(n: 2)
var x: Box[a]
var y: Box[b]
echo wrapped(x, y)
echo wrapped(x, x)

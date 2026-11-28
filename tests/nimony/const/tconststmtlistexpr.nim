# Constants whose initializer is a `block`, `if` or `case` expression keep
# their values.

import std/syncio

const a = block: (let d = 3; d + 1)
const b: int = block:
  var s = 0
  for i in 1..4: s += i
  s
const e = block: (block: 7)
const f = if e == 1: "one" elif e == 7: "seven" else: "other"
const h: int8 = case f
  of "one": 1
  else: 64

proc p =
  const g = block: (let x = 2; x * 21)
  echo g

echo a, " ", b, " ", e, " ", f, " ", h
p()

# Such an expression as an element of an array, tuple or set constructor is
# evaluated with the element's type, not the type of the whole constant.
type O = object
  t: (int, int)
const arr = [(block: 2), 3]
const tup = (x: (block: "y"), z: [(block: 4'i8), 5])
const st = {(block: 1'u8)..3'u8, 7'u8}
const obj = O(t: ((block: 5), 6))
echo arr[0], " ", arr[1], " ", tup.x, " ", tup.z[0], " ", card(st), " ", obj.t[0]

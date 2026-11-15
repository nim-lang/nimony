# Constants whose initializer is a `block` expression keep their values.

import std/syncio

const a = block: (let d = 3; d + 1)
const b: int = block:
  var s = 0
  for i in 1..4: s += i
  s
const e = block: (block: 7)

proc p =
  const g = block: (let x = 2; x * 21)
  echo g

echo a, " ", b, " ", e
p()

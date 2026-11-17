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

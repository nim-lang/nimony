# issue #1598

import std/assertions

const
  a = 1
  b = 2
  c = 4
  ab = a or b
  ac = a and c
  bc = b xor c
  anot = not a
  bShra = b shr a
  bShla = b shl a

assert ab == (a or b)
assert ac == (a and c)
assert bc == (b xor c)
assert anot == (not a)
assert bShra == (b shr a)
assert bShla == (b shl a)

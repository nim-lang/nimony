## `of` labels are constant-*expressions*, not only literals: they are folded
## before the exhaustiveness check and before the backends see them, which used
## to leave e.g. `pred('Q')` in the tree as a `(sub 'Q' 1)` the C backend then
## rejected with "expected valid `of` value".

import std/assertions

type E = enum
  eA, eB, eC, eD

const K = 3

proc fchar(c: char): int =
  case c
  of 'A'..pred('Q'): 1
  of succ('Q')..'Z': 2
  of 'Q': 3
  else: 0

proc fint(x: int): int =
  case x
  of 1+1: 10
  of 5..K+7: 20
  of 100: 30
  else: 0

proc fenum(e: E): int =
  case e
  of eA..pred(eC): 1
  of eC, succ(eC): 2

proc fstr(s: string): int =
  case s
  of "a" & "b": 1
  of "c": 2
  else: 0

assert fchar('A') == 1
assert fchar('B') == 1
assert fchar('P') == 1
assert fchar('R') == 2
assert fchar('Z') == 2
assert fchar('Q') == 3
assert fchar('z') == 0

assert fint(2) == 10
assert fint(5) == 20
assert fint(10) == 20
assert fint(11) == 0
assert fint(100) == 30

assert fenum(eA) == 1
assert fenum(eB) == 1
assert fenum(eC) == 2
assert fenum(eD) == 2

assert fstr("ab") == 1
assert fstr("c") == 2
assert fstr("zz") == 0

import std/assertions

type Foo = enum
  A, B, C, D, E, F

var s, s1: set[Foo]
s = {A..D}
s1 = {A..C}
assert s1 < s
let y = s1 * s
let z: set[Foo] = y
assert y == {A..C}
assert z == {A..C}
assert s - s1 == {D}
assert s1 <= s
assert card(s) == 4
assert card(s1) == 3
let val = D
s1 = {A..B, val, F}
assert card(s1) == 4

template resem() =
  s = {A, C..E, F}
  s1 = {A..B, val, F}

resem()
block:
  var sss: set[char]
  sss = {} # empty set
  assert card(sss) == 0
  sss = {'a' .. 'z', '_'}
  assert card(sss) == 27
  sss.excl('m')
  assert card(sss) == 26
  sss.incl('u')
  assert card(sss) == 26

block:
  proc test() =
    discard {'a'}

# issue #1459
block:
  proc foo: char = 'a'
  var x: set[char] = {}
  x.incl foo()

  assert x == {'a'}

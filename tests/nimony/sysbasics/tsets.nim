type Foo = enum
  A, B, C, D, E, F

var s, s1: set[Foo]
s = {A..D}
s1 = {A..C}
discard s1 < s
let y = s1 * s
let z: set[Foo] = y
discard y == {A..C}
discard z == {A..C}
discard s - s1 == {D}
discard s1 <= s
let val = D
s1 = {A..B, val, F}

template resem() =
  s = {A, C..E, F}
  s1 = {A..B, val, F}

resem()
block:
  var sss: set[char]
  sss = {} # empty set
  sss = {'a' .. 'z', '_'}
  sss.excl('m')
  sss.incl('u')

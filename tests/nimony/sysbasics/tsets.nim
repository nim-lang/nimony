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

template resem() =
  s = {A, C..E, F}

resem()

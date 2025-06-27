import std/assertions

const a = {'a', 'b'}
const b = {'0', '1'} + {'8', '9'}
assert b == {'0', '1', '8', '9'}

const c = a + {'x'}
assert c == {'a', 'b', 'x'}

const d = {'a'..'c'}
assert d == {'a', 'b', 'c'}
const e = {'0'..'3', '6'..'9', '3'..'7'}
assert e == {'0'..'9'}
const f = {5'i8..8, 9'i8..10}
assert f == {5'i8, 6, 7, 8, 9, 10}
const g = {1'i8..3} + {5'i8..8} + {10'i8..14'i8}
assert g == {1'i8, 2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14}

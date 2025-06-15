import std/assertions

const d = {'a'..'c'}
assert d == {'a', 'b', 'c'}
const e = {'0'..'3', '6'..'9', '3'..'7'}
assert e == {'0'..'9'}
const f = {5'i8..8, 9'i8..10}
assert f == {5'i8, 6, 7, 8, 9, 10}

import std/[assertions, lenientops, syncio]

assert 2 + 3.5 == 5.5
assert 3.5 + 2 == 5.5
assert 5 - 1.5 == 3.5
assert 5.5 - 2 == 3.5
assert 2 * 1.5 == 3.0
assert 1.5 * 2 == 3.0
assert 3 / 2.0 == 1.5
assert 3.0 / 2 == 1.5

assert 2 < 2.5
assert 2.5 < 3
assert 2 <= 2.0
assert 2.0 <= 2

let f32 = 1'i32 + 2.5'f32
assert f32 == 3.5'f32

echo "ok"

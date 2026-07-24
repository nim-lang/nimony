import std/assertions

# `x in a .. b` / `contains(Slice[T], x)` for the comparable scalar types.
assert 128'u8 in 127'u8 .. 129'u8
assert 5'u8 notin 127'u8 .. 129'u8
assert 50 in 0 .. 100
assert -1 notin 0 .. 100
assert 3.5 in 1.0 .. 4.0
assert 'c' in 'a' .. 'z'
assert contains(0 .. 10, 10)   # inclusive upper bound
assert not contains(0 .. 10, 11)

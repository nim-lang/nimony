import std/syncio

# `echo` uses `write`, which is overloaded for `int` and `float`, but not specific  sizes.
# The expectation is that other numeric types will be widened to the best match.

echo 5'i8
echo 5'i16
echo 5'i32
echo 5'i64
echo 5'u8
echo 5'u16
echo 5'u32
echo 5'u64
echo 5.0'f32
echo 5.0'f64

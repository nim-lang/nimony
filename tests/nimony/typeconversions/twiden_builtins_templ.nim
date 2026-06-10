import std/syncio

# Widening must work in typed templates.

template five(x: typedesc[int8]): int8 = 5'i8
template five(x: typedesc[int16]): int16 = 5'i16
template five(x: typedesc[int32]): int32 = 5'i32
template five(x: typedesc[int64]): int64 = 5'i64
template five(x: typedesc[uint8]): uint8 = 5'u8
template five(x: typedesc[uint16]): uint16 = 5'u16
template five(x: typedesc[uint32]): uint32 = 5'u32
template five(x: typedesc[uint64]): uint64 = 5'u64
template five(x: typedesc[float32]): float32 = 5.0'f32
template five(x: typedesc[float64]): float64 = 5.0'f64

echo int8.five
echo int16.five
echo int32.five
echo int64.five
echo uint8.five
echo uint16.five
echo uint32.five
echo uint64.five
echo float32.five
echo float64.five

# `int` and `float` are aliases

echo int.five
echo float.five

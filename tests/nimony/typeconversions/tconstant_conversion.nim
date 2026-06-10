import std/syncio

proc printI32(a: int32) = echo a
proc printI64(a: int64) = echo a
proc printF32(a: float32) = echo a
proc printF64(a: float64) = echo a

printI32 1'i32 + 2
printI64 1 + 3

printF32 1.0'f32 + 4.12
printF32 1.0'f32 + 5
printF64 1.0 + 6.0
printF64 1.0 + 7

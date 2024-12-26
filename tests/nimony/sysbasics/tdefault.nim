template default*(x: typedesc[bool]): bool = false
template default*(x: typedesc[char]): char = '\0'
template default*(x: typedesc[int]): int = 0
template default*(x: typedesc[uint]): uint = 0'u
template default*(x: typedesc[int8]): int8 = 0'i8
template default*(x: typedesc[uint8]): uint8 = 0'u8
template default*(x: typedesc[int16]): int16 = 0'i16
template default*(x: typedesc[uint16]): uint16 = 0'u16
template default*(x: typedesc[int32]): int32 = 0'i32
template default*(x: typedesc[uint32]): uint32 = 0'u32
template default*(x: typedesc[int64]): int64 = 0'i64
template default*(x: typedesc[uint64]): uint64 = 0'u64
template default*(x: typedesc[float32]): float32 = 0.0'f32
template default*(x: typedesc[float64]): float64 = 0.0'f64
template default*(x: typedesc[string]): string = ""
template default*[T: enum](x: typedesc[T]): T = T(0)

template default*[T: ptr](x: typedesc[T]): T = T(nil)
template default*[T: ref](x: typedesc[T]): T = T(nil)
discard default(string)
discard default(int)

discard default(ptr int)
type Enum = enum a, b, c
discard default(Enum)

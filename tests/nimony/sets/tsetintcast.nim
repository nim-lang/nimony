# issue #2602: a word-sized set casts to and from an integer (bit fields)
import std/[assertions, syncio]

type
  MyFlag* {.size: sizeof(cint).} = enum
    A
    B
    C
    D
  MyFlags = set[MyFlag]

  Small = enum
    X, Y, Z

proc toNum(f: MyFlags): int = cast[cint](f)
proc toFlags(v: int): MyFlags = cast[MyFlags](v)

assert toNum({}) == 0
assert toNum({A}) == 1
assert toNum({D}) == 8
assert toNum({A, C}) == 5
assert toFlags(0) == {}
assert toFlags(7) == {A, B, C}

assert cast[uint8]({X, Z}) == 5'u8
assert cast[set[Small]](2'u8) == {Y}
echo toNum({A, B, D}), " ", cast[uint8]({Y, Z})

import std/assertions

# Numeric union types from `system/basic_types`

template testSignedInt[T](x: typedesc[T]) =
  assert x is SomeSignedInt
  assert x isnot SomeUnsignedInt
  assert x is SomeInteger
  assert x isnot SomeFloat
  assert x is SomeNumber
  assert x is SomeOrdinal

template testUnsignedInt[T](x: typedesc[T]) =
  assert T isnot SomeSignedInt
  assert x is SomeUnsignedInt
  assert x is SomeInteger
  assert x isnot SomeFloat
  assert x is SomeNumber
  assert x is SomeOrdinal

template testFloat[T](x: typedesc[T]) =
  assert x isnot SomeSignedInt
  assert x isnot SomeUnsignedInt
  assert x isnot SomeInteger
  assert x is SomeFloat
  assert x is SomeNumber
  assert x isnot SomeOrdinal

testSignedInt int
testSignedInt int8
testSignedInt int16
testSignedInt int32
testSignedInt int64

testUnsignedInt uint
testUnsignedInt uint8
testUnsignedInt uint16
testUnsignedInt uint32
testUnsignedInt uint64

testFloat float
testFloat float32
testFloat float64

assert bool isnot SomeSignedInt
assert bool isnot SomeUnsignedInt
assert bool isnot SomeInteger
assert bool isnot SomeFloat
assert bool isnot SomeNumber
assert bool is SomeOrdinal


# `enum` union from `system/basic_types`

type
  EnumWithoutHoles = enum
    e0, e1, e2

  EnumWithHoles = enum
    a = 0
    b = 2
    c = 3

  DenseEnum = enum
    x = 0
    y = 1
    z = 2

  SmallEnum {.size: 1.} = enum
    p, q, r

  SizeEnum {.size: sizeof(cint).} = enum
    a = 0
    b = 1
    c = 2

template testOrdinalEnum[T](x: typedesc[T]) =
  assert x is enum
  assert x is OrdinalEnum
  assert x isnot HoleyEnum
  assert x is SomeOrdinal
  assert x isnot int
  assert x isnot bool

template testHoleyEnum[T](x: typedesc[T]) =
  assert x is enum
  assert x is HoleyEnum
  assert x isnot OrdinalEnum
  assert x is SomeOrdinal
  assert x isnot int
  assert x isnot bool

testOrdinalEnum EnumWithoutHoles
testOrdinalEnum DenseEnum
testOrdinalEnum SmallEnum
testOrdinalEnum SizeEnum
testHoleyEnum EnumWithHoles


# Non-enum ordinals (in SomeOrdinal but not in the `enum` union)

assert bool isnot enum
assert int isnot enum

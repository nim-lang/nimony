import std/assertions

# Numeric union types from `system/basic_types`

template testSignedInt(T: typedesc) =
  assert T is SomeSignedInt
  assert T isnot SomeUnsignedInt
  assert T is SomeInteger
  assert T isnot SomeFloat
  assert T is SomeNumber
  assert T is SomeOrdinal

template testUnsignedInt(T: typedesc) =
  assert T isnot SomeSignedInt
  assert T is SomeUnsignedInt
  assert T is SomeInteger
  assert T isnot SomeFloat
  assert T is SomeNumber
  assert T is SomeOrdinal

template testFloat(T: typedesc) =
  assert T isnot SomeSignedInt
  assert T isnot SomeUnsignedInt
  assert T isnot SomeInteger
  assert T is SomeFloat
  assert T is SomeNumber
  assert T isnot SomeOrdinal

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

template testOrdinalEnum(T: typedesc) =
  assert T is enum
  assert T is OrdinalEnum
  assert T isnot HoleyEnum
  assert T is SomeOrdinal
  assert T isnot int
  assert T isnot bool

template testHoleyEnum(T: typedesc) =
  assert T is enum
  assert T is HoleyEnum
  assert T isnot OrdinalEnum
  assert T is SomeOrdinal
  assert T isnot int
  assert T isnot bool

testOrdinalEnum EnumWithoutHoles
testOrdinalEnum DenseEnum
testOrdinalEnum SmallEnum
testOrdinalEnum SizeEnum
testHoleyEnum EnumWithHoles


# Non-enum ordinals (in SomeOrdinal but not in the `enum` union)

assert bool isnot enum
assert int isnot enum

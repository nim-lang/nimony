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

# `enum` union from `system/basic_types` (OrdinalEnum | HoleyEnum)

type
  NormalEnum = enum
    e0, e1, e2
  HoleyEnumType = enum
    a = 0
    b = 2
    c = 3

template testNormalEnum(T: typedesc) =
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

testNormalEnum NormalEnum
testHoleyEnum HoleyEnumType

# Non-enum ordinals: in SomeOrdinal but not in the `enum` union
assert bool is SomeOrdinal
assert bool isnot enum
assert int is SomeOrdinal
assert int isnot enum

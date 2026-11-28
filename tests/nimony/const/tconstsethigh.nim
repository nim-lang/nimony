import std/[assertions, setutils]

const
  singleton = {255'u8} + {0'u8}
  shortRun = {253'u8..255'u8} + {0'u8}
  longRun = {248'u8..255'u8} + {0'u8}
  full = {0'u8..255'u8} + {0'u8}
  difference = {0'u8..255'u8} - {0'u8..247'u8}
  intersection = {248'u8..255'u8} * {250'u8..255'u8}
  symmetric = {0'u8, 255'u8} -+- {0'u8}
  empty = {255'u8} - {255'u8}
  chars = {'\255'} + {'a'}

assert singleton == {0'u8, 255'u8}
assert shortRun == {0'u8, 253'u8..255'u8}
assert longRun == {0'u8, 248'u8..255'u8}
assert full == {0'u8..255'u8}
assert difference == {248'u8..255'u8}
assert intersection == {250'u8..255'u8}
assert symmetric == {255'u8}
assert empty == {}
assert chars == {'a', '\255'}

const hasHigh = 255'u8 in singleton
assert hasHigh

block:
  type Eight = enum e0, e1, e2, e3, e4, e5, e6, e7
  const ends = {e7} + {e0}
  const all = {e0..e7} * {e0..e7}
  assert ends == {e0, e7}
  assert all == {e0..e7}

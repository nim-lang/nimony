import std/[assertions, setutils]

const a = {'a', 'b'}
const b = {'0', '1'} + {'8', '9'}
assert b == {'0', '1', '8', '9'}

block:
  const c = b - {'0', '1'}
  assert c == {'8', '9'}

const c = a + {'x'}
assert c == {'a', 'b', 'x'}

block:
  const d = a - {'b'}
  assert d == {'a'}

  const e = c - {'x'}
  assert e == {'a', 'b'}

const d = {'a'..'c'}
assert d == {'a', 'b', 'c'}
const e = {'0'..'3', '6'..'9', '3'..'7'}
assert e == {'0'..'9'}
const f = {5'i8..8, 9'i8..10}
assert f == {5'i8, 6, 7, 8, 9, 10}
const g = {1'i8..3} + {5'i8..8} + {10'i8..14'i8}
assert g == {1'i8, 2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14}


block:
  # Set subtraction (-)
  const a = {'a', 'b', 'c'}
  const b = a - {'b'}
  assert b == {'a', 'c'} # {'a', 'b', 'c'} - {'b'} == {'a', 'c'}

  const c = {'0', '1', '2', '3'}
  const d = c - {'2', '3'}
  assert d == {'0', '1'} # {'0', '1', '2', '3'} - {'2', '3'} == {'0', '1'}

  # Set symmetric difference (-+-)
  const e = {'a', 'b', 'c'}
  const f = {'b', 'c', 'd'}
  const g = e -+- f
  assert g == {'a', 'd'} # {'a', 'b', 'c'} -+- {'b', 'c', 'd'} == {'a', 'd'}

  const h = {'1', '2', '3'}
  const i = {'2', '3', '4'}
  const j = h -+- i
  assert j == {'1', '4'} # {'1', '2', '3'} -+- {'2', '3', '4'} == {'1', '4'}

  # Set intersection (*)
  const k = {'a', 'b', 'c'}
  const l = {'b', 'c', 'd'}
  const m = k * l
  assert m == {'b', 'c'} # {'a', 'b', 'c'} * {'b', 'c', 'd'} == {'b', 'c'}

  const n = {'1', '2', '3'}
  const o = {'2', '3', '4'}
  const p = n * o
  assert p == {'2', '3'} # {'1', '2', '3'} * {'2', '3', '4'} == {'2', '3'}

block enumsets:
  type
    TEnum = enum A, B, C
  const cset = {A} + {B}

  const s1 = A in cset
  const s2 = B in cset
  const s3 = C notin cset
  assert s1
  assert s2
  assert s3

  assert A in cset
  assert B in cset
  assert C notin cset

block:
  const cset = {1'i8, 2'i8, 6'i8..12'i8}

  const s1 = 1'i8 in cset
  assert s1

  const s2 = 3'i8 in cset
  assert not s2

  const s3 = 6'i8 in cset
  assert s3

  const s4 = 12'i8 in cset
  assert s4

  const s5 = 8'i8 in cset
  assert s5

  const s6 = 13'i8 in cset
  assert not s6

block:
  const
    Whitespace = {' ', '\t', '\n', '\r'}

    s1 = card(Whitespace)

    Data = {1'i8, 2, 3, 4, 5}
    s2 = len(Data)


  assert s1 == 4
  assert s2 == 5

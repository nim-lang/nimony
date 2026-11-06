import std/[assertions, math, rationals]

block: # construction and reduction
  var z = Rational[int](num: 0, den: 1)
  let o = initRational(num = 1, den = 1)
  let a = initRational(1, 2)
  let b = -1 // -2
  let tt = 10 // 2

  assert a == a
  assert a - a == z
  assert a + b == o
  assert a / b == o
  assert a * b == 1 // 4
  assert 3 / a == 6 // 1
  assert a / 3 == 1 // 6
  assert tt * z == z
  assert 10 * a == tt
  assert a * 10 == tt
  assert tt / 10 == a

  var r = Rational[int](num: 2, den: 4)
  reduce(r)
  assert r == 1 // 2
  assert $r == "1/2"

block: # comparisons and hashing
  let z = 0 // 1
  let o = 1 // 1
  let a = 1 // 2
  let b = -1 // -2

  assert z < o
  assert z <= o
  assert z == z
  assert cmp(z, o) < 0
  assert cmp(o, z) > 0
  assert o == o
  assert o >= o
  assert not (o > o)
  assert cmp(o, o) == 0
  assert cmp(z, z) == 0
  assert hash(o) == hash(o)
  assert a == b
  assert a >= b
  assert not (b > a)
  assert cmp(a, b) == 0
  assert hash(a) == hash(b)

block: # in-place arithmetic
  var x = 1 // 3
  x *= 5 // 1
  assert x == 5 // 3
  x += 2 // 9
  assert x == 17 // 9
  x -= 9 // 18
  assert x == 25 // 18
  x /= 1 // 2
  assert x == 50 // 18

  var y = 1 // 3
  y *= 4
  assert y == 4 // 3
  y += 5
  assert y == 19 // 3
  y -= 2
  assert y == 13 // 3
  y /= 9
  assert y == 13 // 27

block: # conversions
  let y = 13 // 27
  assert toRational(5) == 5 // 1
  assert abs(toFloat(y) - 0.4814814814814815) < 1.0e-7
  assert toInt(0 // 1) == 0
  assert toInt(3 // 2) == 1
  assert toFloat(1 // 2) == 0.5

  when sizeof(int) == 8:
    assert toRational(0.98765432) == 2111111029 // 2137499919
    assert toRational(PI) == 817696623 // 260280919
  when sizeof(int) == 4:
    assert toRational(0.98765432) == 80 // 81
    assert toRational(PI) == 355 // 113

  assert toRational(0.1) == 1 // 10
  assert toRational(0.9) == 9 // 10
  assert toRational(0.0) == 0 // 1
  assert toRational(-0.25) == 1 // -4
  assert toRational(3.2) == 16 // 5
  assert toRational(0.33) == 33 // 100
  assert toRational(0.22) == 11 // 50
  assert toRational(10.0) == 10 // 1
  assert toRational(0.5, 100) == 1 // 2

block: # division and modulo
  assert (1 // 1) div (3 // 10) == 3
  assert (-1 // 1) div (3 // 10) == -3
  assert (3 // 10) mod (1 // 1) == 3 // 10
  assert (-3 // 10) mod (1 // 1) == -3 // 10
  assert floorDiv(1 // 1, 3 // 10) == 3
  assert floorDiv(-1 // 1, 3 // 10) == -4
  assert floorMod(3 // 10, 1 // 1) == 3 // 10
  assert floorMod(-3 // 10, 1 // 1) == 7 // 10

block: # reciprocal, powers and absolute value
  assert reciprocal(1 // 2) == 2 // 1
  assert reciprocal(-1 // 2) == -2 // 1
  assert (-3 // 5) ^ 0 == 1 // 1
  assert (-3 // 5) ^ 1 == -3 // 5
  assert (-3 // 5) ^ 2 == 9 // 25
  assert (-3 // 5) ^ -2 == 25 // 9
  assert (0 // 1) ^ 0 == 1 // 1
  assert (0 // 1) ^ 1 == 0 // 1
  assert abs(1 // 2) == 1 // 2
  assert abs(-1 // 2) == 1 // 2

block: # unsigned components
  let u = 3u // 2
  assert u == u
  assert u + u == 3u // 1
  assert 3u.toRational - u == u
  assert u * 2 == 3u // 1
  assert u / 2u == 3u // 4
  assert reciprocal(u) == 2u // 3
  assert u ^ 2u == 9u // 4
  assert u div (1u // 2u) == 3u
  assert toFloat(u) == 1.5
  assert toInt(u) == 1
  assert hash(u) == hash(3u // 2)
  assert $u == "3/2"
  assert abs(u) == u

when sizeof(int) == 8:
  assert almostEqual(PI.toRational.toFloat, PI)

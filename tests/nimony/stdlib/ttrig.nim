import std/[math, assertions]

# Exercise the libc-free implementation with `-d:nimNativeIo`.
assert almostEqual(sin(PI / 6.0), 0.5)
assert almostEqual(sin(-PI / 6.0), -0.5)
assert almostEqual(sin(PI / 2.0), 1.0)
assert almostEqual(cos(PI / 3.0), 0.5)
assert almostEqual(cos(PI), -1.0)
assert almostEqual(tan(PI / 4.0), 1.0)
assert almostEqual(tan(-PI / 4.0), -1.0)
assert abs(sin(1e20) - (-0.6452512852657808)) < 1e-12
assert abs(sin(-1e20) - 0.6452512852657808) < 1e-12
assert abs(sin(1e300) - (-0.8178819121159085)) < 1e-12
assert abs(sin(1.7976931348623157e308) - 0.004961954789184062) < 1e-12

assert almostEqual(arcsin(-0.5), -PI / 6.0)
assert almostEqual(arcsin(1.0), PI / 2.0)
assert arcsin(1.01).isNaN
assert almostEqual(arccos(0.5), PI / 3.0)
assert almostEqual(arccos(-1.0), PI)
assert arccos(-1.01).isNaN
assert almostEqual(arctan(-1.0), -PI / 4.0)
assert almostEqual(arctan(Inf), PI / 2.0)
assert arctan(NaN).isNaN

assert almostEqual(arctan2(1.0, 1.0), PI / 4.0)
assert almostEqual(arctan2(1.0, -1.0), 3.0 * PI / 4.0)
assert almostEqual(arctan2(-1.0, -1.0), -3.0 * PI / 4.0)
assert almostEqual(arctan2(1.0, 0.0), PI / 2.0)
assert arctan2(0.0, -1.0) == PI

assert almostEqual(sin(PI.float32 / 6.0'f32), 0.5'f32)
assert almostEqual(cos(PI.float32), -1.0'f32)
assert almostEqual(tan(PI.float32 / 4.0'f32), 1.0'f32)
assert abs(sin(1e20'f32) - 0.6565767'f32) < 1e-6'f32
assert abs(sin(-1e20'f32) - (-0.6565767'f32)) < 1e-6'f32
assert abs(sin(3e38'f32) - 0.8749049'f32) < 1e-6'f32
assert almostEqual(arcsin(-0.5'f32), -PI.float32 / 6.0'f32)
assert almostEqual(arccos(0.5'f32), PI.float32 / 3.0'f32)
assert almostEqual(arctan(-1.0'f32), -PI.float32 / 4.0'f32)
assert almostEqual(arctan2(-1.0'f32, -1.0'f32), -3.0'f32 * PI.float32 / 4.0'f32)

assert sin(Inf).isNaN
assert cos(-Inf).isNaN
assert tan(NaN).isNaN

block:
  for x in [0.0, -0.0, 0.3, 1.0, 2.0, -2.5, 4.0, 5.5, 100.0, 1e20, -1e300]:
    let (s, c) = sincos(x)
    assert s == sin(x)
    assert c == cos(x)
  let (s0, c0) = sincos(-0.0)
  assert s0 == 0.0 and c0 == 1.0
  assert sincos(NaN).s.isNaN and sincos(NaN).c.isNaN
  assert sincos(Inf).s.isNaN and sincos(-Inf).c.isNaN
  let (sf, cf) = sincos(PI.float32 / 6.0'f32)
  assert sf == sin(PI.float32 / 6.0'f32)
  assert cf == cos(PI.float32 / 6.0'f32)

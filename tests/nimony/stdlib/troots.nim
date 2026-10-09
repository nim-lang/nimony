import std/[math, assertions]

block: # sqrt float32 range and special values
  assert almostEqual(sqrt(4.0'f32), 2.0'f32)
  assert almostEqual(sqrt(1e30'f32), 1e15'f32)
  assert almostEqual(sqrt(1e-30'f32), 1e-15'f32)
  assert sqrt(cast[float32](1'u32)) > 0'f32 # least positive subnormal
  assert signbit(sqrt(-0.0'f32))
  assert sqrt(float32(Inf)) == float32(Inf)
  assert sqrt(-1.0'f32).isNaN

block: # sqrt float64 range and special values
  assert almostEqual(sqrt(4.0), 2.0)
  assert almostEqual(sqrt(1e300), 1e150)
  assert almostEqual(sqrt(1e-300), 1e-150)
  assert sqrt(cast[float64](1'u64)) > 0.0 # least positive subnormal
  assert signbit(sqrt(-0.0))
  assert sqrt(Inf) == Inf
  assert sqrt(-1.0).isNaN

block: # cbrt float64 range and special values
  assert almostEqual(cbrt(8.0), 2.0)
  assert almostEqual(cbrt(1e300), 1e100)
  assert almostEqual(cbrt(1e-300), 1e-100)
  assert cbrt(cast[float64](1'u64)) > 0.0
  assert cbrt(-27.0) == -3.0
  assert signbit(cbrt(-0.0))
  assert cbrt(Inf) == Inf
  assert cbrt(-Inf) == -Inf
  assert cbrt(NaN).isNaN

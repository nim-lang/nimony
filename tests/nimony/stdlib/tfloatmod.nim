import std/[math, assertions]

block: # quotient overflow
  let tinyRemainder = 1.0e308 mod 1.0e-308
  assert tinyRemainder == cast[float64](0x00028401cf53d610'u64)
  assert (1.0e38'f32 mod 1.0e-30'f32) == 8.6795644e-31'f32

block: # large exponent gaps and retained remainder bits
  assert cast[float64](0x4630000000000000'u64) mod 3.0 == 1.0 # 2^100
  assert cast[float32](0x67800000'u32) mod 3.0'f32 == 1.0'f32 # 2^80
  assert 1.0e308 mod 3.0 == 2.0

block: # signs and exact multiples
  assert (-6.5) mod 2.5 == -1.5
  assert 6.5 mod (-2.5) == 1.5
  assert (-6.5'f32) mod (-2.5'f32) == -1.5'f32
  assert 12.0 mod 3.0 == 0.0
  assert (-12.0) mod 3.0 == 0.0
  assert signbit((-12.0) mod 3.0)
  assert signbit((-0.0'f32) mod 2.0'f32)

block: # special values
  assert (1.25 mod Inf) == 1.25
  assert (1.25'f32 mod float32(Inf)) == 1.25'f32
  assert (Inf mod 2.0).isNaN
  assert (1.0 mod 0.0).isNaN
  assert (NaN mod 2.0).isNaN
  assert (1.0 mod NaN).isNaN
  assert (1.0'f32 mod float32(Inf)) == 1.0'f32
  assert (float32(Inf) mod 2.0'f32).isNaN
  assert (1.0'f32 mod 0.0'f32).isNaN
  assert (float32(NaN) mod 2.0'f32).isNaN
  assert (1.0'f32 mod float32(NaN)).isNaN

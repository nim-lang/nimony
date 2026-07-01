import std/[assertions, complex, math]

proc close(a, b: float): bool =
  abs(a - b) < 1e-9

proc close(a, b: float32): bool =
  abs(a - b) < 1e-5'f32

proc close(a, b: Complex64): bool =
  close(a.re, b.re) and close(a.im, b.im)

proc close(a, b: Complex32): bool =
  close(a.re, b.re) and close(a.im, b.im)

block: # construction and accessors
  let z = complex(1.0, 2.0)
  assert z.re == 1.0
  assert z.im == 2.0
  assert complex(5.0) == complex(5.0, 0.0)

block: # arithmetic
  let a = complex(1.0, 2.0)
  let b = complex(3.0, -1.0)
  assert a + b == complex(4.0, 1.0)
  assert a - b == complex(-2.0, 3.0)
  assert a * b == complex(5.0, 5.0)        # (1+2i)(3-i) = 5 + 5i
  assert close(a / b, complex(0.1, 0.7))

block: # mixed scalar arithmetic
  let a = complex(2.0, 3.0)
  assert a + 1.0 == complex(3.0, 3.0)
  assert 1.0 + a == complex(3.0, 3.0)
  assert a - 1.0 == complex(1.0, 3.0)
  assert a * 2.0 == complex(4.0, 6.0)
  assert 2.0 * a == complex(4.0, 6.0)
  assert close(a / 2.0, complex(1.0, 1.5))

block: # abs / abs2 / arg / conjugate / unary minus
  let z = complex(3.0, 4.0)
  assert close(abs(z), 5.0)
  assert close(abs2(z), 25.0)
  assert conjugate(z) == complex(3.0, -4.0)
  assert -z == complex(-3.0, -4.0)
  assert close(arg(complex(0.0, 1.0)), PI / 2.0)

block: # inv
  let z = complex(1.0, 2.0)
  assert close(z * inv(z), complex(1.0, 0.0))

block: # sqrt
  assert close(sqrt(complex(-1.0, 0.0)), complex(0.0, 1.0))
  let s = sqrt(complex(3.0, 4.0))
  assert close(s * s, complex(3.0, 4.0))

block: # exp / ln round-trip
  let z = complex(0.5, 1.0)
  assert close(exp(ln(z)), z)

block: # pow
  let z = complex(2.0, 0.0)
  assert close(pow(z, complex(2.0, 0.0)), complex(4.0, 0.0))

block: # stringification
  assert $complex(1.0, 2.0) == "(1.0, 2.0)"

block: # generic over float32 (Complex32)
  let a = complex(1.0'f32, 2.0'f32)
  let b = complex(3.0'f32, -1.0'f32)
  assert a + b == complex(4.0'f32, 1.0'f32)
  assert a * b == complex(5.0'f32, 5.0'f32)
  assert 2.0'f32 * a == complex(2.0'f32, 4.0'f32)
  assert close(abs(complex(3.0'f32, 4.0'f32)), 5.0'f32)
  assert close(abs2(complex(3.0'f32, 4.0'f32)), 25.0'f32)
  assert conjugate(a) == complex(1.0'f32, -2.0'f32)
  let s = sqrt(complex(3.0'f32, 4.0'f32))
  assert close(s * s, complex(3.0'f32, 4.0'f32))

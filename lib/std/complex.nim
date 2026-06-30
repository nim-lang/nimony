#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## This module implements complex numbers and basic mathematical operations
## on them.
##
## A complex number is a number of the form `a + b*i`, where `a` is the real
## part and `b` is the imaginary part (and `i` is the imaginary unit with the
## property `i*i == -1`).
##
## The operations are provided for 64-bit floats (`Complex64`). A generic
## `Complex[T: SomeFloat]` form (as in Nim 2) is not used here because the
## stdlib's float `math`, comparison and `$` operations are concrete per float
## type rather than generic over `SomeFloat`.

import std/math

type
  Complex64* = object
    ## A complex number with 64-bit float components.
    re*, im*: float

  Complex* = Complex64
    ## The default complex number type.

func complex*(re, im: float): Complex64 =
  ## Returns a complex number with real part `re` and imaginary part `im`.
  result = Complex64(re: re, im: im)

func complex*(re: float): Complex64 =
  ## Returns a complex number with real part `re` and zero imaginary part.
  result = Complex64(re: re, im: 0.0)

func abs2*(z: Complex64): float =
  ## Returns the squared absolute value of `z`, i.e. `z.re^2 + z.im^2`.
  ## Cheaper than `abs` because it avoids the square root.
  result = z.re * z.re + z.im * z.im

func abs*(z: Complex64): float =
  ## Returns the absolute value (modulus) of `z`.
  result = hypot(z.re, z.im)

func arg*(z: Complex64): float =
  ## Returns the argument (phase angle, in radians) of `z`.
  result = arctan2(z.im, z.re)

func conjugate*(z: Complex64): Complex64 =
  ## Returns the complex conjugate of `z` (`z.im` negated).
  result = Complex64(re: z.re, im: -z.im)

func `==`*(a, b: Complex64): bool =
  ## Compares two complex numbers for equality.
  result = a.re == b.re and a.im == b.im

func `+`*(a, b: Complex64): Complex64 =
  result = Complex64(re: a.re + b.re, im: a.im + b.im)

func `+`*(a: Complex64; b: float): Complex64 =
  result = Complex64(re: a.re + b, im: a.im)

func `+`*(a: float; b: Complex64): Complex64 =
  result = Complex64(re: a + b.re, im: b.im)

func `-`*(z: Complex64): Complex64 =
  ## Unary minus.
  result = Complex64(re: -z.re, im: -z.im)

func `-`*(a, b: Complex64): Complex64 =
  result = Complex64(re: a.re - b.re, im: a.im - b.im)

func `-`*(a: Complex64; b: float): Complex64 =
  result = Complex64(re: a.re - b, im: a.im)

func `-`*(a: float; b: Complex64): Complex64 =
  result = Complex64(re: a - b.re, im: -b.im)

func `*`*(a, b: Complex64): Complex64 =
  result = Complex64(re: a.re * b.re - a.im * b.im,
                     im: a.re * b.im + a.im * b.re)

func `*`*(a: Complex64; b: float): Complex64 =
  result = Complex64(re: a.re * b, im: a.im * b)

func `*`*(a: float; b: Complex64): Complex64 =
  result = Complex64(re: a * b.re, im: a * b.im)

func `/`*(a, b: Complex64): Complex64 =
  let d = b.re * b.re + b.im * b.im
  result = Complex64(re: (a.re * b.re + a.im * b.im) / d,
                     im: (a.im * b.re - a.re * b.im) / d)

func `/`*(a: Complex64; b: float): Complex64 =
  result = Complex64(re: a.re / b, im: a.im / b)

func inv*(z: Complex64): Complex64 =
  ## Returns the multiplicative inverse `1 / z`.
  let d = z.re * z.re + z.im * z.im
  result = Complex64(re: z.re / d, im: -z.im / d)

func sqrt*(z: Complex64): Complex64 =
  ## Returns the principal square root of `z`.
  if z.re == 0.0 and z.im == 0.0:
    result = Complex64(re: 0.0, im: 0.0)
  else:
    let m = abs(z)
    var s = sqrt((m - z.re) * 0.5)
    if z.im < 0.0: s = -s
    result = Complex64(re: sqrt((m + z.re) * 0.5), im: s)

func exp*(z: Complex64): Complex64 =
  ## Returns the exponential `e^z`.
  let r = exp(z.re)
  result = Complex64(re: r * cos(z.im), im: r * sin(z.im))

func ln*(z: Complex64): Complex64 =
  ## Returns the principal natural logarithm of `z`.
  result = Complex64(re: ln(abs(z)), im: arg(z))

func pow*(z, w: Complex64): Complex64 =
  ## Returns `z` raised to the power `w` (principal value).
  if z.re == 0.0 and z.im == 0.0:
    if w.re == 0.0 and w.im == 0.0:
      result = Complex64(re: 1.0, im: 0.0)
    else:
      result = Complex64(re: 0.0, im: 0.0)
  else:
    result = exp(w * ln(z))

proc `$`*(z: Complex64): string =
  ## Returns a textual representation of `z` as `"(re, im)"`.
  result = "(" & $z.re & ", " & $z.im & ")"

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
## `Complex[T]` is generic over the floating-point component type `T`. Following
## the modelling of `std/math`, each operation depends precisely on the concepts
## it needs: the arithmetic operators come from `Arithmetic`, and the
## transcendental functions add `HasSqrt`, `HasExp`, `HasLn`, `HasSin`,
## `HasCos` and `HasArctan2`. So `Complex[float32]` and `Complex[float64]` both
## work, and a user float type satisfying the relevant concepts works too.

import std/math

type
  Complex*[T: SomeFloat] = object
    ## A complex number with real part `re` and imaginary part `im`, both of
    ## the floating-point type `T`.
    re*, im*: T

  Complex64* = Complex[float64]
    ## A complex number with 64-bit float components.

  Complex32* = Complex[float32]
    ## A complex number with 32-bit float components.

  Stringable = concept
    proc `$`(x: Self): string

func complex*[T: SomeFloat](re, im: T): Complex[T] =
  ## Returns a complex number with real part `re` and imaginary part `im`.
  result = Complex[T](re: re, im: im)

func complex*[T: SomeFloat](re: T): Complex[T] =
  ## Returns a complex number with real part `re` and zero imaginary part.
  result = Complex[T](re: re, im: T(0))

func abs2*[T: SomeFloat and Arithmetic](z: Complex[T]): T =
  ## Returns the squared absolute value of `z`, i.e. `z.re^2 + z.im^2`.
  ## Cheaper than `abs` because it avoids the square root.
  result = z.re * z.re + z.im * z.im

func abs*[T: SomeFloat and HasHypot](z: Complex[T]): T =
  ## Returns the absolute value (modulus) of `z`.
  result = hypot(z.re, z.im)

func arg*[T: SomeFloat and HasArctan2](z: Complex[T]): T =
  ## Returns the argument (phase angle, in radians) of `z`.
  result = arctan2(z.im, z.re)

func conjugate*[T: SomeFloat and Arithmetic](z: Complex[T]): Complex[T] =
  ## Returns the complex conjugate of `z` (`z.im` negated).
  result = Complex[T](re: z.re, im: -z.im)

func `==`*[T: SomeFloat and Arithmetic](a, b: Complex[T]): bool =
  ## Compares two complex numbers for equality.
  result = a.re == b.re and a.im == b.im

func `+`*[T: SomeFloat and Arithmetic](a, b: Complex[T]): Complex[T] =
  result = Complex[T](re: a.re + b.re, im: a.im + b.im)

func `+`*[T: SomeFloat and Arithmetic](a: Complex[T]; b: T): Complex[T] =
  result = Complex[T](re: a.re + b, im: a.im)

func `+`*[T: SomeFloat and Arithmetic](a: T; b: Complex[T]): Complex[T] =
  result = Complex[T](re: a + b.re, im: b.im)

func `-`*[T: SomeFloat and Arithmetic](z: Complex[T]): Complex[T] =
  ## Unary minus.
  result = Complex[T](re: -z.re, im: -z.im)

func `-`*[T: SomeFloat and Arithmetic](a, b: Complex[T]): Complex[T] =
  result = Complex[T](re: a.re - b.re, im: a.im - b.im)

func `-`*[T: SomeFloat and Arithmetic](a: Complex[T]; b: T): Complex[T] =
  result = Complex[T](re: a.re - b, im: a.im)

func `-`*[T: SomeFloat and Arithmetic](a: T; b: Complex[T]): Complex[T] =
  result = Complex[T](re: a - b.re, im: -b.im)

func `*`*[T: SomeFloat and Arithmetic](a, b: Complex[T]): Complex[T] =
  result = Complex[T](re: a.re * b.re - a.im * b.im,
                      im: a.re * b.im + a.im * b.re)

func `*`*[T: SomeFloat and Arithmetic](a: Complex[T]; b: T): Complex[T] =
  result = Complex[T](re: a.re * b, im: a.im * b)

func `*`*[T: SomeFloat and Arithmetic](a: T; b: Complex[T]): Complex[T] =
  result = Complex[T](re: a * b.re, im: a * b.im)

func `/`*[T: SomeFloat and Arithmetic](a, b: Complex[T]): Complex[T] =
  let d = b.re * b.re + b.im * b.im
  result = Complex[T](re: (a.re * b.re + a.im * b.im) / d,
                      im: (a.im * b.re - a.re * b.im) / d)

func `/`*[T: SomeFloat and Arithmetic](a: Complex[T]; b: T): Complex[T] =
  result = Complex[T](re: a.re / b, im: a.im / b)

func inv*[T: SomeFloat and Arithmetic](z: Complex[T]): Complex[T] =
  ## Returns the multiplicative inverse `1 / z`.
  let d = z.re * z.re + z.im * z.im
  result = Complex[T](re: z.re / d, im: -z.im / d)

func sqrt*[T: SomeFloat and Arithmetic and HasHypot and HasSqrt](z: Complex[T]): Complex[T] =
  ## Returns the principal square root of `z`.
  if z.re == T(0) and z.im == T(0):
    result = Complex[T](re: T(0), im: T(0))
  else:
    let m = abs(z)
    var s = sqrt((m - z.re) * T(0.5))
    if z.im < T(0): s = -s
    result = Complex[T](re: sqrt((m + z.re) * T(0.5)), im: s)

func exp*[T: SomeFloat and Arithmetic and HasExp and HasSin and HasCos](
    z: Complex[T]): Complex[T] =
  ## Returns the exponential `e^z`.
  let r = exp(z.re)
  result = Complex[T](re: r * cos(z.im), im: r * sin(z.im))

func ln*[T: SomeFloat and HasHypot and HasLn and HasArctan2](
    z: Complex[T]): Complex[T] =
  ## Returns the principal natural logarithm of `z`.
  result = Complex[T](re: ln(abs(z)), im: arg(z))

func pow*[T: SomeFloat and Arithmetic and HasHypot and HasExp and HasLn and
          HasSin and HasCos and HasArctan2](z, w: Complex[T]): Complex[T] =
  ## Returns `z` raised to the power `w` (principal value).
  if z.re == T(0) and z.im == T(0):
    if w.re == T(0) and w.im == T(0):
      result = Complex[T](re: T(1), im: T(0))
    else:
      result = Complex[T](re: T(0), im: T(0))
  else:
    result = exp(w * ln(z))

proc `$`*[T: SomeFloat and Stringable](z: Complex[T]): string =
  ## Returns a textual representation of `z` as `"(re, im)"`.
  result = "(" & $z.re & ", " & $z.im & ")"

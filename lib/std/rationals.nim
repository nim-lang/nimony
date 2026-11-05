#
#
#            Nim's Runtime Library
#        (c) Copyright 2015 Dennis Felsing
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#


## This module implements rational numbers, consisting of a numerator and
## a denominator. The denominator can not be 0. The component type must be an
## integer type; unsigned components are supported. Floats are not supported;
## use `toFloat` to convert a rational to a float.
##
## The fields are public, so a valid denominator is not a type invariant. Every
## operation therefore states its denominator requirements as a contract. A
## violated contract is reported by Nimony as a panic; this port does not raise
## Nim's `DivByZeroDefect`.

runnableExamples:
  let
    r1 = 1 // 2
    r2 = -3 // 4

  doAssert r1 + r2 == -1 // 4
  doAssert r1 - r2 ==  5 // 4
  doAssert r1 * r2 == -3 // 8
  doAssert r1 / r2 == -2 // 3

import std/[math, hashes]

type
  Rational*[T: SomeInteger] = object
    ## A rational number, consisting of a numerator `num` and a denominator `den`.
    ## `T` must be an integer type.
    num*, den*: T

  RationalInt = SomeInteger and IntegerArithmetic
    ## The integer operations a rational's component type has to provide.

func rationalAbs[T: RationalInt](x: T): T {.inline.} =
  if x < T(0): T(0) - x else: x

func intPow[T: RationalInt](base, exponent: T): T =
  ## Integer exponentiation without `std/math.^`'s `Natural` exponent.
  result = T(1)
  var b = base
  var e = exponent
  while e > T(0):
    if e mod T(2) != T(0):
      result = result * b
    e = e div T(2)
    if e > T(0):
      b = b * b

func reduce*[T: RationalInt](x: var Rational[T]) {.requires: x.den != 0.} =
  ## Reduces the rational number `x`, so that the numerator and denominator
  ## have no common divisors other than 1 (and -1).
  ##
  ## The denominator of `x` must not be 0.
  ##
  ## **Note:** This is called automatically by the various operations on rationals.
  runnableExamples:
    var r = Rational[int](num: 2, den: 4) # 1/2
    reduce(r)
    doAssert r.num == 1
    doAssert r.den == 2
  let common = gcd(x.num, x.den)
  if x.den > T(0):
    x.num = x.num div common
    x.den = x.den div common
  elif x.den < T(0):
    x.num = (T(0) - x.num) div common
    x.den = (T(0) - x.den) div common

func initRational*[T: RationalInt](num, den: T): Rational[T] {.
    requires: den != 0.} =
  ## Creates a new rational number with numerator `num` and denominator `den`.
  ## `den` must not be 0.
  result = Rational[T](num: num, den: den)
  reduce(result)

func `//`*[T: RationalInt](num, den: T): Rational[T] {.
    requires: den != 0.} =
  ## A friendlier version of `initRational <#initRational,T,T>`_.
  runnableExamples:
    let x = 1 // 3 + 1 // 5
    doAssert x == 8 // 15

  initRational[T](num, den)

func `$`*[T: RationalInt and Stringable](x: Rational[T]): string =
  ## Turns a rational number into a string.
  runnableExamples:
    doAssert $(1 // 2) == "1/2"

  result = $x.num & "/" & $x.den

func toRational*[T: RationalInt](x: T): Rational[T] =
  ## Converts some integer `x` to a rational number.
  runnableExamples:
    doAssert toRational(42) == 42 // 1
  result = Rational[T](num: x, den: T(1))

func toRational*(x: float,
                 n: int = high(int) shr (sizeof(int) div 2 * 8)): Rational[int] {.
    requires: n > 0.} =
  ## Calculates the best rational approximation of `x`,
  ## where the denominator is smaller than `n`
  ## (default is the largest possible `int` for maximal resolution).
  ## `n` must be positive.
  ##
  ## The algorithm is based on the theory of continued fractions.
  # David Eppstein / UC Irvine / 8 Aug 1993
  # With corrections from Arno Formella, May 2008
  runnableExamples:
    let x = 1.2
    doAssert x.toRational.toFloat == x

  var
    m11, m22 = 1
    m12, m21 = 0
    ai = int(x)
    x = x
  while m21 * ai + m22 <= n:
    swap m12, m11
    swap m22, m21
    m11 = m12 * ai + m11
    m21 = m22 * ai + m21
    if x == float(ai): break # division by zero
    x = 1 / (x - float(ai))
    if x > float(high(int32)): break # representation failure
    ai = int(x)
  result = initRational(m11, m21)

func toFloat*[T: RationalInt](x: Rational[T]): float {.requires: x.den != 0.} =
  ## Converts a rational number `x` to a `float`.
  float(x.num) / float(x.den)

func toInt*[T: RationalInt](x: Rational[T]): int {.requires: x.den != 0.} =
  ## Converts a rational number `x` to an `int`. Conversion rounds towards 0 if
  ## `x` does not contain an integer value.
  int(x.num div x.den)

func `+`*[T: RationalInt](x, y: Rational[T]): Rational[T] {.
    requires: x.den != 0 and y.den != 0.} =
  ## Adds two rational numbers.
  let common = lcm(x.den, y.den)
  result = Rational[T](num: common div x.den * x.num + common div y.den * y.num,
                       den: common
  )
  reduce(result)

func `+`*[T: RationalInt](x: Rational[T], y: T): Rational[T] {.
    requires: x.den != 0.} =
  ## Adds the rational `x` to the int `y`.
  result = Rational[T](num: x.num + y * x.den, den: x.den)

func `+`*[T: RationalInt](x: T, y: Rational[T]): Rational[T] {.
    requires: y.den != 0.} =
  ## Adds the int `x` to the rational `y`.
  result = Rational[T](num: x * y.den + y.num, den: y.den)

func `+=`*[T: RationalInt](x: var Rational[T], y: Rational[T]) {.
    requires: x.den != 0 and y.den != 0.} =
  ## Adds the rational `y` to the rational `x` in-place.
  let common = lcm(x.den, y.den)
  x.num = common div x.den * x.num + common div y.den * y.num
  x.den = common
  reduce(x)

func `+=`*[T: RationalInt](x: var Rational[T], y: T) {.
    requires: x.den != 0.} =
  ## Adds the int `y` to the rational `x` in-place.
  x.num += y * x.den

func `-`*[T: RationalInt](x: Rational[T]): Rational[T] {.
    requires: x.den != 0.} =
  ## Unary minus for rational numbers.
  result = Rational[T](num: T(0) - x.num, den: x.den)

func `-`*[T: RationalInt](x, y: Rational[T]): Rational[T] {.
    requires: x.den != 0 and y.den != 0.} =
  ## Subtracts two rational numbers.
  let common = lcm(x.den, y.den)
  result = Rational[T](num: common div x.den * x.num - common div y.den * y.num,
                den: common
  )
  reduce(result)

func `-`*[T: RationalInt](x: Rational[T], y: T): Rational[T] {.
    requires: x.den != 0.} =
  ## Subtracts the int `y` from the rational `x`.
  result = Rational[T](num: x.num - y * x.den, den: x.den)

func `-`*[T: RationalInt](x: T, y: Rational[T]): Rational[T] {.
    requires: y.den != 0.} =
  ## Subtracts the rational `y` from the int `x`.
  result = Rational[T](num: x * y.den - y.num, den: y.den)

func `-=`*[T: RationalInt](x: var Rational[T], y: Rational[T]) {.
    requires: x.den != 0 and y.den != 0.} =
  ## Subtracts the rational `y` from the rational `x` in-place.
  let common = lcm(x.den, y.den)
  x.num = common div x.den * x.num - common div y.den * y.num
  x.den = common
  reduce(x)

func `-=`*[T: RationalInt](x: var Rational[T], y: T) {.
    requires: x.den != 0.} =
  ## Subtracts the int `y` from the rational `x` in-place.
  x.num -= y * x.den

func `*`*[T: RationalInt](x, y: Rational[T]): Rational[T] {.
    requires: x.den != 0 and y.den != 0.} =
  ## Multiplies two rational numbers.
  result = Rational[T](num: x.num * y.num, den: x.den * y.den)
  reduce(result)

func `*`*[T: RationalInt](x: Rational[T], y: T): Rational[T] {.
    requires: x.den != 0.} =
  ## Multiplies the rational `x` with the int `y`.
  result = Rational[T](num: x.num * y, den: x.den)
  reduce(result)

func `*`*[T: RationalInt](x: T, y: Rational[T]): Rational[T] {.
    requires: y.den != 0.} =
  ## Multiplies the int `x` with the rational `y`.
  result = Rational[T](num: x * y.num, den: y.den)
  reduce(result)

func `*=`*[T: RationalInt](x: var Rational[T], y: Rational[T]) {.
    requires: x.den != 0 and y.den != 0.} =
  ## Multiplies the rational `x` by `y` in-place.
  x.num *= y.num
  x.den *= y.den
  reduce(x)

func `*=`*[T: RationalInt](x: var Rational[T], y: T) {.
    requires: x.den != 0.} =
  ## Multiplies the rational `x` by the int `y` in-place.
  x.num *= y
  reduce(x)

func reciprocal*[T: RationalInt](x: Rational[T]): Rational[T] {.
    requires: x.den != 0 and x.num != 0.} =
  ## Calculates the reciprocal of `x` (`1/x`).
  ## The numerator of `x` must not be 0.
  if x.num > T(0):
    result = Rational[T](num: x.den, den: x.num)
  else:
    result = Rational[T](num: T(0) - x.den, den: T(0) - x.num)

func `/`*[T: RationalInt](x, y: Rational[T]): Rational[T] {.
    requires: x.den != 0 and y.den != 0 and y.num != 0.} =
  ## Divides the rational `x` by the rational `y`.
  result = Rational[T](num: x.num * y.den, den: x.den * y.num)
  reduce(result)

func `/`*[T: RationalInt](x: Rational[T], y: T): Rational[T] {.
    requires: x.den != 0 and y != 0.} =
  ## Divides the rational `x` by the int `y`.
  result = Rational[T](num: x.num, den: x.den * y)
  reduce(result)

func `/`*[T: RationalInt](x: T, y: Rational[T]): Rational[T] {.
    requires: y.den != 0 and y.num != 0.} =
  ## Divides the int `x` by the rational `y`.
  result = Rational[T](num: x * y.den, den: y.num)
  reduce(result)

func `/=`*[T: RationalInt](x: var Rational[T], y: Rational[T]) {.
    requires: x.den != 0 and y.den != 0 and y.num != 0.} =
  ## Divides the rational `x` by the rational `y` in-place.
  x.num *= y.den
  x.den *= y.num
  reduce(x)

func `/=`*[T: RationalInt](x: var Rational[T], y: T) {.
    requires: x.den != 0 and y != 0.} =
  ## Divides the rational `x` by the int `y` in-place.
  x.den *= y
  reduce(x)

func cmp*[T: RationalInt](x, y: Rational[T]): int {.
    requires: x.den != 0 and y.den != 0.} =
  ## Compares two rationals. Returns
  ## * a value less than zero, if `x < y`
  ## * a value greater than zero, if `x > y`
  ## * zero, if `x == y`
  int((x - y).num)

func `<`*[T: RationalInt](x, y: Rational[T]): bool {.
    requires: x.den != 0 and y.den != 0.} =
  ## Returns true if `x` is less than `y`.
  (x - y).num < T(0)

func `<=`*[T: RationalInt](x, y: Rational[T]): bool {.
    requires: x.den != 0 and y.den != 0.} =
  ## Returns true if `x` is less than or equal to `y`.
  (x - y).num <= T(0)

func `==`*[T: RationalInt](x, y: Rational[T]): bool {.
    requires: x.den != 0 and y.den != 0.} =
  ## Compares two rationals for equality.
  (x - y).num == T(0)

func abs*[T: RationalInt](x: Rational[T]): Rational[T] {.
    requires: x.den != 0.} =
  ## Returns the absolute value of `x`.
  runnableExamples:
    doAssert abs(1 // 2) == 1 // 2
    doAssert abs(-1 // 2) == 1 // 2
  result = Rational[T](num: rationalAbs(x.num), den: rationalAbs(x.den))

func `div`*[T: RationalInt](x, y: Rational[T]): T {.
    requires: x.den != 0 and y.den != 0 and y.num != 0.} =
  ## Computes the rational truncated division.
  (x.num * y.den) div (y.num * x.den)

func `mod`*[T: RationalInt](x, y: Rational[T]): Rational[T] {.
    requires: x.den != 0 and y.den != 0 and y.num != 0.} =
  ## Computes the rational modulo by truncated division (remainder).
  ## This is same as `x - (x div y) * y`.
  let numerator = (x.num * y.den) mod (y.num * x.den)
  result = initRational(numerator, x.den * y.den)

func floorDiv*[T: RationalInt](x, y: Rational[T]): T {.
    requires: x.den != 0 and y.den != 0 and y.num != 0.} =
  ## Computes the rational floor division.
  ##
  ## Floor division is conceptually defined as `floor(x / y)`.
  ## This is different from the `div` operator, which is defined
  ## as `trunc(x / y)`. That is, `div` rounds towards 0 and `floorDiv`
  ## rounds down.
  floorDiv(x.num * y.den, y.num * x.den)

func floorMod*[T: RationalInt](x, y: Rational[T]): Rational[T] {.
    requires: x.den != 0 and y.den != 0 and y.num != 0.} =
  ## Computes the rational modulo by floor division (modulo).
  ##
  ## This is same as `x - floorDiv(x, y) * y`.
  ## This func behaves the same as the `%` operator in Python.
  let numerator = floorMod(x.num * y.den, y.num * x.den)
  result = initRational(numerator, x.den * y.den)

func hash*[T: RationalInt and Hashable](x: Rational[T]): Hash {.
    requires: x.den != 0.} =
  ## Computes the hash for the rational `x`.
  # reduce first so that hash(x) == hash(y) for x == y
  var copy = x
  reduce(copy)

  var h: Hash = 0
  h = h !& hash(copy.num)
  h = h !& hash(copy.den)
  result = !$h

func `^`*[T: RationalInt](x: Rational[T], y: T): Rational[T] {.
    requires: x.den != 0 and (y >= T(0) or x.num != 0).} =
  ## Computes `x` to the power of `y`.
  ##
  ## The exponent `y` must be an integer. Negative exponents are supported
  ## but floating point exponents are not. For a negative exponent the
  ## numerator of `x` must not be 0.
  runnableExamples:
    doAssert (-3 // 5) ^ 0 == (1 // 1)
    doAssert (-3 // 5) ^ 1 == (-3 // 5)
    doAssert (-3 // 5) ^ 2 == (9 // 25)
    doAssert (-3 // 5) ^ -2 == (25 // 9)

  if y >= T(0):
    result = Rational[T](num: intPow(x.num, y), den: intPow(x.den, y))
  else:
    let exponent = T(0) - y
    result = Rational[T](num: intPow(x.den, exponent),
                         den: intPow(x.num, exponent))
  # Note that all powers of reduced rationals are already reduced,
  # so we don't need to call reduce() here

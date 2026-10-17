## Concepts, constants and freestanding pure-Nim helpers shared by every
## `std/math` backend. The general math utilities are adapted from Nim's
## Runtime Library `lib/pure/math.nim`; the concepts and backend-independent
## organization are Nimony-specific.
## Source: https://github.com/nim-lang/Nim/blob/devel/lib/pure/math.nim
## Copyright (c) 2015 Andreas Rumpf. MIT license, as in Nim's `copying.txt`.
## Permission is hereby granted, free of charge, to any person obtaining a copy
## of this software and associated documentation files (the "Software"), to deal
## in the Software without restriction, including without limitation the rights
## to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
## copies of the Software, and to permit persons to whom the Software is
## furnished to do so, subject to the following conditions:
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
## AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
## OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
## THE SOFTWARE.
## Nothing here depends on `cmath` or the per-category primitive modules
## (`sin`, `exp`, ...), so this module remains importable with or without
## `nimNativeIo`.

type
  Arithmetic* = concept ## Types that supply the arithmetic and comparison
                        ## operators signed, unsigned and float types have in common.
    func `+`(x, y: Self): Self
    func `-`(x, y: Self): Self
    func `*`(x, y: Self): Self
    func `mod`(x, y: Self): Self
    func `==`(x, y: Self): bool
    func `<`(x, y: Self): bool
    func `<=`(x, y: Self): bool

  IntegerArithmetic* = concept of Arithmetic ## `Arithmetic` plus the integer-only operations.
    func `div`(x, y: Self): Self
    func inc(x: var Self, y: Self)
    func dec(x: var Self, y: Self)

  SignedArithmetic* = concept of Arithmetic ## `Arithmetic` plus negation: signed integers and floats.
    func `-`(x: Self): Self

  FloatArithmetic* = concept of SignedArithmetic ## `SignedArithmetic` plus real division.
    func `/`(x, y: Self): Self

  # Concepts for the individual transcendental functions below. They let generic
  # code (e.g. `std/complex`) depend precisely on the operations it actually uses,
  # rather than on a concrete float type.
  HasSqrt* = concept ## Types that provide `sqrt`.
    func sqrt(x: Self): Self
  HasExp* = concept ## Types that provide `exp`.
    func exp(x: Self): Self
  HasLn* = concept ## Types that provide `ln`.
    func ln(x: Self): Self
  HasSin* = concept ## Types that provide `sin`.
    func sin(x: Self): Self
  HasCos* = concept ## Types that provide `cos`.
    func cos(x: Self): Self
  HasHypot* = concept ## Types that provide `hypot`.
    func hypot(x, y: Self): Self
  HasArctan2* = concept ## Types that provide `arctan2`.
    func arctan2(y, x: Self): Self

  FloatClass* = enum ## Describes the class a floating point value belongs to.
                     ## This is the type that is returned by the
                     ## `classify func <#classify,float>`_.
    fcNormal,        ## value is an ordinary nonzero floating point value
    fcSubnormal,     ## value is a subnormal (a very small) floating point value
    fcZero,          ## value is zero
    fcNegZero,       ## value is the negative zero
    fcNan,           ## value is Not a Number (NaN)
    fcInf,           ## value is positive infinity
    fcNegInf         ## value is negative infinity

const
  PI* = 3.1415926535897932384626433          ## The circle constant PI (Ludolph's number).
  TAU* = 2.0 * PI                            ## The circle constant TAU (= 2 * PI).
  E* = 2.71828182845904523536028747          ## Euler's number.

  MaxFloat64Precision* = 16                  ## Maximum number of meaningful digits
                                             ## after the decimal point for Nim's
                                             ## `float64` type.
  MaxFloat32Precision* = 8                   ## Maximum number of meaningful digits
                                             ## after the decimal point for Nim's
                                             ## `float32` type.
  MaxFloatPrecision* = MaxFloat64Precision   ## Maximum number of
                                             ## meaningful digits
                                             ## after the decimal point
                                             ## for Nim's `float` type.
  MinFloatNormal* = 2.225073858507201e-308   ## Smallest normal number for Nim's
                                             ## `float` type (= 2^-1022).

  RadPerDeg = PI / 180.0  ## Number of radians per degree.

func sgn*[T: SomeNumber and Comparable](x: T): int {.inline.} =
  ## Sign function.
  ##
  ## Returns:
  ## * `-1` for negative numbers and `NegInf`,
  ## * `1` for positive numbers and `Inf`,
  ## * `0` for positive zero, negative zero and `NaN`
  runnableExamples:
    assert sgn(5) == 1
    assert sgn(0) == 0
    assert sgn(-4.1) == -1

  ord(T(0) < x) - ord(x < T(0))

func floorMod*[T: SomeNumber and Arithmetic](x, y: T): T {.inline.} =
  ## Floor modulo is conceptually defined as `x - (floorDiv(x, y) * y)`.
  ##
  ## This func behaves the same as the `%` operator in Python.
  ##
  ## **See also:**
  ## * `mod func <#mod,float64,float64>`_
  ## * `floorDiv func <#floorDiv,T,T>`_
  runnableExamples:
    assert floorMod( 13,  3) ==  1
    assert floorMod(-13,  3) ==  2
    assert floorMod( 13, -3) == -2
    assert floorMod(-13, -3) == -1

  result = x mod y
  if (result > T(0) and y < T(0)) or (result < T(0) and y > T(0)):
    result = result + y

func floorDiv*[T: SomeInteger and IntegerArithmetic](x, y: T): T {.inline.} =
  ## Floor division is conceptually defined as `floor(x / y)`.
  ##
  ## This is different from the `system.div <system.html#div,int,int>`_
  ## operator, which is defined as `trunc(x / y)`.
  ## That is, `div` rounds towards `0` and `floorDiv` rounds down.
  ##
  ## **See also:**
  ## * `system.div func <system.html#div,int,int>`_ for integer division
  ## * `floorMod func <#floorMod,T,T>`_ for Python-like (`%` operator) behavior
  runnableExamples:
    assert floorDiv( 13,  3) ==  4
    assert floorDiv(-13,  3) == -5
    assert floorDiv( 13, -3) == -5
    assert floorDiv(-13, -3) ==  4

  result = x div y
  let r = x mod y
  if (r > T(0) and y < T(0)) or (r < T(0) and y > T(0)):
    result = result - T(1)

func euclDiv*[T: SomeInteger and IntegerArithmetic](x, y: T): T {.inline.}=
  ## Returns euclidean division of `x` by `y`.
  runnableExamples:
    assert euclDiv(13, 3) == 4
    assert euclDiv(-13, 3) == -5
    assert euclDiv(13, -3) == -4
    assert euclDiv(-13, -3) == 5

  result = x div y
  if x mod y < 0:
    if y > T(0):
      dec result
    else:
      inc result

func euclMod*[T: SomeNumber and SignedArithmetic](x, y: T): T {.inline.} =
  ## Returns euclidean modulo of `x` by `y`.
  ## `euclMod(x, y)` is non-negative.
  runnableExamples:
    assert euclMod(13, 3) == 1
    assert euclMod(-13, 3) == 2
    assert euclMod(13, -3) == 1
    assert euclMod(-13, -3) == 2

  result = x mod y
  if result < 0:
    result = result + abs(y)

template ceilDivUint[T: SomeUnsignedInt and IntegerArithmetic](x, y: T): T =
  # If the divisor is const, the backend C/C++ compiler generates code without a `div`
  # instruction, as it is slow on most CPUs.
  # If the divisor is a power of 2 and a const unsigned integer type, the
  # compiler generates faster code.
  # If the divisor is const and a signed integer, generated code becomes slower
  # than the code with unsigned integers, because division with signed integers
  # need to works for both positive and negative value without `idiv`/`sdiv`.
  # That is why this code convert parameters to unsigned.
  # This post contains a comparison of the performance of signed/unsigned integers:
  # https://github.com/nim-lang/Nim/pull/18596#issuecomment-894420984.
  # If signed integer arguments were not converted to unsigned integers,
  # `ceilDiv` wouldn't work for any positive signed integer value, because
  # `x + (y - 1)` can overflow.
  (x + (y - T(1))) div y

template ceilDivSigned[T: SomeInteger](x, y: T; U: untyped): T {.untyped.} =
  T(ceilDivUint(x.U, y.U))

func ceilDiv*[T: SomeInteger and IntegerArithmetic](x, y: T): T {.inline, raises.} =
  ## Ceil division is conceptually defined as `ceil(x / y)`.
  ##
  ## Assumes `x >= 0` and `y > 0` (and `x + y - 1 <= high(T)` if T is SomeUnsignedInt).
  ##
  ## This is different from the `system.div <system.html#div,int,int>`_
  ## operator, which works like `trunc(x / y)`.
  ## That is, `div` rounds towards `0` and `ceilDiv` rounds up.
  ##
  ## This function has the above input limitation, because that allows the
  ## compiler to generate faster code and it is rarely used with
  ## negative values or unsigned integers close to `high(T)/2`.
  ## If you need a `ceilDiv` that works with any input, see:
  ## https://github.com/demotomohiro/divmath.
  ##
  ## **See also:**
  ## * `system.div func <system.html#div,int,int>`_ for integer division
  ## * `floorDiv func <#floorDiv,T,T>`_ for integer division which rounds down.
  runnableExamples:
    assert ceilDiv(12, 3) ==  4
    assert ceilDiv(13, 3) ==  5

  if x >= T(0) and y > T(0):
    discard
  else:
    raise RangeError
  when T is SomeUnsignedInt:
    if x + y - T(1) >= x:
      discard
    else:
      raise RangeError

  result =  when T is int:
              ceilDivSigned(x, y, uint)
            elif T is int64:
              ceilDivSigned(x, y, uint64)
            elif T is int32:
              ceilDivSigned(x, y, uint32)
            elif T is int16:
              ceilDivSigned(x, y, uint16)
            elif T is int8:
              ceilDivSigned(x, y, uint8)
            else:
              ceilDivUint(x, y)

func divmod*[T: SomeInteger and IntegerArithmetic](x, y: T): (T, T) {.inline.} =
  ## Computes both division and modulus.
  ## Return structure is: (quotient, remainder)
  runnableExamples:
    assert divmod(5, 2) == (2, 1)
    assert divmod(5, -3) == (-1, 2)

  # It seems there is no reasons to use `div` in stdlib.h like Nim 2.
  # https://stackoverflow.com/questions/4565272/why-use-div-or-ldiv-in-c-c
  # See Notes on:
  # https://en.cppreference.com/w/c/numeric/math/div.html
  (x div y, x mod y)

func sum*[T: HasDefault and Arithmetic](x: openArray[T]): T =
  ## Computes the sum of the elements in `x`.
  ##
  ## If `x` is empty, 0 is returned.
  ##
  ## **See also:**
  ## * `prod func <#prod,openArray[T]>`_
  runnableExamples:
    assert sum([1, 2, 3, 4]) == 10
    assert sum([-4, 3, 5]) == 4
  result = default(T)
  for i in items(x): result = result + i

func cumsum*[T: Arithmetic](x: var openArray[T]) =
  ## Transforms `x` in-place (must be declared as `var`) into its
  ## cumulative (aka prefix) summation.
  ##
  ## **See also:**
  ## * `sum func <#sum,openArray[T]>`_
  ## * `cumsummed func <#cumsummed,openArray[T]>`_ for a version which
  ##   returns a cumsummed sequence
  runnableExamples:
    var a = [1, 2, 3, 4]
    cumsum(a)
    assert a == @[1, 3, 6, 10]

  for i in 1 ..< x.len: x[i] = x[i - 1] + x[i]

func prod*[T: HasDefault and Arithmetic](x: openArray[T]): T =
  ## Computes the product of the elements in `x`.
  ##
  ## If `x` is empty, 1 is returned.
  ##
  ## **See also:**
  ## * `sum func <#sum,openArray[T]>`_
  ## * `fac func <#fac,int>`_
  runnableExamples:
    assert prod([1, 2, 3, 4]) == 24
    assert prod([-4, 3, 5]) == -60

  result = T(1)
  for i in items(x): result = result * i

func cumprod*[T: Arithmetic](x: var openArray[T]) =
  ## Transforms ``x`` in-place (must be declared as `var`) into its
  ## product.
  ##
  ## See also:
  ## * `prod func <#sum,openArray[T]>`_
  ## * `cumproded func <#cumproded,openArray[T]>`_ for a version which
  ##   returns cumproded sequence
  runnableExamples:
    var a = [1, 2, 3, 4]
    cumprod(a)
    assert a == @[1, 2, 6, 24]
  for i in 1 ..< x.len: x[i] = x[i-1] * x[i]

func isPowerOfTwo*(x: int): bool =
  ## Returns `true`, if `x` is a power of two, `false` otherwise.
  ##
  ## Zero and negative numbers are not a power of two.
  ##
  ## **See also:**
  ## * `nextPowerOfTwo func <#nextPowerOfTwo,int>`_
  runnableExamples:
    assert isPowerOfTwo(16)
    assert not isPowerOfTwo(5)
    assert not isPowerOfTwo(0)
    assert not isPowerOfTwo(-16)

  return (x > 0) and ((x and (x - 1)) == 0)

func nextPowerOfTwo*(x: int): int =
  ## Returns `x` rounded up to the nearest power of two.
  ##
  ## Zero and negative numbers get rounded up to 1.
  ##
  ## **See also:**
  ## * `isPowerOfTwo func <#isPowerOfTwo,int>`_
  runnableExamples:
    assert nextPowerOfTwo(16) == 16
    assert nextPowerOfTwo(5) == 8
    assert nextPowerOfTwo(0) == 1
    assert nextPowerOfTwo(-16) == 1

  result = x - 1
  when defined(cpu64):
    result = result or (result shr 32)
  when defined(cpu64) or defined(cpu32):
    result = result or (result shr 16)
  when defined(cpu64) or defined(cpu32) or defined(cpu16):
    result = result or (result shr 8)
  result = result or (result shr 4)
  result = result or (result shr 2)
  result = result or (result shr 1)
  result += 1 + ord(x <= 0)

func degToRad*[T: SomeFloat and FloatArithmetic](d: T): T {.inline.} =
  ## Converts from degrees to radians.
  ##
  ## **See also:**
  ## * `radToDeg func <#radToDeg,T>`_
  runnableExamples:
    assert almostEqual(degToRad(180.0), PI)

  result = d * T(RadPerDeg)

func radToDeg*[T: SomeFloat and FloatArithmetic](r: T): T {.inline.} =
  ## Converts from radians to degrees.
  ##
  ## **See also:**
  ## * `degToRad func <#degToRad,T>`_
  runnableExamples:
    assert almostEqual(radToDeg(2 * PI), 360.0)

  result = r / T(RadPerDeg)

func fac*(n: int): int =
  ## Computes the [factorial](https://en.wikipedia.org/wiki/Factorial) of
  ## a non-negative integer `n`.
  ##
  ## **See also:**
  ## * `prod func <#prod,openArray[T]>`_
  runnableExamples:
    assert fac(0) == 1
    assert fac(4) == 24
    assert fac(10) == 3628800

  #assert n >= 0, "argument of fac must not be negative"

  result = 1
  for i in 1 .. n:
    result *= i

func binom*(n, k: Natural): int =
  ## Computes the [binomial coefficient](https://en.wikipedia.org/wiki/Binomial_coefficient).
  runnableExamples:
    assert binom(6, 2) == 15
    assert binom(6, 0) == 1

  if k <= 0: return 1
  if 2 * k > n: return binom(n, n - k)
  result = n
  for i in 2 .. k:
    result = (result * (n + 1 - i)) div i

func gcd*[T: SignedArithmetic](x, y: T): T =
  ## Computes the greatest common (positive) divisor of `x` and `y`.
  ##
  ## Note that for floats, the result cannot always be interpreted as
  ## "greatest decimal `z` such that `z*N == x and z*M == y`
  ## where N and M are positive integers".
  ##
  ## **See also:**
  ## * `gcd func <#gcd,SomeInteger,SomeInteger>`_ for an integer version
  ## * `lcm func <#lcm,T,T>`_
  runnableExamples:
    assert gcd(13.5, 9.0) == 4.5

  var (x, y) = (x, y)
  while y != T(0):
    x = x mod y
    swap x, y
  abs x

func gcd*[T: SignedArithmetic](x: openArray[T]): T =
  ## Computes the greatest common (positive) divisor of the elements of `x`.
  ##
  ## **See also:**
  ## * `gcd func <#gcd,T,T>`_ for a version with two arguments
  runnableExamples:
    assert gcd(@[13.5, 9.0]) == 4.5

  result = x[0]
  for i in 1 ..< x.len:
    result = gcd(result, x[i])

func lcm*[T: IntegerArithmetic and SignedArithmetic](x, y: T): T {.inline.} =
  ## Computes the least common multiple of `x` and `y`.
  ##
  ## **See also:**
  ## * `gcd func <#gcd,T,T>`_
  runnableExamples:
    assert lcm(24, 30) == 120
    assert lcm(13, 39) == 39

  x div gcd(x, y) * y

func lcm*[T: IntegerArithmetic and SignedArithmetic](x: openArray[T]): T =
  ## Computes the least common multiple of the elements of `x`.
  ##
  ## **See also:**
  ## * `lcm func <#lcm,T,T>`_ for a version with two arguments
  runnableExamples:
    assert lcm(@[24, 30]) == 120

  result = x[0]
  for i in 1 ..< x.len:
    result = lcm(result, x[i])

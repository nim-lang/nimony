import std/[assertions, fenv]

type
  Arithmetic = concept
    proc `+`(x, y: Self): Self
    proc `-`(x, y: Self): Self
    proc inc(x: var Self, y: Self)
    proc dec(x: var Self, y: Self)
    proc `*`(x, y: Self): Self
    proc `div`(x, y: Self): Self
    proc `mod`(x, y: Self): Self
    proc `<`(x, y: Self): bool
    proc `>`(x, y: Self): bool

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

{.push header: "<math.h>".}
# These are C macros and can take both float and double type values.
proc c_signbit[T: SomeFloat](x: T): int {.importc: "signbit".}
proc c_copysign[T: SomeFloat](x, y: T): T {.importc: "copysign".}
proc c_fpclassify[T: SomeFloat](x: T): int {.importc: "fpclassify".}
proc c_isnan[T: SomeFloat](x: T): int {.importc: "isnan".}
func c_frexp[T: SomeFloat](x: T; exponent: ptr cint): T {.importc: "frexp".}
{.pop.}

# use push pragma when it is supported
let
  c_fpNormal    {.importc: "FP_NORMAL", header: "<math.h>".}: int
  c_fpSubnormal {.importc: "FP_SUBNORMAL", header: "<math.h>".}: int
  c_fpZero      {.importc: "FP_ZERO", header: "<math.h>".}: int
  c_fpInfinite  {.importc: "FP_INFINITE", header: "<math.h>".}: int
  c_fpNan       {.importc: "FP_NAN", header: "<math.h>".}: int

type
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

func signbit*[T: SomeFloat](x: T): bool {.inline.} =
  ## Returns true if `x` is negative, false otherwise.
  runnableExamples:
    assert not signbit(0.0)
    #assert signbit(0.0 * -1.0)
    assert signbit(-0.1)
    assert not signbit(0.1)

  c_signbit(x) != 0

func copySign*[T: SomeFloat](x, y: T): T {.inline.} =
  ## Returns a value with the magnitude of `x` and the sign of `y`;
  ## this works even if x or y are NaN, infinity or zero, all of which can carry a sign.
  runnableExamples:
    assert copySign(10.0, 1.0) == 10.0
    assert copySign(10.0, -1.0) == -10.0
    assert copySign(-Inf, -0.0) == -Inf
    assert copySign(NaN, 1.0).isNaN
    assert copySign(1.0, copySign(NaN, -1.0)) == -1.0
  c_copysign(x, y)

func classify*[T: SomeFloat](x: T): FloatClass {.inline.} =
  ## Classifies a floating point value.
  ##
  ## Returns `x`'s class as specified by the `FloatClass enum<#FloatClass>`_.
  runnableExamples:
    assert classify(0.3) == fcNormal
    assert classify(0.0) == fcZero
    assert classify(0.3 / 0.0) == fcInf
    assert classify(-0.3 / 0.0) == fcNegInf

  let r = c_fpclassify(x)
  if r == c_fpNormal:
    result = fcNormal
  elif r == c_fpSubnormal:
    result = fcSubnormal
  elif r == c_fpZero:
    result = if signbit(x): fcNegZero else: fcZero
  elif r == c_fpNan:
    result = fcNan
  elif r == c_fpInfinite:
    result = if signbit(x): fcNegInf else: fcInf
  else:
    # can be implementation-defined type
    result = fcNan

func isNaN*[T: SomeFloat](x: T): bool {.inline.} =
  ## Returns whether `x` is a `NaN`, more efficiently than via `classify(x) == fcNan`.
  runnableExamples:
    assert NaN.isNaN
    assert not Inf.isNaN
    assert not isNaN(3.1415926)

  c_isnan(x) != 0

func almostEqual*[T: SomeFloat](x, y: T; unitsInLastPlace: int = 4): bool {.
    untyped.} =
  ## Checks if two float values are almost equal, using the
  ## [machine epsilon](https://en.wikipedia.org/wiki/Machine_epsilon).
  ##
  ## `unitsInLastPlace` is the max number of
  ## [units in the last place](https://en.wikipedia.org/wiki/Unit_in_the_last_place)
  ## difference tolerated when comparing two numbers. The larger the value, the
  ## more error is allowed. A `0` value means that two numbers must be exactly the
  ## same to be considered equal.
  ##
  ## The machine epsilon has to be scaled to the magnitude of the values used
  ## and multiplied by the desired precision in ULPs unless the difference is
  ## subnormal.
  ##
  # taken from: https://en.cppreference.com/w/cpp/types/numeric_limits/epsilon
  runnableExamples:
    assert almostEqual(PI, 3.14159265358979)
    assert almostEqual(Inf, Inf)
    assert not almostEqual(NaN, NaN)

  if x == y:
    # short circuit exact equality -- needed to catch two infinities of
    # the same sign. And perhaps speeds things up a bit sometimes.
    return true
  let diff = abs(x - y)
  return diff <= epsilon(T) * abs(x + y) * T(unitsInLastPlace) or diff < minimumPositiveValue(T)

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

func frexp*[T: SomeFloat](x: T): tuple[frac: T, exp: int] {.inline.} =
  ## Splits `x` into a normalized fraction `frac` and an integral power of 2 `exp`,
  ## such that `abs(frac) in 0.5..<1` and `x == frac * 2 ^ exp`, except for special
  ## cases shown below.
  runnableExamples:
    assert frexp(8.0) == (0.5, 4)
    assert frexp(-8.0) == (-0.5, 4)
    assert frexp(0.0) == (0.0, 0)

    # special cases:
    assert frexp(-0.0).frac.signbit # signbit preserved for +-0
    assert frexp(Inf).frac == Inf # +- Inf preserved
    assert frexp(NaN).frac.isNaN

  var exp = cint(0)
  let frac = c_frexp(x, addr exp)
  result = (frac: frac, exp: exp.int)

{.push header: "<math.h>".}
func floor*[T: SomeFloat](x: T): T {.importc: "floor".} =
  ## Computes the floor function (i.e. the largest integer not greater than `x`).
  ##
  ## **See also:**
  ## * `ceil func <#ceil,float64>`_
  ## * `round func <#round,float64>`_
  ## * `trunc func <#trunc,float64>`_
  runnableExamples:
    assert floor(2.1)  == 2.0
    assert floor(2.9)  == 2.0
    assert floor(-3.5) == -4.0

func ceil*[T: SomeFloat](x: T): T {.importc: "ceil".} =
  ## Computes the ceiling function (i.e. the smallest integer not smaller
  ## than `x`).
  ##
  ## **See also:**
  ## * `floor func <#floor,float64>`_
  ## * `round func <#round,float64>`_
  ## * `trunc func <#trunc,float64>`_
  runnableExamples:
    assert ceil(2.1)  == 3.0
    assert ceil(2.9)  == 3.0
    assert ceil(-2.1) == -2.0

func round*[T: SomeFloat](x: T): T {.importc: "round".} =
  ## Returns the nearest integer value to `x`, rounding halfway cases away from zero.
  ##
  ## **See also:**
  ## * `floor func <#floor,float64>`_
  ## * `ceil func <#ceil,float64>`_
  ## * `trunc func <#trunc,float64>`_
  runnableExamples:
    assert round(3.4) == 3.0
    assert round(3.5) == 4.0
    assert round(4.5) == 5.0

func trunc*[T: SomeFloat](x: T): T {.importc: "trunc".} =
  ## Returns the nearest integer not greater in magnitude than `x`.
  ##
  ## **See also:**
  ## * `floor func <#floor,float64>`_
  ## * `ceil func <#ceil,float64>`_
  ## * `round func <#round,float64>`_
  runnableExamples:
    assert trunc(PI) == 3.0
    assert trunc(-1.85) == -1.0

func `mod`*[T: SomeFloat](x, y: T): T {.importc: "fmod".} =
  ## Computes the modulo operation for float values (the remainder of `x` divided by `y`).
  ##
  ## **See also:**
  ## * `floorMod func <#floorMod,T,T>`_ for Python-like (`%` operator) behavior
  runnableExamples:
    assert  6.5 mod  2.5 ==  1.5
    assert -6.5 mod  2.5 == -1.5
    assert  6.5 mod -2.5 ==  1.5
    assert -6.5 mod -2.5 == -1.5
{.pop.}

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

func floorDiv*[T: SomeInteger and Arithmetic](x, y: T): T {.inline.} =
  ## Floor division is conceptually defined as `floor(x / y)`.
  ##
  ## This is different from the `system.div <system.html#div,int,int>`_
  ## operator, which is defined as `trunc(x / y)`.
  ## That is, `div` rounds towards `0` and `floorDiv` rounds down.
  ##
  ## **See also:**
  ## * `system.div proc <system.html#div,int,int>`_ for integer division
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

func euclDiv*[T: SomeInteger and Arithmetic](x, y: T): T {.inline.}=
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

func euclMod*[T: SomeNumber and Arithmetic](x, y: T): T {.inline.} =
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

template ceilDivUint[T: SomeUnsignedInt and Arithmetic](x, y: T): T =
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

func ceilDiv*[T: SomeInteger and Arithmetic](x, y: T): T {.inline.} =
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
  ## * `system.div proc <system.html#div,int,int>`_ for integer division
  ## * `floorDiv func <#floorDiv,T,T>`_ for integer division which rounds down.
  runnableExamples:
    assert ceilDiv(12, 3) ==  4
    assert ceilDiv(13, 3) ==  5

  assert x >= T(0) and y > T(0)
  when T is SomeUnsignedInt:
    assert x + y - T(1) >= x

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

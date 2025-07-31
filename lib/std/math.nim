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
    proc `/`(x, y: Self): Self
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

when defined(posix) and not defined(genode) and not defined(macosx):
  {.passL: "-lm".}

# tgmath.h provides type-generic macros and they reduces a number of C functions to import.
# But it requires C compiler supports C99.
const CMathHeader = "<tgmath.h>"

{.push header: CMathHeader.}
# These are C macros and can take both float and double type values.
proc c_signbit[T: SomeFloat](x: T): int {.importc: "signbit".}
proc c_copysign[T: SomeFloat](x, y: T): T {.importc: "copysign".}
proc c_fpclassify[T: SomeFloat](x: T): int {.importc: "fpclassify".}
proc c_isnan[T: SomeFloat](x: T): int {.importc: "isnan".}
func c_frexp[T: SomeFloat](x: T; exponent: ptr cint): T {.importc: "frexp".}
{.pop.}

# use push pragma when it is supported
let
  c_fpNormal    {.importc: "FP_NORMAL", header: CMathHeader.}: int
  c_fpSubnormal {.importc: "FP_SUBNORMAL", header: CMathHeader.}: int
  c_fpZero      {.importc: "FP_ZERO", header: CMathHeader.}: int
  c_fpInfinite  {.importc: "FP_INFINITE", header: CMathHeader.}: int
  c_fpNan       {.importc: "FP_NAN", header: CMathHeader.}: int

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

{.push header: CMathHeader.}
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

func divmod*[T: SomeInteger and Arithmetic](x, y: T): (T, T) {.inline.} =
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

{.push header: CMathHeader.}
func sqrt*[T: SomeFloat](x: T): T {.importc: "sqrt".} =
  ## Computes the square root of `x`.
  ##
  ## **See also:**
  ## * `cbrt func <#cbrt,float64>`_ for the cube root
  runnableExamples:
    assert almostEqual(sqrt(4.0), 2.0)
    assert almostEqual(sqrt(1.44), 1.2)
func cbrt*[T: SomeFloat](x: T): T {.importc: "cbrt".} =
  ## Computes the cube root of `x`.
  ##
  ## **See also:**
  ## * `sqrt func <#sqrt,float64>`_ for the square root
  runnableExamples:
    assert almostEqual(cbrt(8.0), 2.0)
    assert almostEqual(cbrt(2.197), 1.3)
    assert almostEqual(cbrt(-27.0), -3.0)
func pow*[T: SomeFloat](x, y: T): T {.importc: "pow".} =
  ## Computes `x` raised to the power of `y`.
  ##
  ## You may use the `^ func <#^, T, U>`_ instead.
  ##
  ## **See also:**
  ## * `^ (SomeNumber, Natural) func <#^,T,Natural>`_
  ## * `^ (SomeNumber, SomeFloat) func <#^,T,U>`_
  ## * `sqrt func <#sqrt,float64>`_
  ## * `cbrt func <#cbrt,float64>`_
  runnableExamples:
    assert almostEqual(pow(100, 1.5), 1000.0)
    assert almostEqual(pow(16.0, 0.5), 4.0)
func hypot*[T: SomeFloat](x, y: T): T {.importc: "hypot".} =
  ## Computes the length of the hypotenuse of a right-angle triangle with
  ## `x` as its base and `y` as its height. Equivalent to `sqrt(x*x + y*y)`.
  runnableExamples:
    assert almostEqual(hypot(3.0, 4.0), 5.0)
func exp*[T: SomeFloat](x: T): T {.importc: "exp".} =
  ## Computes the exponential function of `x` (`e^x`).
  ##
  ## **See also:**
  ## * `ln func <#ln,float64>`_
  runnableExamples:
    assert almostEqual(exp(1.0), E)
    assert almostEqual(ln(exp(4.0)), 4.0)
    assert almostEqual(exp(0.0), 1.0)
func ln*[T: SomeFloat](x: T): T {.importc: "log".} =
  ## Computes the [natural logarithm](https://en.wikipedia.org/wiki/Natural_logarithm)
  ## of `x`.
  ##
  ## **See also:**
  ## * `log func <#log,T,T>`_
  ## * `log10 func <#log10,float64>`_
  ## * `log2 func <#log2,float64>`_
  ## * `exp func <#exp,float64>`_
  runnableExamples:
    assert almostEqual(ln(exp(4.0)), 4.0)
    assert almostEqual(ln(1.0), 0.0)
    assert almostEqual(ln(0.0), -Inf)
    assert ln(-7.0).isNaN
func log2*[T: SomeFloat](x: T): T {.importc: "log2".} =
  ## Computes the binary logarithm (base 2) of `x`.
  ##
  ## **See also:**
  ## * `log func <#log,T,T>`_
  ## * `log10 func <#log10,float64>`_
  ## * `ln func <#ln,float64>`_
  runnableExamples:
    assert almostEqual(log2(8.0), 3.0)
    assert almostEqual(log2(1.0), 0.0)
    assert almostEqual(log2(0.0), -Inf)
    assert log2(-2.0).isNaN
func log10*[T: SomeFloat](x: T): T {.importc: "log10".} =
  ## Computes the common logarithm (base 10) of `x`.
  ##
  ## **See also:**
  ## * `ln func <#ln,float64>`_
  ## * `log func <#log,T,T>`_
  ## * `log2 func <#log2,float64>`_
  runnableExamples:
    assert almostEqual(log10(100.0) , 2.0)
    assert almostEqual(log10(0.0), -Inf)
    assert log10(-100.0).isNaN
{.pop.}

# remove this when Natural type is added to system
type
  Natural = int

func `^`*[T: SomeNumber and Arithmetic](x: T; y: Natural): T =
  ## Computes `x` to the power of `y`.
  ##
  ## The exponent `y` must be non-negative, use
  ## `pow <#pow,float64,float64>`_ for negative exponents.
  ##
  ## **See also:**
  ## * `^ func <#^,T,U>`_ for negative exponent or floats
  ## * `pow func <#pow,float64,float64>`_ for `float32` or `float64` output
  ## * `sqrt func <#sqrt,float64>`_
  ## * `cbrt func <#cbrt,float64>`_
  runnableExamples:
    assert -3 ^ 0 == 1
    assert -3 ^ 1 == -3
    assert -3 ^ 2 == 9

  case y
  of 0: result = 1
  of 1: result = x
  of 2: result = x * x
  of 3: result = x * x * x
  else:
    var (x, y) = (x, y)
    result = 1
    while true:
      if (y and 1) != 0:
        result *= x
      y = y shr 1
      if y == 0:
        break
      x *= x

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

const RadPerDeg = PI / 180.0  ## Number of radians per degree.

func degToRad*[T: SomeFloat and Arithmetic](d: T): T {.inline.} =
  ## Converts from degrees to radians.
  ##
  ## **See also:**
  ## * `radToDeg func <#radToDeg,T>`_
  runnableExamples:
    assert almostEqual(degToRad(180.0), PI)

  result = d * T(RadPerDeg)

func radToDeg*[T: SomeFloat and Arithmetic](r: T): T {.inline.} =
  ## Converts from radians to degrees.
  ##
  ## **See also:**
  ## * `degToRad func <#degToRad,T>`_
  runnableExamples:
    assert almostEqual(radToDeg(2 * PI), 360.0)

  result = r / T(RadPerDeg)

{.push header: CMathHeader.}
func sin*[T: SomeFloat](x: T): T {.importc: "sin".} =
  ## Computes the sine of `x`.
  ##
  ## **See also:**
  ## * `arcsin func <#arcsin,float64>`_
  runnableExamples:
    assert almostEqual(sin(PI / 6), 0.5)
    assert almostEqual(sin(degToRad(90.0)), 1.0)
func cos*[T: SomeFloat](x: T): T {.importc: "cos".} =
  ## Computes the cosine of `x`.
  ##
  ## **See also:**
  ## * `arccos func <#arccos,float64>`_
  runnableExamples:
    assert almostEqual(cos(2 * PI), 1.0)
    assert almostEqual(cos(degToRad(60.0)), 0.5)
func tan*[T: SomeFloat](x: T): T {.importc: "tan".} =
  ## Computes the tangent of `x`.
  ##
  ## **See also:**
  ## * `arctan func <#arctan,float64>`_
  runnableExamples:
    assert almostEqual(tan(degToRad(45.0)), 1.0)
    assert almostEqual(tan(PI / 4), 1.0)
func arcsin*[T: SomeFloat](x: T): T {.importc: "asin".} =
  ## Computes the arc sine of `x`.
  ##
  ## **See also:**
  ## * `sin func <#sin,float64>`_
  runnableExamples:
    assert almostEqual(radToDeg(arcsin(0.0)), 0.0)
    assert almostEqual(radToDeg(arcsin(1.0)), 90.0)
func arccos*[T: SomeFloat](x: T): T {.importc: "acos".} =
  ## Computes the arc cosine of `x`.
  ##
  ## **See also:**
  ## * `cos func <#cos,float64>`_
  runnableExamples:
    assert almostEqual(radToDeg(arccos(0.0)), 90.0)
    assert almostEqual(radToDeg(arccos(1.0)), 0.0)
func arctan*[T: SomeFloat](x: T): T {.importc: "atan".} =
  ## Calculate the arc tangent of `x`.
  ##
  ## **See also:**
  ## * `arctan2 func <#arctan2,float64,float64>`_
  ## * `tan func <#tan,float64>`_
  runnableExamples:
    assert almostEqual(arctan(1.0), 0.7853981633974483)
    assert almostEqual(radToDeg(arctan(1.0)), 45.0)
func arctan2*[T: SomeFloat](y, x: T): T {.importc: "atan2".} =
  ## Calculate the arc tangent of `y/x`.
  ##
  ## It produces correct results even when the resulting angle is near
  ## `PI/2` or `-PI/2` (`x` near 0).
  ##
  ## **See also:**
  ## * `arctan func <#arctan,float64>`_
  runnableExamples:
    assert almostEqual(arctan2(1.0, 0.0), PI / 2.0)
    assert almostEqual(radToDeg(arctan2(1.0, 0.0)), 90.0)
func sinh*[T: SomeFloat](x: T): T {.importc: "sinh".} =
  ## Computes the [hyperbolic sine](https://en.wikipedia.org/wiki/Hyperbolic_function#Definitions) of `x`.
  ##
  ## **See also:**
  ## * `arcsinh func <#arcsinh,float64>`_
  runnableExamples:
    assert almostEqual(sinh(0.0), 0.0)
    assert almostEqual(sinh(1.0), 1.175201193643801)
func cosh*[T: SomeFloat](x: T): T {.importc: "cosh".} =
  ## Computes the [hyperbolic cosine](https://en.wikipedia.org/wiki/Hyperbolic_function#Definitions) of `x`.
  ##
  ## **See also:**
  ## * `arccosh func <#arccosh,float64>`_
  runnableExamples:
    assert almostEqual(cosh(0.0), 1.0)
    assert almostEqual(cosh(1.0), 1.543080634815244)
func tanh*[T: SomeFloat](x: T): T {.importc: "tanh".} =
  ## Computes the [hyperbolic tangent](https://en.wikipedia.org/wiki/Hyperbolic_function#Definitions) of `x`.
  ##
  ## **See also:**
  ## * `arctanh func <#arctanh,float64>`_
  runnableExamples:
    assert almostEqual(tanh(0.0), 0.0)
    assert almostEqual(tanh(1.0), 0.7615941559557649)
{.pop.}

import std/[assertions, fenv]
import std/math/common
export common

when defined(nimNativeIo):
  import std/math/frexp
  import std/math/rounding
  import std/math/power
  import std/math/exponential
  import std/math/trig
  import std/math/hyperbolic
  import std/math/special
  export frexp, rounding, power, exponential, trig, hyperbolic, special
else:
  import std/math/cmath
  export cmath

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

func log*[T: SomeFloat](x, base: T): T {.untyped.} =
  ## Computes the logarithm of `x` to base `base`.
  ##
  ## **See also:**
  ## * `ln func <#ln,float64>`_
  ## * `log10 func <#log10,float64>`_
  ## * `log2 func <#log2,float64>`_
  runnableExamples:
    assert almostEqual(log(9.0, 3.0), 2.0)
    assert almostEqual(log(0.0, 2.0), -Inf)
    assert log(-7.0, 4.0).isNaN
    assert log(8.0, -2.0).isNaN

  ln(x) / ln(base)

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
      let z = x
      x *= z

func cot*(x: float32): float32 = 1.0'f32 / tan(x)
func cot*(x: float64): float64 = 1.0 / tan(x)
func sec*(x: float32): float32 = 1.0'f32 / cos(x)
func sec*(x: float64): float64 = 1.0 / cos(x)
func csc*(x: float32): float32 = 1.0'f32 / sin(x)
func csc*(x: float64): float64 = 1.0 / sin(x)
func coth*(x: float32): float32 = 1.0'f32 / tanh(x)
func coth*(x: float64): float64 = 1.0 / tanh(x)
func sech*(x: float32): float32 = 1.0'f32 / cosh(x)
func sech*(x: float64): float64 = 1.0 / cosh(x)
func csch*(x: float32): float32 = 1.0'f32 / sinh(x)
func csch*(x: float64): float64 = 1.0 / sinh(x)
func arccot*(x: float32): float32 = arctan(1.0'f32 / x)
func arccot*(x: float64): float64 = arctan(1.0 / x)
func arcsec*(x: float32): float32 = arccos(1.0'f32 / x)
func arcsec*(x: float64): float64 = arccos(1.0 / x)
func arccsc*(x: float32): float32 = arcsin(1.0'f32 / x)
func arccsc*(x: float64): float64 = arcsin(1.0 / x)
func arccoth*(x: float32): float32 = arctanh(1.0'f32 / x)
func arccoth*(x: float64): float64 = arctanh(1.0 / x)
func arcsech*(x: float32): float32 = arccosh(1.0'f32 / x)
func arcsech*(x: float64): float64 = arccosh(1.0 / x)
func arccsch*(x: float32): float32 = arcsinh(1.0'f32 / x)
func arccsch*(x: float64): float64 = arcsinh(1.0 / x)

func splitDecimal*[T: SomeFloat and FloatArithmetic and HasDefault](x: T): tuple[intpart: T, floatpart: T] {.untyped.} =
  ## Breaks `x` into an integer and a fractional part.
  ##
  ## Returns a tuple containing `intpart` and `floatpart`, representing
  ## the integer part and the fractional part, respectively.
  ##
  ## Both parts have the same sign as `x`.  Analogous to the `modf`
  ## function in C.
  runnableExamples:
    assert splitDecimal(5.25) == (intpart: 5.0, floatpart: 0.25)
    assert splitDecimal(-2.73) == (intpart: -2.0, floatpart: -0.73)
  result = default(tuple[intpart: T, floatpart: T])
  var absolute = abs(x)
  result.intpart = floor(absolute)
  result.floatpart = absolute - result.intpart
  if x < T(0):
    result.intpart = -result.intpart
    result.floatpart = -result.floatpart

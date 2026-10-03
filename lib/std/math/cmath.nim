## The libc `<math.h>` bindings backing `std/math` when `nimNativeIo` is
## *not* defined. Every function here is a thin `importc` wrapper; the
## freestanding/native reimplementations live in the sibling `math/xxx.nim`
## modules (`sin`, `exponential`, ...) selected instead of this module by
## `std/math` when `nimNativeIo` is defined.
import std/math/common

when defined(posix) and not defined(genode) and not defined(macosx):
  {.passL: "-lm".}

const CMathHeader = "<math.h>"

{.push header: CMathHeader.}
# These are C macros and can take both float and double type values.
func c_signbit[T: SomeFloat](x: T): int {.importc: "signbit".}
func c_fpclassify[T: SomeFloat](x: T): int {.importc: "fpclassify".}
func c_isnan[T: SomeFloat](x: T): int {.importc: "isnan".}

func c_frexp(x: float32; exponent: ptr cint): float32 {.importc: "frexpf".}
func c_frexp(x: float64; exponent: ptr cint): float64 {.importc: "frexp".}
{.pop.}

# use push pragma when it is supported
let
  c_fpNormal    {.importc: "FP_NORMAL", header: CMathHeader.}: int
  c_fpSubnormal {.importc: "FP_SUBNORMAL", header: CMathHeader.}: int
  c_fpZero      {.importc: "FP_ZERO", header: CMathHeader.}: int
  c_fpInfinite  {.importc: "FP_INFINITE", header: CMathHeader.}: int
  c_fpNan       {.importc: "FP_NAN", header: CMathHeader.}: int

func signbit*[T: SomeFloat](x: T): bool {.inline.} =
  ## Returns true if `x` is negative, false otherwise.
  runnableExamples:
    assert not signbit(0.0)
    #assert signbit(0.0 * -1.0)
    assert signbit(-0.1)
    assert not signbit(0.1)

  c_signbit(x) != 0

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

func frexp*[T: SomeFloat](x: T): tuple[frac: T, exp: int] {.inline, untyped.} =
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
func copySign*(x, y: float32): float32 {.importc: "copysignf".}
func copySign*(x, y: float64): float64 {.importc: "copysign".} =
  ## Returns a value with the magnitude of `x` and the sign of `y`;
  ## this works even if x or y are NaN, infinity or zero, all of which can carry a sign.
  runnableExamples:
    assert copySign(10.0, 1.0) == 10.0
    assert copySign(10.0, -1.0) == -10.0
    assert copySign(-Inf, -0.0) == -Inf
    assert copySign(NaN, 1.0).isNaN
    assert copySign(1.0, copySign(NaN, -1.0)) == -1.0

func floor*(x: float32): float32 {.importc: "floorf".}
func floor*(x: float64): float64 {.importc: "floor".} =
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

func ceil*(x: float32): float32 {.importc: "ceilf".}
func ceil*(x: float64): float64 {.importc: "ceil".} =
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

func round*(x: float32): float32 {.importc: "roundf".}
func round*(x: float64): float64 {.importc: "round".} =
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

func trunc*(x: float32): float32 {.importc: "truncf".}
func trunc*(x: float64): float64 {.importc: "trunc".} =
  ## Returns the nearest integer not greater in magnitude than `x`.
  ##
  ## **See also:**
  ## * `floor func <#floor,float64>`_
  ## * `ceil func <#ceil,float64>`_
  ## * `round func <#round,float64>`_
  runnableExamples:
    assert trunc(PI) == 3.0
    assert trunc(-1.85) == -1.0

func `mod`*(x, y: float32): float32 {.importc: "fmodf".}
func `mod`*(x, y: float64): float64 {.importc: "fmod".} =
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

{.push header: CMathHeader.}
func sqrt*(x: float32): float32 {.importc: "sqrtf".}
func sqrt*(x: float64): float64 {.importc: "sqrt".} =
  ## Computes the square root of `x`.
  ##
  ## **See also:**
  ## * `cbrt func <#cbrt,float64>`_ for the cube root
  runnableExamples:
    assert almostEqual(sqrt(4.0), 2.0)
    assert almostEqual(sqrt(1.44), 1.2)
func cbrt*(x: float32): float32 {.importc: "cbrtf".}
func cbrt*(x: float64): float64 {.importc: "cbrt".} =
  ## Computes the cube root of `x`.
  ##
  ## **See also:**
  ## * `sqrt func <#sqrt,float64>`_ for the square root
  runnableExamples:
    assert almostEqual(cbrt(8.0), 2.0)
    assert almostEqual(cbrt(2.197), 1.3)
    assert almostEqual(cbrt(-27.0), -3.0)
func pow*(x, y: float32): float32 {.importc: "powf".}
func pow*(x, y: float64): float64 {.importc: "pow".} =
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
    assert almostEqual(pow(100.0, 1.5), 1000.0)
    assert almostEqual(pow(16.0, 0.5), 4.0)
func hypot*(x, y: float32): float32 {.importc: "hypotf".}
func hypot*(x, y: float64): float64 {.importc: "hypot".} =
  ## Computes the length of the hypotenuse of a right-angle triangle with
  ## `x` as its base and `y` as its height. Equivalent to `sqrt(x*x + y*y)`.
  runnableExamples:
    assert almostEqual(hypot(3.0, 4.0), 5.0)
{.pop.}

{.push header: CMathHeader.}
func exp*(x: float32): float32 {.importc: "expf".}
func exp*(x: float64): float64 {.importc: "exp".} =
  ## Computes the exponential function of `x` (`e^x`).
  ##
  ## **See also:**
  ## * `ln func <#ln,float64>`_
  runnableExamples:
    assert almostEqual(exp(1.0), E)
    assert almostEqual(ln(exp(4.0)), 4.0)
    assert almostEqual(exp(0.0), 1.0)
func ln*(x: float32): float32 {.importc: "logf".}
func ln*(x: float64): float64 {.importc: "log".} =
  ## Computes the [natural logarithm](https://en.wikipedia.org/wiki/Natural_logarithm)
  ## of `x`.
  ##
  ## **See also:**
  ## * `log func <#log,T,T>`_
  ## * `log10 func <#log10,float64>`_
  ## * `log2 func <#log2,float64>`_
  ## * `log1p func <#log1p,float64>`_
  ## * `exp func <#exp,float64>`_
  runnableExamples:
    assert almostEqual(ln(exp(4.0)), 4.0)
    assert almostEqual(ln(1.0), 0.0)
    assert almostEqual(ln(0.0), -Inf)
    assert ln(-7.0).isNaN
func log2*(x: float32): float32 {.importc: "log2f".}
func log2*(x: float64): float64 {.importc: "log2".} =
  ## Computes the binary logarithm (base 2) of `x`.
  ##
  ## **See also:**
  ## * `ln func <#ln,float64>`_
  ## * `log func <#log,T,T>`_
  ## * `log10 func <#log10,float64>`_
  ## * `log1p func <#log1p,float64>`_
  runnableExamples:
    assert almostEqual(log2(8.0), 3.0)
    assert almostEqual(log2(1.0), 0.0)
    assert almostEqual(log2(0.0), -Inf)
    assert log2(-2.0).isNaN
func log10*(x: float32): float32 {.importc: "log10f".}
func log10*(x: float64): float64 {.importc: "log10".} =
  ## Computes the common logarithm (base 10) of `x`.
  ##
  ## **See also:**
  ## * `ln func <#ln,float64>`_
  ## * `log func <#log,T,T>`_
  ## * `log2 func <#log2,float64>`_
  ## * `log1p func <#log1p,float64>`_
  runnableExamples:
    assert almostEqual(log10(100.0) , 2.0)
    assert almostEqual(log10(0.0), -Inf)
    assert log10(-100.0).isNaN
func log1p*(x: float32): float32 {.importc: "log1pf".}
func log1p*(x: float64): float64 {.importc: "log1p".} =
  ## Computes the natural logarithm of `1 + x`.
  ##
  ## **See also:**
  ## * `ln func <#ln,float64>`_
  ## * `log func <#log,T,T>`_
  ## * `log10 func <#log10,float64>`_
  ## * `log2 func <#log2,float64>`_
  runnableExamples:
    assert almostEqual(log1p(0.0), 0.0)
    assert almostEqual(log1p(exp(1.0) - 1.0), 1.0)
    assert almostEqual(log1p(exp(2.0) - 1.0), 2.0)
{.pop.}

{.push header: CMathHeader.}
func sin*(x: float32): float32 {.importc: "sinf".}
func sin*(x: float64): float64 {.importc: "sin".} =
  ## Computes the sine of `x`.
  ##
  ## **See also:**
  ## * `arcsin func <#arcsin,float64>`_
  runnableExamples:
    assert almostEqual(sin(PI / 6), 0.5)
    assert almostEqual(sin(degToRad(90.0)), 1.0)
func cos*(x: float32): float32 {.importc: "cosf".}
func cos*(x: float64): float64 {.importc: "cos".} =
  ## Computes the cosine of `x`.
  ##
  ## **See also:**
  ## * `arccos func <#arccos,float64>`_
  runnableExamples:
    assert almostEqual(cos(2 * PI), 1.0)
    assert almostEqual(cos(degToRad(60.0)), 0.5)
func tan*(x: float32): float32 {.importc: "tanf".}
func tan*(x: float64): float64 {.importc: "tan".} =
  ## Computes the tangent of `x`.
  ##
  ## **See also:**
  ## * `arctan func <#arctan,float64>`_
  runnableExamples:
    assert almostEqual(tan(degToRad(45.0)), 1.0)
    assert almostEqual(tan(PI / 4), 1.0)
func arcsin*(x: float32): float32 {.importc: "asinf".}
func arcsin*(x: float64): float64 {.importc: "asin".} =
  ## Computes the arc sine of `x`.
  ##
  ## **See also:**
  ## * `sin func <#sin,float64>`_
  runnableExamples:
    assert almostEqual(radToDeg(arcsin(0.0)), 0.0)
    assert almostEqual(radToDeg(arcsin(1.0)), 90.0)
func arccos*(x: float32): float32 {.importc: "acosf".}
func arccos*(x: float64): float64 {.importc: "acos".} =
  ## Computes the arc cosine of `x`.
  ##
  ## **See also:**
  ## * `cos func <#cos,float64>`_
  runnableExamples:
    assert almostEqual(radToDeg(arccos(0.0)), 90.0)
    assert almostEqual(radToDeg(arccos(1.0)), 0.0)
func arctan*(x: float32): float32 {.importc: "atanf".}
func arctan*(x: float64): float64 {.importc: "atan".} =
  ## Calculate the arc tangent of `x`.
  ##
  ## **See also:**
  ## * `arctan2 func <#arctan2,float64,float64>`_
  ## * `tan func <#tan,float64>`_
  runnableExamples:
    assert almostEqual(arctan(1.0), 0.7853981633974483)
    assert almostEqual(radToDeg(arctan(1.0)), 45.0)
func arctan2*(y, x: float32): float32 {.importc: "atan2f".}
func arctan2*(y, x: float64): float64 {.importc: "atan2".} =
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
{.pop.}

{.push header: CMathHeader.}
func sinh*(x: float32): float32 {.importc: "sinhf".}
func sinh*(x: float64): float64 {.importc: "sinh".} =
  ## Computes the [hyperbolic sine](https://en.wikipedia.org/wiki/Hyperbolic_function#Definitions) of `x`.
  ##
  ## **See also:**
  ## * `arcsinh func <#arcsinh,float64>`_
  runnableExamples:
    assert almostEqual(sinh(0.0), 0.0)
    assert almostEqual(sinh(1.0), 1.175201193643801)
func cosh*(x: float32): float32 {.importc: "coshf".}
func cosh*(x: float64): float64 {.importc: "cosh".} =
  ## Computes the [hyperbolic cosine](https://en.wikipedia.org/wiki/Hyperbolic_function#Definitions) of `x`.
  ##
  ## **See also:**
  ## * `arccosh func <#arccosh,float64>`_
  runnableExamples:
    assert almostEqual(cosh(0.0), 1.0)
    assert almostEqual(cosh(1.0), 1.543080634815244)
func tanh*(x: float32): float32 {.importc: "tanhf".}
func tanh*(x: float64): float64 {.importc: "tanh".} =
  ## Computes the [hyperbolic tangent](https://en.wikipedia.org/wiki/Hyperbolic_function#Definitions) of `x`.
  ##
  ## **See also:**
  ## * `arctanh func <#arctanh,float64>`_
  runnableExamples:
    assert almostEqual(tanh(0.0), 0.0)
    assert almostEqual(tanh(1.0), 0.7615941559557649)
func arcsinh*(x: float32): float32 {.importc: "asinhf".}
func arcsinh*(x: float64): float64 {.importc: "asinh".}
  ## Computes the inverse hyperbolic sine of `x`.
  ##
  ## **See also:**
  ## * `sinh func <#sinh,float64>`_
func arccosh*(x: float32): float32 {.importc: "acoshf".}
func arccosh*(x: float64): float64 {.importc: "acosh".}
  ## Computes the inverse hyperbolic cosine of `x`.
  ##
  ## **See also:**
  ## * `cosh func <#cosh,float64>`_
func arctanh*(x: float32): float32 {.importc: "atanhf".}
func arctanh*(x: float64): float64 {.importc: "atanh".}
  ## Computes the inverse hyperbolic tangent of `x`.
  ##
  ## **See also:**
  ## * `tanh func <#tanh,float64>`_
{.pop.}

{.push header: CMathHeader.}
func erf*(x: float32): float32 {.importc: "erff".}
func erf*(x: float64): float64 {.importc: "erf".}
  ## Computes the [error function](https://en.wikipedia.org/wiki/Error_function) for `x`.
func erfc*(x: float32): float32 {.importc: "erfcf".}
func erfc*(x: float64): float64 {.importc: "erfc".}
  ## Computes the [complementary error function](https://en.wikipedia.org/wiki/Error_function#Complementary_error_function) for `x`.
func gamma*(x: float32): float32 {.importc: "tgammaf".}
func gamma*(x: float64): float64 {.importc: "tgamma".} =
  ## Computes the [gamma function](https://en.wikipedia.org/wiki/Gamma_function) for `x`.
  ##
  ## **See also:**
  ## * `lgamma func <#lgamma,float64>`_ for the natural logarithm of the gamma function
  runnableExamples:
    assert almostEqual(gamma(1.0), 1.0)
    assert almostEqual(gamma(4.0), 6.0)
    assert almostEqual(gamma(11.0), 3628800.0)
func lgamma*(x: float32): float32 {.importc: "lgammaf".}
func lgamma*(x: float64): float64 {.importc: "lgamma".} =
  ## Computes the natural logarithm of the gamma function for `x`.
  ##
  ## **See also:**
  ## * `gamma func <#gamma,float64>`_ for gamma function
{.pop.}

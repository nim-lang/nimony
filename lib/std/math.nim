import std/[assertions, fenv]

const
  PI* = 3.1415926535897932384626433          ## The circle constant PI (Ludolph's number).

# These are C macros and can take both float and double type values.
proc c_signbit[T: SomeFloat](x: T): int {.importc: "signbit", header: "<math.h>".}
proc c_fpclassify[T: SomeFloat](x: T): int {.importc: "fpclassify", header: "<math.h>".}

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

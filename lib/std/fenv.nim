## Floating-point environment. Handling of floating-point rounding and
## exceptions (overflow, division by zero, etc.).
## The types, vars and procs are bindings for the C standard library
## [<fenv.h>](https://en.cppreference.com/w/c/numeric/fenv) header.

const
  FLT_MIN = 1.17549435e-38'f32     ## the minimum value of a float
  FLT_EPSILON = 1.19209290e-07'f32 ## the difference between 1 and the least value greater than 1 of a float

  DBL_MIN = 2.2250738585072014E-308    ## the minimal value of a double
  DBL_EPSILON = 2.2204460492503131E-16 ## the difference between 1 and the least value greater than 1 of a double

template minimumPositiveValue*(T: typedesc[float32]): float32 = FLT_MIN
  ## The smallest positive (nonzero) number that can be represented in a
  ## 32-bit floating-point type.
template epsilon*(T: typedesc[float32]): float32 = FLT_EPSILON
  ## The difference between 1.0 and the smallest number greater than
  ## 1.0 that can be represented in a 32-bit floating-point type.

template minimumPositiveValue*(T: typedesc[float64]): float64 = DBL_MIN
  ## The smallest positive (nonzero) number that can be represented in a
  ## 64-bit floating-point type.
template epsilon*(T: typedesc[float64]): float64 = DBL_EPSILON
  ## The difference between 1.0 and the smallest number greater than
  ## 1.0 that can be represented in a 64-bit floating-point type.

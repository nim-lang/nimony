import std/[assertions, math]

## Generic bodies with inherited concepts (`concept of`) must pick the
## structurally matching overload, not accept a scalar type parameter
## when the argument is a generic constructor over the same type variable.
## `std/math.pow` must not participate when the arguments are `Complex[T]`.

type
  AdditiveSemigroup* = concept
    proc `+`*(a, b: Self): Self
    proc `==`*(a, b: Self): bool

  ScalarPart* = concept of AdditiveSemigroup

type Complex*[T: ScalarPart] = object
  re*, im*: T

func `^`*[T: ScalarPart](base: Complex[T], exp: Complex[T]): Complex[T] =
  pow(base, exp)

func `^`*[T: ScalarPart](base: Complex[T], exp: T): Complex[T] =
  pow(base, exp)

func pow*[T: ScalarPart](base: Complex[T], exp: Complex[T]): Complex[T] =
  base

func pow*[T: ScalarPart](base: Complex[T], exp: T): Complex[T] =
  base

func pow*[T: ScalarPart](base: Complex[T], exp: int): Complex[T] =
  base

proc checkConcrete() =
  var a = Complex[float64](re: 2.0, im: 1.0)
  var b = Complex[float64](re: 0.0, im: 1.0)
  discard a ^ b
  discard a ^ 2.0

checkConcrete()

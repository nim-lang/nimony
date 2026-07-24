## Minimal generic dual-style library for concept-check / overload-resolution blowup.
##
## Pattern: a compound concept with many `std/math` requirements, generic
## operators on a wrapper type, and bodies that call unqualified `sin`/`exp`
## so `import std/math` competes with generic overloads during instantiation.

import std/math

export math

type
  Comparable* = concept
    func `==`(a, b: Self): bool
    func `<`(a, b: Self): bool

  Field* = concept of Comparable
    func `+`(a, b: Self): Self
    func `-`(a: Self): Self
    func `-`(a, b: Self): Self
    func `*`(a, b: Self): Self
    func `/`(a, b: Self): Self

  Scalar* = concept
    func high(t: typedesc[Self]): Self
    func low(t: typedesc[Self]): Self

  RealScalar* = concept of Scalar
    func e(t: typedesc[Self]): Self
    func epsilon(t: typedesc[Self]): Self
    func identity(t: typedesc[Self]): Self
    func pi(t: typedesc[Self]): Self
    func tau(t: typedesc[Self]): Self

  FatPart* = concept of Comparable, Field, RealScalar
    func arccos(x: Self): Self
    func arccosh(x: Self): Self
    func arccot(x: Self): Self
    func arccoth(x: Self): Self
    func arccsc(x: Self): Self
    func arccsch(x: Self): Self
    func arcsec(x: Self): Self
    func arcsech(x: Self): Self
    func arcsin(x: Self): Self
    func arcsinh(x: Self): Self
    func arctan(x: Self): Self
    func arctanh(x: Self): Self
    func cos(x: Self): Self
    func cosh(x: Self): Self
    func cot(x: Self): Self
    func coth(x: Self): Self
    func csc(x: Self): Self
    func csch(x: Self): Self
    func exp(x: Self): Self
    func ln(x: Self): Self
    func sec(x: Self): Self
    func sech(x: Self): Self
    func sin(x: Self): Self
    func sinh(x: Self): Self
    func sqrt(x: Self): Self
    func tan(x: Self): Self
    func tanh(x: Self): Self


## Scalar traits required by `FatPart` (mirrors frehmwoerk numerictraits)
## -----------------------------------------------------------------------

func e*(x: typedesc[float32]): float32 = 2.7182818'f32
func epsilon*(x: typedesc[float32]): float32 = 1.1920929e-7'f32
func identity*(x: typedesc[float32]): float32 = 1.0'f32
func high*(x: typedesc[float32]): float32 = 3.4028235e38'f32
func low*(x: typedesc[float32]): float32 = -3.4028235e38'f32
func pi*(x: typedesc[float32]): float32 = 3.1415927'f32
func tau*(x: typedesc[float32]): float32 = 6.2831855'f32

func e*(x: typedesc[float64]): float64 = 2.718281828459045
func epsilon*(x: typedesc[float64]): float64 = 2.220446049250313e-16
func identity*(x: typedesc[float64]): float64 = 1.0
func high*(x: typedesc[float64]): float64 = Inf
func low*(x: typedesc[float64]): float64 = -Inf
func pi*(x: typedesc[float64]): float64 = PI
func tau*(x: typedesc[float64]): float64 = 6.283185307179586


type
  Box*[T: FatPart] = object
    re, du: T

func newBox*[T: FatPart](re, du: T): Box[T] =
  Box[T](re: re, du: du)

func real*[T: FatPart](a: Box[T]): T = a.re
func dual*[T: FatPart](a: Box[T]): T = a.du

func `==`*[T: FatPart](a, b: Box[T]): bool =
  a.re == b.re and a.du == b.du

func `+`*[T: FatPart](a, b: Box[T]): Box[T] =
  Box[T](re: a.re + b.re, du: a.du + b.du)

func `*`*[T: FatPart](a, b: Box[T]): Box[T] =
  Box[T](re: a.re * b.re, du: a.du * b.re + a.re * b.du)

func `/`*[T: FatPart](a, b: Box[T]): Box[T] =
  Box[T](re: a.re / b.re, du: (a.du * b.re - a.re * b.du) / (b.re * b.re))

func exp*[T: FatPart](a: Box[T]): Box[T] =
  let re = exp(a.re)
  Box[T](re: re, du: a.du * re)

func ln*[T: FatPart](a: Box[T]): Box[T] =
  let re = ln(a.re)
  Box[T](re: re, du: a.du / a.re)

func sqrt*[T: FatPart](a: Box[T]): Box[T] =
  let t = sqrt(a.re)
  let two = T.identity + T.identity
  Box[T](re: t, du: a.du * t / two)

func sin*[T: FatPart](a: Box[T]): Box[T] =
  let re = sin(a.re)
  Box[T](re: re, du: a.du * cos(a.re))

func cos*[T: FatPart](a: Box[T]): Box[T] =
  let re = cos(a.re)
  Box[T](re: re, du: -a.du * sin(a.re))

func tan*[T: FatPart](a: Box[T]): Box[T] =
  let ca = cos(a.re)
  let re = tan(a.re)
  Box[T](re: re, du: a.du / (ca * ca))

func cot*[T: FatPart](a: Box[T]): Box[T] =
  let ct = cot(a.re)
  let one = T.identity
  Box[T](re: ct, du: -a.du * (one + ct * ct))

func sec*[T: FatPart](a: Box[T]): Box[T] =
  let re = sec(a.re)
  Box[T](re: re, du: a.du * tan(a.re) * sec(a.re))

func csc*[T: FatPart](a: Box[T]): Box[T] =
  let re = csc(a.re)
  Box[T](re: re, du: -a.du * csc(a.re) * cot(a.re))

func sinh*[T: FatPart](a: Box[T]): Box[T] =
  let re = sinh(a.re)
  Box[T](re: re, du: a.du * cosh(a.re))

func cosh*[T: FatPart](a: Box[T]): Box[T] =
  let re = cosh(a.re)
  Box[T](re: re, du: a.du * sinh(a.re))

func tanh*[T: FatPart](a: Box[T]): Box[T] =
  let t = tanh(a.re)
  let one = T.identity
  Box[T](re: t, du: a.du * (one - t * t))

func arcsin*[T: FatPart](a: Box[T]): Box[T] =
  let re = arcsin(a.re)
  let one = T.identity
  Box[T](re: re, du: a.du / sqrt(one - a.re * a.re))

func arccos*[T: FatPart](a: Box[T]): Box[T] =
  let re = arccos(a.re)
  let one = T.identity
  Box[T](re: re, du: -a.du / sqrt(one - a.re * a.re))

func arctan*[T: FatPart](a: Box[T]): Box[T] =
  let re = arctan(a.re)
  let one = T.identity
  Box[T](re: re, du: a.du / (one + a.re * a.re))

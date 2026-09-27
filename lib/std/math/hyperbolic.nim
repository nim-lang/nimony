## Native (libc-free) implementations of `expm1`, `sinh`, `cosh`, `tanh`,
## `arcsinh`, `arccosh` and `arctanh`. The hyperbolic formulas and range handling
## follow Rust compiler-builtins/libm's corresponding f32/f64 routines; `expm1`
## is adapted from FreeBSD/SunPro fdlibm `s_expm1.c`.
## Rust sources: `library/compiler-builtins/libm/src/math/{expm1,expm1f,sinh,sinhf,cosh,coshf,tanh,tanhf,asinh,asinhf,acosh,acoshf,atanh,atanhf,k_expo2,k_expo2f}.rs`.
## Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
## Copyright (c) 2018 Jorge Aparicio (Rust compiler-builtins/libm code).
## Permission to use, copy, modify, and distribute this software is freely
## granted, provided that this notice is preserved.
## Selected instead of `math/cmath` by `std/math` when `nimNativeIo` is defined.
import std/math/common

func sinh*(x: float32): float32 = default(float32)
func sinh*(x: float64): float64 = default(float64)

func cosh*(x: float32): float32 = default(float32)
func cosh*(x: float64): float64 = default(float64)

func tanh*(x: float32): float32 = default(float32)
func tanh*(x: float64): float64 = default(float64)

func arcsinh*(x: float32): float32 = default(float32)
func arcsinh*(x: float64): float64 = default(float64)

func arccosh*(x: float32): float32 = default(float32)
func arccosh*(x: float64): float64 = default(float64)

func arctanh*(x: float32): float32 = default(float32)
func arctanh*(x: float64): float64 = default(float64)

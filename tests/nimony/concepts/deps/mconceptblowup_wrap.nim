## Thin wrappers that force generic `Box[float32]` instantiation (dualfloat32 pattern).

import std/math

import ./mconceptblowup_lib as lib

func wrapPlus*(a, b: lib.Box[float32]): lib.Box[float32] = lib.`+`(a, b)
func wrapMul*(a, b: lib.Box[float32]): lib.Box[float32] = lib.`*`(a, b)
func wrapDiv*(a, b: lib.Box[float32]): lib.Box[float32] = lib.`/`(a, b)
func wrapExp*(a: lib.Box[float32]): lib.Box[float32] = lib.exp(a)
func wrapLn*(a: lib.Box[float32]): lib.Box[float32] = lib.ln(a)
func wrapSqrt*(a: lib.Box[float32]): lib.Box[float32] = lib.sqrt(a)
func wrapSin*(a: lib.Box[float32]): lib.Box[float32] = lib.sin(a)
func wrapCos*(a: lib.Box[float32]): lib.Box[float32] = lib.cos(a)
func wrapTan*(a: lib.Box[float32]): lib.Box[float32] = lib.tan(a)
func wrapCot*(a: lib.Box[float32]): lib.Box[float32] = lib.cot(a)
func wrapSec*(a: lib.Box[float32]): lib.Box[float32] = lib.sec(a)
func wrapCsc*(a: lib.Box[float32]): lib.Box[float32] = lib.csc(a)
func wrapSinh*(a: lib.Box[float32]): lib.Box[float32] = lib.sinh(a)
func wrapCosh*(a: lib.Box[float32]): lib.Box[float32] = lib.cosh(a)
func wrapTanh*(a: lib.Box[float32]): lib.Box[float32] = lib.tanh(a)
func wrapArcsin*(a: lib.Box[float32]): lib.Box[float32] = lib.arcsin(a)
func wrapArccos*(a: lib.Box[float32]): lib.Box[float32] = lib.arccos(a)
func wrapArctan*(a: lib.Box[float32]): lib.Box[float32] = lib.arctan(a)

## Regression for concept-check blowup when instantiating generic math wrappers.
##
## Before the fix, compiling the wrapper module took tens of seconds because
## each thin wrapper forces `Box[float32]` instantiation and expensive `FatPart`
## checking plus `std/math` overload resolution inside generic bodies.
##
## After the fix, semantics for this test should finish in well under a second.

import std/[assertions, math]

import deps/mconceptblowup_lib as lib
import deps/mconceptblowup_wrap

let a = lib.newBox(1.0'f32, 2.0'f32)
let b = lib.newBox(3.0'f32, 4.0'f32)

# Instantiate every generic wrapper (the compile-time stressor).
discard wrapPlus(a, b)
discard wrapMul(a, b)
discard wrapDiv(a, b)
discard wrapExp(lib.newBox(0.0'f32, 1.0'f32))
discard wrapLn(lib.newBox(2.0'f32, 1.0'f32))
discard wrapSqrt(lib.newBox(4.0'f32, 4.0'f32))
discard wrapSin(lib.newBox(0.0'f32, 1.0'f32))
discard wrapCos(lib.newBox(0.0'f32, 1.0'f32))
discard wrapTan(lib.newBox(0.0'f32, 1.0'f32))
discard wrapCot(lib.newBox(1.0'f32, 0.0'f32))
discard wrapSec(lib.newBox(0.0'f32, 1.0'f32))
discard wrapCsc(lib.newBox(1.0'f32, 0.0'f32))
discard wrapSinh(lib.newBox(0.0'f32, 1.0'f32))
discard wrapCosh(lib.newBox(0.0'f32, 1.0'f32))
discard wrapTanh(lib.newBox(0.0'f32, 1.0'f32))
discard wrapArcsin(lib.newBox(0.0'f32, 1.0'f32))
discard wrapArccos(lib.newBox(0.5'f32, 1.0'f32))
discard wrapArctan(lib.newBox(0.0'f32, 1.0'f32))

assert lib.real(wrapPlus(a, b)) == 4.0'f32
assert lib.dual(wrapPlus(a, b)) == 6.0'f32

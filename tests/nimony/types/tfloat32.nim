import std/[syncio, assertions]

## `float32` values, at the places where a 32-bit float is easy to mistake for a
## 64-bit one.
##
## Every case here failed on the AArch64 native backend at once, all for the same
## reason: a width that was assumed to be 64 rather than read from the type. That
## is harmless for a float64 and harmless for a register-to-register move of a
## float32 — which is why it survived — but it is fatal for two things. A float
## LITERAL picks its bit pattern from the width, so a `float32` literal built as a
## `double` has zeros in the half a 32-bit read looks at. And a store picks its
## instruction from the width, so an 8-byte store into a 4-byte element writes over
## the neighbour.
##
## `1.0'f32` reading back as `0.0` is the signature of both, because the low half
## of the `double` 1.0 is zero — as it is for every value with an empty mantissa
## tail, i.e. exactly the round numbers a test is most likely to use.

proc bits(x: float32): uint32 = cast[uint32](x)

proc viaCall(dst: var float32; v: float32) = dst = v

proc roundTrip(v: float32): float32 {.noinline.} = v

type FBox = object
  f: float32
  n: int

var gScalar: float32 = 1.5'f32
var gArr: array[2, float32] = [1.5'f32, 2.5'f32]
var gBox = FBox(f: 1.5'f32, n: 0)

proc main =
  # A float32 literal stored through an out-of-line call. Needs a real call: a
  # small proc is inlined and the literal folded, which is why `seq[float32]`'s
  # generic `[]=` was where this first showed up rather than any direct call.
  var viaProc = 0.0'f32
  viaCall(viaProc, 1.0'f32)
  assert bits(viaProc) == 0x3F800000'u32

  # seq[float32]: stores and loads through the generic accessors.
  var s = newSeq[float32](4)
  s[0] = 1.0'f32
  s[1] = 2.0'f32
  s[2] = 4.0'f32
  s[3] = 0.5'f32
  assert bits(s[0]) == 0x3F800000'u32
  assert bits(s[1]) == 0x40000000'u32
  assert bits(s[2]) == 0x40800000'u32
  assert bits(s[3]) == 0x3F000000'u32

  # ... and read back with a loop-variable index, not a constant one.
  var seen = 0'u32
  for i in 0 ..< s.len:
    seen = seen xor bits(s[i])
  assert seen == (0x3F800000'u32 xor 0x40000000'u32 xor 0x40800000'u32 xor 0x3F000000'u32)

  # array[N, float32]: a direct element store, no accessor call. This is the
  # 8-byte-store half — element 0's store must not reach element 1.
  var a: array[3, float32] = [0.0'f32, 0.0'f32, 0.0'f32]
  a[0] = 1.0'f32
  a[1] = 2.0'f32
  a[2] = 3.0'f32
  assert bits(a[0]) == 0x3F800000'u32
  assert bits(a[1]) == 0x40000000'u32
  assert bits(a[2]) == 0x40400000'u32
  for i in 0 ..< 3:
    assert bits(a[i]) != 0'u32

  # Arithmetic keeps float32 precision rather than silently widening.
  var acc = 0.0'f32
  for i in 0 ..< s.len:
    acc = acc + s[i]
  assert bits(acc) == bits(7.5'f32)

  # float64 in the same shapes, as the control: these always worked, and a fix
  # that traded one width for the other would show up here.
  var d = newSeq[float64](3)
  d[0] = 1.0; d[1] = 2.0; d[2] = 4.0
  var dacc = 0.0
  for i in 0 ..< d.len: dacc = dacc + d[i]
  assert cast[uint64](dacc) == cast[uint64](7.0)

  # A float32 passed BY VALUE to an out-of-line call. The argument register is
  # the destination whose width the literal's bit pattern comes from, and it
  # was hardcoded to 8 bytes on both targets.
  assert bits(roundTrip(7.5'f32)) == bits(7.5'f32)

  # Initializers, not just stores. An `aconstr` element and a float global each
  # decide a width of their own, and each defaulted to 64 independently of the
  # stores above — so a fix to the store paths alone leaves these reading 0.0.
  assert bits(gScalar) == bits(1.5'f32)
  gScalar = 3.5'f32
  assert bits(gScalar) == bits(3.5'f32)
  assert bits(gArr[0]) == bits(1.5'f32)
  assert bits(gBox.f) == bits(1.5'f32)

  var localArr: array[2, float32] = [1.5'f32, 2.5'f32]
  assert bits(localArr[0]) == bits(1.5'f32)
  assert bits(localArr[1]) == bits(2.5'f32)

  var fromLit: seq[float32] = @[1.5'f32, 2.5'f32]
  assert bits(fromLit[0]) == bits(1.5'f32)

  # An object field's default inside a constructor, which is the width a
  # synthesized `oconstr` — a coroutine frame's, say — depends on.
  var box = FBox(f: 1.5'f32, n: 0)
  assert bits(box.f) == bits(1.5'f32)
  box.f = 7.5'f32
  assert bits(box.f) == bits(7.5'f32)

  # Comparison against a LITERAL. Here the compare instruction already had the
  # right width, so the literal operand — the only one with no type to read a
  # width from — was compared as the wrong half of a double.
  var cmpV = 3.0'f32
  assert cmpV > 2.9'f32
  assert cmpV < 3.1'f32
  assert cmpV == 3.0'f32
  var cmpOther = 2.9'f32
  assert cmpV > cmpOther     # the variable-operand form, which always worked

  echo "float32 ok"

main()

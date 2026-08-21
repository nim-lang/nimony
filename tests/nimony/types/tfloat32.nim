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

  echo "float32 ok"

main()

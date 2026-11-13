# The translation of OpenSSL's x86-64 Montgomery code in
# `std/crypto/private/ctbignumx64`, routine by routine, against a plain
# reference: bn_mul_mont, bn_get_bits5, bn_scatter5, bn_gather5,
# bn_mul_mont_gather5, bn_mul4x_mont_gather5 and bn_power5 (the five
# squarings of bnPower5Sqr5, then the gathered multiplication), over sizes of
# 2..64 words, moduli with and without the top bit, and `rp` aliasing `ap`.
# Native-only, like the rest of this directory (`hastur nativetests`).
import std/syncio
import std/crypto/private/ctbignumx64

proc rnd(seed: var uint64): uint64 =
  seed = seed xor (seed shl 13)
  seed = seed xor (seed shr 7)
  seed = seed xor (seed shl 17)
  result = seed

proc negInv64(m0: uint64): uint64 =
  var inv = 1'u64
  for i in 0 ..< 6: inv = inv * (2'u64 - m0 * inv)
  result = 0'u64 - inv

proc w(x: var seq[uint64]): W64 = cast[W64](addr x[0])

proc geq(a, m: seq[uint64]): bool =
  result = true
  var i = a.len - 1
  while i >= 0:
    if a[i] != m[i]: return a[i] > m[i]
    dec i

proc subm(a: var seq[uint64]; m: seq[uint64]) =
  var br = 0'u64
  for i in 0 ..< a.len:
    let x = a[i] - m[i] - br
    br = if a[i] < m[i] or (a[i] == m[i] and br == 1'u64): 1'u64 else: 0'u64
    a[i] = x

proc reduce(a: var seq[uint64]; m: seq[uint64]) =
  # OpenSSL's 4x/8x paths leave results below R, not always below m
  while geq(a, m): subm(a, m)

# reference: 32-bit CIOS on 2*num limbs (R = 2^(64 num)), full reduction
proc refMont(a, b, m: seq[uint64]; num: int): seq[uint64] =
  let n = 2*num
  var A = newSeq[uint32](n)
  var B = newSeq[uint32](n)
  var M = newSeq[uint32](n)
  for i in 0 ..< num:
    A[2*i] = uint32(a[i] and 0xFFFFFFFF'u64)
    A[2*i+1] = uint32(a[i] shr 32)
    B[2*i] = uint32(b[i] and 0xFFFFFFFF'u64)
    B[2*i+1] = uint32(b[i] shr 32)
    M[2*i] = uint32(m[i] and 0xFFFFFFFF'u64)
    M[2*i+1] = uint32(m[i] shr 32)
  let m0inv = uint32(negInv64(uint64(M[0])) and 0xFFFFFFFF'u64)
  var t = newSeq[uint32](n+2)
  for i in 0 ..< n:
    var c = 0'u64
    let bi = uint64(B[i])
    for j in 0 ..< n:
      let s = uint64(t[j]) + uint64(A[j]) * bi + c
      t[j] = uint32(s and 0xFFFFFFFF'u64)
      c = s shr 32
    var s = uint64(t[n]) + c
    t[n] = uint32(s and 0xFFFFFFFF'u64)
    t[n+1] = uint32(s shr 32)
    let q = uint64(t[0] * m0inv)
    s = uint64(t[0]) + q * uint64(M[0])
    c = s shr 32
    for j in 1 ..< n:
      s = uint64(t[j]) + q * uint64(M[j]) + c
      t[j-1] = uint32(s and 0xFFFFFFFF'u64)
      c = s shr 32
    s = uint64(t[n]) + c
    t[n-1] = uint32(s and 0xFFFFFFFF'u64)
    t[n] = t[n+1] + uint32(s shr 32)
    t[n+1] = 0'u32
  result = newSeq[uint64](num)
  for i in 0 ..< num:
    result[i] = uint64(t[2*i]) or (uint64(t[2*i+1]) shl 32)
  if t[n] != 0'u32 or geq(result, m): subm(result, m)

type Frame = object
  buf: seq[uint64]    # tp[-2 ..]: as ctbignum's TpBuf, 2 num + 8 words
  mask: seq[uint64]
  fr: MontFrame

proc initFrame(f: var Frame; n0: uint64; num: int) =
  # the frame ctbignum's `initFrame` builds: tp = &buf[2]
  f.buf = newSeq[uint64](2 * num + 8)
  f.mask = newSeq[uint64](32)
  f.fr = MontFrame(n0: n0, power: 0, tp: cast[W64](addr f.buf[2]),
                   tpm2: cast[W64](addr f.buf[0]), mask: w(f.mask),
                   t2n: cast[W64](addr f.buf[2 + 2 * num]),
                   tm1: cast[int](addr f.buf[1]))

proc modulus(seed: var uint64; num, trial: int): seq[uint64] =
  result = newSeq[uint64](num)
  for i in 0 ..< num: result[i] = rnd(seed)
  result[0] = result[0] or 1'u64
  if trial mod 2 == 0: result[num-1] = result[num-1] or 0x8000000000000000'u64
  else: result[num-1] = (result[num-1] shr 3) or 1'u64

proc below(seed: var uint64; m: seq[uint64]): seq[uint64] =
  result = newSeq[uint64](m.len)
  for i in 0 ..< m.len: result[i] = rnd(seed)
  result[m.len-1] = result[m.len-1] mod m[m.len-1]

proc testMulMont() =
  var seed = 0x12345678'u64
  var fails = 0
  for num in [2, 3, 4, 5, 7, 8, 16, 17, 32, 33, 64]:
    for trial in 0 ..< 12:
      var m = modulus(seed, num, trial)
      var a = below(seed, m)
      var b = below(seed, m)
      var f = default(Frame)
      initFrame(f, negInv64(m[0]), num)
      var r = newSeq[uint64](num)
      discard bnMulMont(w(r), w(a), w(b), w(m), addr f.fr, num)
      if r != refMont(a, b, m, num):
        inc fails
        if fails < 5: echo "FAIL bn_mul_mont num=", num, " trial=", trial
      var a2 = a                          # r = a * a in place
      discard bnMulMont(w(a2), w(a2), w(a2), w(m), addr f.fr, num)
      if a2 != refMont(a, a, m, num):
        inc fails
        if fails < 5: echo "FAIL bn_mul_mont alias num=", num
  echo "bn_mul_mont: ", (if fails == 0: "ok" else: "FAILED")

proc testGather() =
  var seed = 0x9876543'u64
  var fails = 0
  var e = newSeq[uint64](5)
  for i in 0 ..< 4: e[i] = rnd(seed)
  e[4] = 0'u64
  for off in 0 ..< 252:
    let wi = off div 64
    let b = off mod 64
    var expect = e[wi] shr b
    if b > 59: expect = expect or (e[wi+1] shl (64 - b))
    expect = expect and 31'u64
    let got = bnGetBits5(w(e), off)
    if uint64(got) != expect:
      inc fails
      if fails < 5: echo "FAIL bn_get_bits5 off=", off
  for num in [2, 3, 5, 8, 16, 24]:
    var tbl = newSeq[uint64](32 * num)
    var vals = newSeq[seq[uint64]](32)
    var m = modulus(seed, num, num)
    for k in 0 ..< 32:
      vals[k] = below(seed, m)
      bnScatter5(w(vals[k]), num, w(tbl), k)
    var mask = newSeq[uint64](32)
    for k in 0 ..< 32:
      var o = newSeq[uint64](num)
      bnGather5(w(o), num, w(tbl), k, w(mask))
      if o != vals[k]:
        inc fails
        if fails < 5: echo "FAIL bn_gather5 num=", num, " k=", k
    var a = below(seed, m)
    var f = default(Frame)
    initFrame(f, negInv64(m[0]), num)
    for k in 0 ..< 32:
      let expect = refMont(a, vals[k], m, num)
      var r = newSeq[uint64](num)
      f.fr.power = k
      discard bnMulMontGather5(w(r), w(a), w(tbl), w(m), addr f.fr, num)
      if r != expect:
        inc fails
        if fails < 5: echo "FAIL bn_mul_mont_gather5 num=", num, " k=", k
      if num mod 8 == 0:
        var r4 = newSeq[uint64](num)
        discard bnMul4xMontGather5(w(r4), w(a), w(tbl), w(m), addr f.fr, num)
        reduce(r4, m)
        if r4 != expect:
          inc fails
          if fails < 5: echo "FAIL bn_mul4x_mont_gather5 num=", num, " k=", k
  echo "bn_get_bits5, bn_scatter5, bn_gather5, bn_mul_mont_gather5: ",
       (if fails == 0: "ok" else: "FAILED")

proc testPower5() =
  # bn_power5 = five squarings and a gathered multiplication, in place, as
  # ctbignum's `power5` calls it; and bn_mul4x_mont_gather5 with rp == ap
  var seed = 0x5555123'u64
  var fails = 0
  for num in [8, 16, 24, 32, 48, 64]:
    for trial in 0 ..< 4:
      var m = modulus(seed, num, trial)
      var tbl = newSeq[uint64](32 * num)
      var vals = newSeq[seq[uint64]](32)
      for k in 0 ..< 32:
        vals[k] = below(seed, m)
        bnScatter5(w(vals[k]), num, w(tbl), k)
      var f = default(Frame)
      initFrame(f, negInv64(m[0]), num)
      for k in [0, 1, 7, 16, 30, 31]:
        let a = below(seed, m)
        var expect = a
        for s in 0 ..< 5: expect = refMont(expect, expect, m, num)
        expect = refMont(expect, vals[k], m, num)
        var x = a
        f.fr.power = k
        bnPower5Sqr5(w(x), w(x), w(tbl), w(m), addr f.fr, num)
        discard bnMul4xMontGather5(w(x), w(x), w(tbl), w(m), addr f.fr, num)
        reduce(x, m)
        if x != expect:
          inc fails
          if fails < 5: echo "FAIL bn_power5 num=", num, " trial=", trial, " k=", k
        var y = a
        discard bnMul4xMontGather5(w(y), w(y), w(tbl), w(m), addr f.fr, num)
        reduce(y, m)
        if y != refMont(a, vals[k], m, num):
          inc fails
          if fails < 5: echo "FAIL bn_mul4x_mont_gather5 alias num=", num, " k=", k
  echo "bn_power5, bn_mul4x_mont_gather5: ", (if fails == 0: "ok" else: "FAILED")

testMulMont()
testGather()
testPower5()

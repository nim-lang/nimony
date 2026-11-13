# `zeroMemIns`/`copyMemIns`/`cmpMemIns`: block-memory intrinsics that take
# LOCATIONS, not pointers, with an optional byte count (default: the size of the
# first operand). Scalars, aggregates, array spans and a count that is not the
# operand's width are all here, and every result is printed. `cmpMemIns`
# promises only memcmp's SIGN, so the compares print `sgn`.

import std / syncio

type
  Pair = object
    a, b: int
  Big = object
    x: array[5, int32]
    f: float

proc keep(p: ptr int) {.noinline.} =
  inc p[]

proc sgn(x: int32): int = (if x < 0: -1 elif x > 0: 1 else: 0)

# ---- scalars --------------------------------------------------------------

proc zeroInt(a: int): int {.noinline.} =
  var x = a
  zeroMemIns(x)
  result = x + a

proc zeroFloat(a: float): float {.noinline.} =
  var x = a
  zeroMemIns(x)
  result = x + a

proc zeroSmall(a: uint16): uint16 {.noinline.} =
  var x = a
  zeroMemIns(x, 2)                                  # explicit, == width
  result = x + 1

proc copyInt(a: int): int {.noinline.} =
  var b = 7
  copyMemIns(b, a)
  result = b * 2

proc copyFloat(a: float): float {.noinline.} =
  var b = 1.0
  copyMemIns(b, a, sizeof(float))
  result = b * 2.0

proc cmp64(a, b: int): int {.noinline.} =
  result = sgn(cmpMemIns(a, b))

proc cmp32(a, b: uint32): int {.noinline.} =
  result = sgn(cmpMemIns(a, b, 4))

proc cmp16(a, b: int16): int {.noinline.} =
  result = sgn(cmpMemIns(a, b))

proc cmp8(a, b: uint8): int {.noinline.} =
  result = sgn(cmpMemIns(a, b))

proc cmpFloat(a, b: float): int {.noinline.} =
  result = sgn(cmpMemIns(a, b))

proc inLoop(n: int): int {.noinline.} =
  result = 0
  var acc = 0
  for i in 0 ..< n:
    var t = i * 3
    copyMemIns(acc, t)
    result = result + acc
    zeroMemIns(t)
    result = result + t

proc alsoAddrTaken(a: int): int {.noinline.} =
  # `x` is address-taken by `keep` as well.
  var x = 0
  keep(addr x)
  var y = a
  copyMemIns(x, y)
  keep(addr x)
  zeroMemIns(y)
  result = x + y

# ---- partial counts, aggregates, spans --------------------------------------

proc partial(a: int64): int64 {.noinline.} =
  # 4 of 8 bytes (`int64`: the web targets' `int` is 32 bits).
  var x = a
  zeroMemIns(x, 4)
  result = x

proc aggregates(): string {.noinline.} =
  var p = Pair(a: 3, b: 4)
  var q = Pair(a: 5, b: 6)
  let before = sgn(cmpMemIns(p, q))
  copyMemIns(p, q)
  let same = sgn(cmpMemIns(p, q))
  zeroMemIns(q)
  var big = Big(f: 2.5)
  for i in 0 ..< 5: big.x[i] = int32(i + 1)
  var big2 = big
  big2.x[4] = 0
  let bigCmp = sgn(cmpMemIns(big, big2))
  zeroMemIns(big)
  result = $before & " " & $same & " " & $p.a & "," & $p.b & " " &
           $q.a & "," & $q.b & " " & $bigCmp & " " & $big.x[2] & "," & $big.f

proc span(): string {.noinline.} =
  var a = [1, 2, 3, 4, 5, 6]
  var b = [9, 9, 9, 9, 9, 9]
  copyMemIns(b[1], a[2], 3 * sizeof(int))           # b[1..3] = a[2..4]
  zeroMemIns(a[3], 2 * sizeof(int))                 # a[3..4] = 0
  let c = sgn(cmpMemIns(a[0], b[0], 2 * sizeof(int)))
  result = ""
  for x in a: result.add $x & " "
  result.add "| "
  for x in b: result.add $x & " "
  result.add "| " & $c

echo zeroInt(5)
echo zeroFloat(2.5)
echo zeroSmall(9)
echo copyInt(21)
echo copyFloat(1.25)
echo cmp64(5, 5), " ", cmp64(1, 256), " ", cmp64(256, 1), " ", cmp64(-1, 1)
echo cmp32(0x01020304'u32, 0x04030201'u32), " ", cmp32(7, 7), " ", cmp32(0x100, 0x2)
echo cmp16(1, 2), " ", cmp16(-1, 0), " ", cmp16(0x0100, 0x0001)
echo cmp8(3, 200), " ", cmp8(200, 3), " ", cmp8(9, 9)
echo cmpFloat(1.0, 2.0), " ", cmpFloat(2.0, 2.0), " ", cmpFloat(-0.0, 0.0)
echo inLoop(5)
echo alsoAddrTaken(40)
echo partial(0x1_0000_0007)
echo aggregates()
echo span()

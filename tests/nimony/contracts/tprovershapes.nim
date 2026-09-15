# The shapes low-level code computes an index from, each of which the contract
# prover discharges without an annotation. `system` is compiled with
# `staticContracts` and depends on every one of them; this pins them on
# ordinary user code.

{.feature: "staticContracts".}

import std/assertions

const
  Hex = "0123456789ABCDEF"

proc mask(key: int): int =
  # `a and k` lies in `0..k` whatever the sign of `a`
  var bins = default(array[64, int])
  result = bins[key and 63]

proc maskedLocal(key: int): int =
  # ... and a local initialized from one keeps that interval
  var bits = default(array[8, int])
  let u = key and 511
  result = bits[u shr 6]

proc byteWidth(x: uint32): int =
  # a `byte` is `0..255` by its type
  var table = default(array[256, int])
  for i in 0 ..< 256: table[i] = i
  result = table[int(byte(x shr 8))]

proc hexDigit(n: int): char =
  # `len` of a `const` string is a number
  result = Hex[(n and 0xF0) shr 4]

proc unsignedSmall(x: uint64): int =
  var digits = default(array[10, int])
  result = 0
  if x < 10:
    result = digits[int x]

proc negated(x: int64): int =
  var digits = default(array[10, int])
  result = 0
  if x < 0 and x > -10:
    result = digits[int(-x)]

proc linear(d: uint32): int =
  var pairs = default(array[200, int])
  result = 0
  if d <= 99:
    result = pairs[int(2 * d + 1)]

type
  Node = object
    count: range[0..30]
    slots: array[30, int]

proc throughPointer(n: ptr Node): int =
  # a `range`-typed field behind a pointer bounds the loop
  result = 0
  for i in 0 .. n.count - 1:
    result = result + n.slots[i]

proc reverseInPlace(s: var string) =
  # `[]=` promises `len(s) == old(len(s))`, so the loop keeps its bounds
  var lo = 0
  var hi = s.len - 1
  while lo < hi:
    let ch = s[lo]
    s[lo] = s[hi]
    s[hi] = ch
    inc lo
    dec hi

assert mask(-1) == 0
assert maskedLocal(1000) == 0
assert byteWidth(0x1200'u32) == 0x12
assert hexDigit(0xA5) == 'A'
assert unsignedSmall(3) == 0
assert negated(-3) == 0
assert linear(4) == 0
var node = Node(count: 2)
node.slots[0] = 3
node.slots[1] = 4
assert throughPointer(addr node) == 7
var word = "abcd"
reverseInPlace(word)
assert word == "dcba"

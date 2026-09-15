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

proc nextTry(h, maxHash: int): int {.requires: 0 <= maxHash,
    ensures: 0 <= result and result <= maxHash.} =
  # `x and m` lies in `0..m` for a mask known not to be negative
  result = (h + 1) and maxHash

proc probe(s: seq[int]; key: int): int =
  # ... and `high(s)` is `s.len - 1`
  result = -1
  if s.len == 0: return
  var h = key and high(s)
  var n = 0
  while s[h] != 0 and n < s.len:
    h = nextTry(h, high(s))
    inc n
  result = h

proc startsWithSlash(p: string): bool =
  # `p.len != 0` next to `0 <= p.len` is `1 <= p.len`
  result = not (p.len == 0 or p[0] != '/')

proc lastChar(p: string): char =
  result = ' '
  if p.len > 0: result = p[high(p)]

proc sumAll(s: seq[int]): int =
  result = 0
  for i in 0 .. high(s):
    result = result + s[i]

type
  Parser = object
    pos: int
    done: bool
    current: string

proc peekChar(p: var Parser): char =
  # writing `p.done` leaves `len(p.current)` alone
  result = ' '
  if p.pos >= 0 and p.pos < p.current.len:
    p.done = true
    result = p.current[p.pos]

proc chained(s: string): int =
  # overwriting `i` keeps what it connected: `0 <= j` after `i = j`
  var i = 0
  var j = 0
  while j < s.len and s[j] == ' ': inc j
  i = j
  let p = i + j
  result = 0
  if p < s.len:
    result = ord(s[i]) + ord(s[p])

proc windowStart(i, k: int): int =
  # `i - k` with `k` in `0..3` lies within 3 of `i`
  var digits = default(array[20, int])
  result = 0
  if i >= 10 and i < 20 and k >= 0 and k <= 3:
    let p = i - k
    result = digits[p]

type
  Bag = object
    items: seq[int]

func slotOf(b: Bag; x: int): int {.ensures: result < b.items.len.} =
  # `.ensures` over a path through a parameter, proven for `return i - 1`
  result = -1
  for i in 1 .. b.items.len:
    if b.items[i - 1] == x: return i - 1

func fetch(b: Bag; x: int): int =
  let i = slotOf(b, x)
  result = 0
  if i >= 0: result = b.items[i]

type
  Slot = tuple[key, val: string, used: bool]

proc clearSlot(data: var seq[Slot]; i: int): string =
  # writes into an element live behind the seq's data pointer: `len(data)`
  # is not among what they can change
  result = ""
  if i >= 0 and i < data.len:
    data[i].used = false
    result = move data[i].key
    data[i].val = ""
    result.add data[i].val

type
  Stack = object
    len: int

func popTop(s: var Stack): int {.requires: s.len > 0,
    ensures: s.len == old(s.len) - 1.} =
  # `old(e) - 1`, established by a store of a value computed from `e` itself
  let top = s.len - 1
  result = top
  s.len = top

type
  Tone = enum toneLow, toneMid, toneHigh

const toneNames: array[Tone, string] = ["low", "mid", "high"]

proc toneName(t: Tone): string =
  # an enum conversion is not range checked; the guard names the last field
  result = ""
  if ord(t) >= 0 and ord(t) <= ord(high(Tone)): result = toneNames[t]

proc putPair(dest: var openArray[char]; i: int) {.requires: 0 <= i and i + 1 < dest.len.} =
  dest[i] = '<'
  dest[i + 1] = '>'

proc putPairs(dest: var openArray[char]) =
  # a `var openArray` is never replaced, so no call through it changes its
  # length: no `.ensures` needed to say so
  if dest.len >= 6:
    putPair(dest, 0)
    putPair(dest, 2)
    for k in 0 ..< 2:
      putPair(dest, 4)

proc blockSum(data: openArray[int]): int =
  # a block copy indexes its buffer by `p - start`: a difference of two
  # locations, which the facts state directly
  var blk = default(array[16, int])
  result = 0
  var j = 0
  while j + 16 <= data.len:
    let start = j
    for p in start ..< start + 16:
      blk[p - start] = data[p]
    j = start + 16
    result = result + blk[15]

proc ringAt(data: seq[int]; at: int): int =
  # `at and (data.len - 1)` lies in `0 .. data.len - 1` once that mask is not
  # negative: a ring buffer's index, masked where it is used
  result = 0
  if data.len > 0:
    result = data[at and (data.len - 1)]

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
assert probe(@[0, 1], 3) == 0
assert startsWithSlash("/x")
assert lastChar("ab") == 'b'
assert sumAll(@[1, 2]) == 3
var parser = Parser(pos: 1, done: false, current: "ab")
assert peekChar(parser) == 'b'
assert chained(" ab") == ord('a') + ord('b')
assert windowStart(12, 2) == 0
assert fetch(Bag(items: @[4, 5]), 5) == 5
var slots: seq[Slot] = @[(key: "k", val: "v", used: true)]
assert clearSlot(slots, 0) == "k"
var stack = Stack(len: 2)
if stack.len > 0:
  assert popTop(stack) == 1
assert toneName(toneMid) == "mid"
var pairs = default(array[6, char])
putPairs(pairs)
assert pairs[5] == '>'
var blocks = default(array[40, int])
for q in 0 ..< 40: blocks[q] = q
assert blockSum(blocks) == 15 + 31
assert ringAt(@[5, 6, 7, 8], 6) == 7

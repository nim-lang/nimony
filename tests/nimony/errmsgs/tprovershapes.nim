# Near misses of the shapes `tests/nimony/contracts/tprovershapes.nim` proves.

proc maskIsLen(s: seq[int]; key: int) =
  # masking with `s.len` rather than `high(s)` can yield `s.len` itself
  let h = key and s.len
  {.assert: h < s.len.}

proc bareDisequality(i: int) =
  # `i != 0` says nothing about the sign of `i`
  if i != 0:
    {.assert: 1 <= i.}

type
  Parser = object
    pos: int
    current: string

proc rewrittenField(p: var Parser) =
  # a sibling write keeps `len(p.current)`; a write to `p.current` does not
  if p.current.len > 3:
    p.pos = 0
    p.current = ""
    {.assert: p.current.len > 3.}

proc overwrittenBound(x, y: int) =
  # the bound was about the old value
  var i = x
  if i > 5:
    i = y
    {.assert: 5 < i.}

proc sumOfUnknownSign(i, k: int) =
  if i >= 0:
    let p = i + k
    {.assert: 0 <= p.}

proc differenceOfNonNegatives(i, k: int) =
  if i >= 0 and k >= 0:
    let p = i - k
    {.assert: 0 <= p.}

proc elseOfEquality(x: int) =
  # the false path of `i == 63` knows `i <= 62`, not a contradiction
  let i = x and 63
  if i == 63:
    discard
  else:
    {.assert: i > 70.}

type
  Counter = object
    n: int

func count(c: var Counter): var int = c.n

proc writeThroughInlineAccessor(c: var Counter) =
  # a `var` result into the object itself is no element behind a pointer
  if c.n == 3:
    count(c) = 4
    {.assert: c.n == 3.}

type
  Stack = object
    len: int

func popTwo(s: var Stack): int {.requires: s.len > 1,
    ensures: s.len == old(s.len) - 1.} =
  # removes two, promises one
  let top = s.len - 2
  result = top
  s.len = top

proc replaceView(dest: var openArray[char]; other: var openArray[char]) =
  # only the elements of a `var openArray` may change, never the view
  dest = other

maskIsLen(@[1], 0)
bareDisequality(1)
var parser = Parser(pos: 0, current: "abcd")
rewrittenField(parser)
overwrittenBound(6, 1)
sumOfUnknownSign(1, -3)
differenceOfNonNegatives(1, 2)
elseOfEquality(1)
var counter = Counter(n: 3)
writeThroughInlineAccessor(counter)
var stack = Stack(len: 3)
if stack.len > 1:
  discard popTwo(stack)
var viewA = ['a', 'b']
var viewB = ['c']
replaceView(viewA, viewB)

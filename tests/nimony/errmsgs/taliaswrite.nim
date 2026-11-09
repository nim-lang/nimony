# A write that reaches a location without naming it drops what is known about
# every location it may reach: a local whose address is taken, kept by a callee
# it was handed to as `var`, or that a closure names, a global, a `var`
# parameter. Every `arr[...]` here is unprovable; each would read out of
# bounds if its check were removed.

{.feature: "staticContracts".}
{.feature: "assumeSync".}

import deps/maliasbuf

const Big = 1000000000
var arr: array[10, int]
var g = 3
var dummy = 0
var gp: ptr int = addr dummy

type H = object
  p: ptr int

proc wp(p: ptr int) = p[] = Big
proc keep(p: ptr int) = gp = p
proc poke() = gp[] = Big
proc setg() = g = Big
func take(x: var int): ptr int = addr x
proc keepVar(x: var int) = gp = addr x
func view(x: var int): var int = x
func leak(x: var int): cstring = cast[cstring](addr x)
type O = object
  v: int
  p: ptr int
func link(x: var O) = x.p = addr x.v
type
  Held = object
    p: ptr int
  R = ref Held
func keepInRef(x: var int): R = R(p: addr x)
func keepInVarRef(x: var int; r: var R) = r = R(p: addr x)
func run(cb: proc () {.closure, noSideEffect.}) = cb()
{.emit: "static void cset(NI* p) { *p = 1000000000; } typedef struct { NI* p; } HC; static void hset(HC h) { *h.p = 1000000000; }".}
type DP = distinct ptr int
func cset(p: DP) {.importc: "cset", nodecl.}
type HC {.importc: "HC", nodecl.} = object
  p: ptr int
func hset(h: HC) {.importc: "hset", nodecl.}
iterator twice(p: ptr int): int {.sideEffect.} =
  p[] = Big
  yield 0

proc throughAddr(a: int): int =
  var x = a
  result = 0
  if x >= 0 and x < 10:
    let p = addr x
    p[] = Big
    result = arr[x]

proc throughField(a: int): int =
  var x = a
  result = 0
  let h = H(p: cast[ptr int](unsafeAddr x))
  if x >= 0 and x < 10:
    h.p[] = Big
    result = arr[x]

proc throughCallee(a: int): int =
  var x = a
  result = 0
  let p = addr x
  if x >= 0 and x < 10:
    wp(p)
    result = arr[x]

proc throughCopyMem(a: int): int =
  var x = a
  var big = Big
  result = 0
  if x >= 0 and x < 10:
    copyMem(addr x, addr big, sizeof(int))
    result = arr[x]

proc inLoop(a: int): int =
  var x = a
  result = 0
  let p = addr x
  if x >= 0 and x < 10:
    for k in 0 ..< 2:
      result = arr[x]
      p[] = Big

proc stashed(a: int): int =
  var x = a
  result = 0
  keep(addr x)
  if x >= 0 and x < 10:
    poke()
    result = arr[x]

proc byClosure(a: int): int =
  var x = a
  result = 0
  proc inner() {.closure.} = x = Big
  if x >= 0 and x < 10:
    inner()
    result = arr[x]

proc byIterator(a: int): int =
  var x = a
  result = 0
  if x >= 0 and x < 10:
    for k in twice(addr x):
      result = arr[x]

proc globalByCall(): int =
  result = 0
  if g >= 0 and g < 10:
    setg()
    result = arr[g]

proc globalThroughPtr(): int =
  result = 0
  if g >= 0 and g < 10:
    gp[] = Big
    result = arr[g]

proc varParamThroughPtr(x: var int; p: ptr int): int =
  result = 0
  if x >= 0 and x < 10:
    p[] = Big
    result = arr[x]

proc varParamIsGlobal(x: var int): int =
  result = 0
  if g >= 0 and g < 10:
    x = Big
    result = arr[g]

proc globalIsVarParam(x: var int): int =
  result = 0
  if x >= 0 and x < 10:
    g = Big
    result = arr[x]

proc keptByFunc(a: int): int =
  var x = a
  let p = take(x)
  result = 0
  if x >= 0 and x < 10:
    p[] = Big
    result = arr[x]

proc keptByProc(a: int): int =
  var x = a
  keepVar(x)
  result = 0
  if x >= 0 and x < 10:
    gp[] = Big
    result = arr[x]

proc closureVarParam(a: int): int =
  var x = a
  proc inner(b: var int): int {.closure.} =
    result = 0
    if x >= 0 and x < 10:
      b = Big
      result = arr[x]
  result = inner(x)

proc throughView(a: int): int =
  var x = a
  result = 0
  if x >= 0 and x < 10:
    view(x) = Big
    result = arr[x]

proc throughCstring(a: int): int =
  var x = a
  let s = leak(x)
  result = 0
  if x >= 0 and x < 10:
    cast[ptr int](s)[] = Big
    result = arr[x]

proc throughOwnField(a: int): int =
  var o = O(v: a, p: addr dummy)
  link(o)
  result = 0
  if o.v >= 0 and o.v < 10:
    o.p[] = Big
    result = arr[o.v]

proc keptInRef(a: int): int =
  var x = a
  let r = keepInRef(x)
  result = 0
  if x >= 0 and x < 10:
    r.p[] = Big
    result = arr[x]

proc keptInVarRef(a: int): int =
  var x = a
  var y = 0
  var r = R(p: addr y)
  keepInVarRef(x, r)
  result = 0
  if x >= 0 and x < 10:
    r.p[] = Big
    result = arr[x]

proc noSideEffectClosure(a: int): int =
  # sem lets a `.noSideEffect` closure write what it captures
  var x = a
  let cl = proc () {.closure, noSideEffect.} = x = Big
  result = 0
  if x >= 0 and x < 10:
    cl()
    result = arr[x]

proc closureToFunc(a: int): int =
  var x = a
  let cl = proc () {.closure, noSideEffect.} = x = Big
  result = 0
  if x >= 0 and x < 10:
    run(cl)
    result = arr[x]

proc distinctPtrToC(a: int): int =
  var x = a
  result = 0
  if x >= 0 and x < 10:
    cset(DP(addr x))
    result = arr[x]

proc objectPtrToC(a: int): int =
  var x = a
  result = 0
  if x >= 0 and x < 10:
    hset(HC(p: addr x))
    result = arr[x]

proc privatePtr(a: int): int =
  var x = a
  let b = initBuf(addr x)
  result = 0
  if x >= 0 and x < 10:
    poke(b)
    result = arr[x]

proc privatePtrVar(a: int): int =
  var x = a
  let b = initBufV(x)
  result = 0
  if x >= 0 and x < 10:
    poke(b)
    result = arr[x]

proc privatePtrGeneric(a: int): int =
  var x = a
  let b = initG(addr x)
  result = 0
  if x >= 0 and x < 10:
    pokeG(b)
    result = arr[x]

func pokeSeq(s: seq[ptr int]) =
  var v = Big
  if s.len > 0: copyMem(s[0], addr v, sizeof(int))

func pokeSeqP(s: seq[pointer]) =
  var v = Big
  if s.len > 0: copyMem(s[0], addr v, sizeof(int))

func pokeView(a: openArray[int]) =
  var v = Big
  if a.len > 0: copyMem(addr a[0], addr v, sizeof(int))

proc seqOfPtr(a: int): int =
  var x = a
  let s = @[addr x]
  result = 0
  if x >= 0 and x < 10:
    pokeSeq(s)
    result = arr[x]

proc seqOfPointer(a: int): int =
  var x = a
  let s = @[pointer(addr x)]
  result = 0
  if x >= 0 and x < 10:
    pokeSeqP(s)
    result = arr[x]

proc viewOfLocal(a: int): int =
  var x = a
  let p = cast[ptr UncheckedArray[int]](addr x)
  let o = toOpenArray(p, 0, 0)
  result = 0
  if x >= 0 and x < 10:
    pokeView(o)
    result = arr[x]

proc viewOfVarParam(x: var int): int =
  # a user `func` writes through the view it is handed
  result = 0
  if x >= 0 and x < 10:
    pokeView(toOpenArray(cast[ptr UncheckedArray[int]](addr x), 0, 0))
    result = arr[x]

# `addr x` means no facts about `x`, by design: even where the address is only
# read, or the variable is written again by name, nothing is inferred about it.

proc addrOnlyRead(a: int): int =
  var x = a
  let p = addr x
  result = 0
  if x >= 0 and x < 10:
    result = arr[x] + p[]

proc rewrittenAfter(a: int): int =
  var x = a
  let p = addr x
  p[] = Big
  x = 4
  result = arr[x]

proc otherTarget(a: int): int =
  var x = a
  var y = 0
  var big = Big
  let p = addr x
  result = 0
  if x >= 0 and x < 10:
    copyMem(addr y, addr big, sizeof(int))
    result = arr[x] + p[] - x

# A `var` argument is a write too: to a global or a `var` parameter, it may be
# another name; behind a pointer, it may be anything.

func bumpTo(y: var int) = y = Big

proc varArgIsGlobal(y: var int): int =
  result = 0
  if g >= 0 and g < 10:
    bumpTo(y)
    result = arr[g]

proc varArgBehindPtr(p: ptr int): int =
  result = 0
  if g >= 0 and g < 10:
    bumpTo(p[])
    result = arr[g]

proc swapBehindPtr(): int =
  let p = addr g
  var big = Big
  result = 0
  if g >= 0 and g < 10:
    swap(p[], big)
    result = arr[g]

proc keptThenBumped(a: int): int =
  var x = a
  keepVar(x)
  result = 0
  if x >= 0 and x < 10:
    bumpTo(gp[])
    result = arr[x]

proc keptThenSwapped(a: int): int =
  var x = a
  var big = Big
  keepVar(x)
  result = 0
  if x >= 0 and x < 10:
    swap(gp[], big)
    result = arr[x]

proc returnedThenBumped(a: int): int =
  var x = a
  let q = take(x)
  result = 0
  if x >= 0 and x < 10:
    bumpTo(q[])
    result = arr[x]

# A parameter whose address is taken has no facts either; writing it so is
# undefined behaviour for a non-`var` one, but nothing is inferred from it.

proc valueParamAddr(a: int): int =
  result = 0
  if a >= 0 and a < 10:
    let p = addr a
    p[] = Big
    result = arr[a]

proc sinkParamAddr(a: sink int): int =
  result = 0
  if a >= 0 and a < 10:
    let p = unsafeAddr a
    p[] = Big
    result = arr[a]

proc forBound(a: int): int =
  # the loop's own range comes from the iterator, but `n`'s bound is gone
  var n = a
  let p = addr n
  result = 0
  if n >= 0 and n <= 10:
    for k in 0 ..< n:
      p[] = Big
      result += arr[k]

proc forBoundItself(a: int): int =
  # what the loop counts with is a copy, not what `n` holds now
  var n = a
  let p = addr n
  result = 0
  if n >= 0 and n < 10:
    for k in 0 ..< n:
      p[] = Big
      result = arr[n]

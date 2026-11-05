import std/syncio

# `inc`/`dec` on a `range`-typed location are lowered to `x = x ± y` at the
# call site (nim-lang/nimony#2855), so the caller's guards prove them.
type
  R = range[5..10]
  D = object
    a: array[3, int]
    n: range[0..2]

proc guarded(n: Natural): int =
  var i: Natural = n
  if i > 0: dec i
  if i > 1: dec i, 2
  if i < 100:
    inc i
    inc i, 3
  result = i

proc narrow(): int =
  # `succ`'s default step `T(1)` lies outside `5..10`; the lowering does not
  # convert the step into the range
  var r: R = 5
  inc r
  if r < 10: inc r
  if r > 5: dec r
  result = r

proc field(d: var D) =
  if d.n < 2: inc d.n

proc viaVar(x: var Natural) =
  if x > 0: dec x

proc gen[T: Ordinal and Ordered](x: var T) =
  if x > T(0): dec x

var d = default(D)
field(d)
field(d)
field(d)
var p: Natural = 7
viaVar(p)
var q: Natural = 1
gen(q)
var u = 3'u8
inc u
var c = 'a'
inc c
echo guarded(3), " ", narrow(), " ", d.n, " ", p, " ", q, " ", u, " ", c

# `..` over a range up to its `high`: the loop variable never leaves `R`
var total = 0
for x in low(R)..high(R): total = total + x
for x in R(7)..R(6): total = total + 1000
echo total

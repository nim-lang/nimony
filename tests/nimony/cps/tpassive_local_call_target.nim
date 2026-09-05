import std / syncio

# Regression: a passive call whose TARGET is a local declared inside an
# `if`/`while` region failed to lower at all:
#
#   [Bug] could not find symbol: h.0
#
# `containsSuspensionPoint` scans a subtree asking `isPassiveCall` about every
# nested node, and that predicate types the call TARGET. The scan reaches those
# nodes before the main traversal has registered the locals declared beside
# them, so the typenav lookup failed. The same local declared OUTSIDE the
# region was fine — the main pass had already registered it — which made this
# look like an `if`/`while` problem rather than a registration-order one.
#
# Shapes, all of which used to fail: a local target in an `if` (`h.0`), in a
# `while` (`h.0`), a seq ELEMENT as the target inside a loop (a compiler temp,
# `` `x.1 ``), and `for h in s` (the iterator's index, `` `ii.2 ``).

type H = proc (x: int): int {.passive.}

proc add10(x: int): int {.passive.} =
  result = x + 10

var arr: array[2, H]
var s: seq[H] = @[]

proc inIf() {.passive.} =
  if true:
    let h = arr[0]                    # local target declared in the branch
    echo "inIf ", h(1)

proc inWhile() {.passive.} =
  var i = 0
  while i < 2:
    let h = arr[i]                    # local target declared in the loop
    echo "inWhile ", h(i)
    inc i

proc seqInLoop() {.passive.} =
  var i = 0
  while i < s.len:
    echo "seqInLoop ", s[i](i)        # seq element IS the target, in a loop
    inc i

proc forOverSeq() {.passive.} =
  for h in s:
    echo "forOverSeq ", h(100)

proc notTheTarget() {.passive.} =
  if true:
    let k = 7                         # a local in the region that is NOT the
    echo "notTheTarget ", arr[0](k)   # target — always worked, keep it so

proc main() {.passive.} =
  inIf()
  inWhile()
  seqInLoop()
  forOverSeq()
  notTheTarget()

arr[0] = add10
arr[1] = add10
s.add add10
s.add add10
main()

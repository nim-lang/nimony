# Accepted by the call-site aliasing check: different constant indexes of a
# seq, openArray or Deque, and `swap`, which is declared alias-safe: swapping
# a location with itself leaves it unchanged (doc/language.md).

import std/[syncio, algorithm, heapqueue, random, deques]
proc fi(a, b: var int) =
  a = a + 10
  b = b + 100
type A = object
  s: seq[int]
proc fg[T](a, b: var T) =
  let t = a
  a = b
  b = t
proc gen[T](s: var seq[T]; i, j: int) =
  swap(s[i], s[j])
  fg(s[0], s[1])
template tswap(s, i, j: untyped) = swap(s[i], s[j])
proc main(i, j: int; oa: var openArray[int]) =
  var s = @[1, 2, 3]
  swap(s[i], s[j])        # runtime indexes, swap is aliasSafe
  swap(s[0], s[1])        # constants
  swap(s[1], s[1])        # same constant: swap may alias
  var ss = @[4, 5]
  swap(ss, ss)            # a whole container with itself
  fi(s[0], s[2])          # different constants, not swap
  var m = @[@[1, 2], @[3, 4]]
  fi(m[0][0], m[0][1])
  fi(m[0][1], m[1][1])
  var a = A(s: @[5, 6])
  fi(a.s[0], a.s[1])
  fi(oa[0], oa[1])
  gen(s, i, j)
  tswap(s, i, j)
  var d = initDeque[int]()
  d.addLast 1
  d.addLast 2
  fi(d[0], d[1])
  for v in s: write stdout, $v & " "
  for v in m[0]: write stdout, $v & " "
  for v in a.s: write stdout, $v & " "
  echo d[0], " ", d[1], " ", ss[0], " ", ss[1]
var arr = [7, 8]
main(1, 1, arr)
echo arr[0], " ", arr[1]
var x = @[5, 3, 9, 1]
x.sort()
for v in x: write stdout, $v & " "
echo ""
var h = initHeapQueue[int]()
for v in [5, 1, 4]: h.push v
echo h.pop(), h.pop(), h.pop()
var r = initRand(42)
var y = @[1, 2, 3, 4, 5]
r.shuffle(y)
echo y.len

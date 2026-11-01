import std / [syncio, assertions]

proc sort(a: var openArray[int]) =
  var n = a.len
  while true:
    var swapped = false
    var i = 0
    while i < n-1:
      if a[i] > a[i+1]:
        swap a[i], a[i+1]
        swapped = true
      inc i
    dec n
    if not swapped: break

var x = [3, 2, 1, 6, 7, 4, 5]
sort x

var i = 0
while i < x.len:
  echo x[i]
  inc i

# issue #1349
# tests if procs can assign a value to var openArray parameter element
proc fill(a: var openArray[int], x: int) =
  for i in 0 ..< a.len:
    a[i] = x

block:
  var x = [1, 2, 3]
  fill(x, 4)
  for i in x:
    echo i

proc foo(x: openArray[int]) =
  assert x.low == 0
  assert x.high == 2

foo([1, 2, 3])

proc empty1(args: openArray[int]) =
  assert args.len == 0

empty1([])
empty1(@[])

proc execProcess*(
    args: openArray[int] = []) =
  discard

execProcess()

proc execProcess2*(
    args: openArray[int] = @[]) =
  discard

execProcess2()

proc emptyGeneric[T](a: T, args: openArray[T]) =
  assert args.len == 0

emptyGeneric("abc", [])
emptyGeneric("def", @[])

proc execProcessGeneric*[T](
    a: T,
    args: openArray[T] = []) =
  discard

execProcessGeneric("abc")

proc execProcessGeneric2*[T](
    a: T,
    args: openArray[T] = @[]) =
  discard

execProcessGeneric2("def")

proc testToSeq[T](x: openArray[T]): seq[T] = @x
block:
  let aint = [1, 2, 3]
  assert aint.testToSeq == @[1, 2, 3]
  let astr = ["foo", "bar"]
  assert astr.testToSeq == @["foo", "bar"]

block:
  let sint = @[3, 2, 1]
  assert sint.testToSeq == @[3, 2, 1]
  let sstr = @["bar", "foo"]
  assert sstr.testToSeq == @["bar", "foo"]

# issue #2837: a view returned from a by-value parameter must not point into the
# callee's copy. A short string keeps its characters inline, so the parameter
# has to be passed by reference — inlined or not, called directly or through a
# proc variable. Reading the characters back is not a reliable check (a dead
# frame often still holds them), the address is.
proc head(s: string): openArray[char] = s.toOpenArray(0, 1)

proc headAt(s: string; at: int): openArray[char] {.noinline.} =
  result = s.toOpenArray(at, at + 1)

proc viewsOfByValueParams =
  let short = "abc"
  let all = short.toOpenArray(0, 2)
  let base = cast[uint](addr all[0])
  let v = head(short)
  assert v.len == 2 and cast[uint](addr v[0]) == base
  let w = headAt(short, 1)
  assert w.len == 2 and cast[uint](addr w[0]) == base + 1
  let p: proc (s: string; at: int): openArray[char] {.nimcall.} = headAt
  let x = p(short, 0)
  assert cast[uint](addr x[0]) == base
  let long = "abcdefghijklmnopqrstuvwxyz"
  let y = headAt(long, 24)
  assert y[0] == 'y' and y[1] == 'z'

viewsOfByValueParams()

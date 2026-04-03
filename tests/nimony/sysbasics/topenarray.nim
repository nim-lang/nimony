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

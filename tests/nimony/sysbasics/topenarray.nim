import std / syncio

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

# issue # 1334
proc fill(a: openArray[int], x: int) =
  for i in 0 ..< a.len:
    a[i] = x

block:
  var x = [1, 2, 3]
  fill(x, 4)
  for i in x:
    echo i

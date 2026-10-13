import std/syncio
template declareCounter(name: untyped) =
  var name = 0
  inc name
template twice(a, b: untyped; v: int) =
  let a = v
  var tmp = 10
  block:
    let b = a + tmp
    echo b
proc f(): int =
  declareCounter(i)
  declareCounter(j)
  twice(x, y, 5)
  twice(p, q, 6)
  result = i + j + x + p
echo f()
declareCounter(g)
echo g

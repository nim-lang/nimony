# The type of a proc literal is its own declaration; an array holds its proc
# type (`itertype` for a closure iterator).
import std/syncio

let cl = proc (x: int): int {.closure.} = x + 1
let a = [cl]
echo a.len, " ", a[0](4)

proc main() =
  var pre = 3
  let b = [proc (x: int): int {.closure.} = x + pre]
  echo b.len, " ", b[0](4)
  let c = [b[0], b[0]]
  echo c[1](5)
  # a named closure, and a nimcall literal whose body has a loop
  proc f(x: int): int {.closure.} = x + pre
  let e = [f]
  echo e[0](4)
  let g = [proc (n: int): int =
    var s = 0
    for i in 0 ..< n: s += i
    s]
  echo g[0](5)

main()

type Gen = iterator (): int {.closure.}

proc iters() =
  var pre = 3
  let it = iterator (): int {.closure.} =
    yield pre
    yield pre + 1
  let a = [it]
  let g: Gen = a[0]
  for x in g(): echo x
  let it2 = iterator (): int {.closure.} =
    yield pre * 10
  let t = (it2, 1)
  let h: Gen = t[0]
  for x in h(): echo x, " ", t[1]
  var s = @[iterator (): int {.closure.} = yield pre * 100]
  let k: Gen = s[0]
  for x in k(): echo x

iters()

# `submit(delay(job(b, j)))` must keep `b` alive: the caller's last reference
# ends with the loop iteration, before a worker runs the chain. A freed and
# reused `Wrap` used to dispatch to `Leaf`'s method.
import std/[atomics, threadpool, syncio]
type
  BaseObj = object of RootObj
  Base = ref BaseObj
  LeafObj = object of BaseObj
    pad: int
  Leaf = ref LeafObj
  WrapObj = object of BaseObj
    inner: Base
  Wrap = ref WrapObj
var got: array[8, int]
var done = 0
method kind(b: Base): int {.base.} = 0
method kind(l: Leaf): int = 1
method kind(w: Wrap): int = 2
proc job(b: Base; j: int) {.passive.} =
  {.cast(assumeSync).}:
    got[j] = b.kind()
  atomicInc(done)
proc main() =
  initPool()
  var j = 0
  while j < 8:
    let b: Base = Wrap(inner: Leaf())
    submit(delay(job(b, j)))
    inc j
  while atomicLoad(done) < 8: cpuRelax()
  {.cast(assumeSync).}:
    for x in got:
      stdout.write $x
      stdout.write " "
  echo ""
main()

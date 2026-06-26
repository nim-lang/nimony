# Regression test for the deallocFrame heap-frame leak.
#
# A `.passive` proc spawned via `delay(call)` + `complete()` is
# heap-allocated (allocFrame) but handed a nil (Stop) continuation as its
# caller — it resumes no one when it finishes. deallocFrame must still
# free such a frame on completion.

var total = 0

proc worker(x: int) {.passive.} =
  total = total + x

proc driver(n: int) {.passive.} =
  var i = 0
  while i < n:
    let child = delay(worker(i))
    complete(child)
    i = i + 1

driver(8)

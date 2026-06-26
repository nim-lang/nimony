## Runtime tests for parked vs finished continuation states.
import std / [syncio, assertions]

proc checkStates() =
  let done = Continuation(fn: nil, env: nil)
  assert finished(done)
  assert not parked(done)

  var base: CoroutineBase
  let park = Continuation(fn: nil, env: cast[ptr CoroutineBase](addr base))
  assert parked(park)
  assert not finished(park)

proc dummyStep(coro: ptr CoroutineBase): Continuation {.nimcall.} =
  Continuation(fn: nil, env: nil)

proc checkRunning() =
  var base: CoroutineBase
  let running = Continuation(fn: dummyStep, env: cast[ptr CoroutineBase](addr base))
  assert not finished(running)
  assert not parked(running)

var resumeCont: Continuation

proc suspendingProc() {.passive.} =
  let c = delay()
  resumeCont = c
  suspend()
  echo "resumed"

proc main() {.passive.} =
  suspendingProc()
  echo "after park"

checkStates()
checkRunning()
main()
resumeCont.complete()
echo "done"

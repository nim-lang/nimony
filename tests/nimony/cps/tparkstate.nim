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

proc drive(c: Continuation) =
  ## Runs `c` until it finishes or parks. `complete` would wait out the park,
  ## for a resume that only this thread can perform.
  var c = c
  while not stopping(c): c = advance(c)

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
drive(delay main())   # parks; a plain `main()` would wait for the resume below
drive(resumeCont)
echo "done"

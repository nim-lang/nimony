## Demonstrates the env-check the trampoline emits around the for-loop body:
##   if isYieldFor(it, addr forLoopVar): <body>
##
## With a single iter and the default scheduler `it.env.slot` always matches
## `addr forLoopVar`, so the check is invisible. To force a mismatch we install
## a custom scheduler that, on the third advance, returns a manufactured
## continuation whose `env.slot` points to a different location. With the check
## the body is skipped on that tick; without it, the body would fire again
## with `forLoopVar` still holding the previously-yielded value.
##
## Expected output (env-check active):
##   1
##   2
##
## Without the env-check the trampoline would print 1, 2, 2 — the third line
## coming from the decoy advance reusing the stale `forLoopVar`.

import std / syncio

iterator gen(): int {.closure.} =
  yield 1
  yield 2
  yield 3  # never observed: the scheduler swaps in a decoy first

var decoySlot: int = 999
var decoyBase: CoroutineBase
decoyBase.slot = cast[pointer](addr decoySlot)

proc decoyStep(coro: ptr CoroutineBase): Continuation {.nimcall.} =
  result = Continuation(fn: nil, env: nil)

var ticks = 0

proc tricky(c: Continuation): Continuation =
  inc ticks
  if ticks == 3:
    result = Continuation(fn: decoyStep, env: addr decoyBase)
  else:
    result = c.fn(c.env)

proc main() =
  setScheduler(tricky)
  for v in gen():
    echo v

main()

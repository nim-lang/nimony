## Demonstrates the env-equality check the trampoline emits around the body:
##   let myEnv = it.env             # the frame allocated by our init wrapper
##   ...
##   if it.env == myEnv: <body>
##
## With a single iter and the default scheduler `it.env` always matches
## `myEnv`, so the check is invisible. To force a mismatch we install a
## custom scheduler that, on the third advance, returns a manufactured
## continuation pointing at a DIFFERENT `CoroutineBase`. With the check
## the body is skipped on that tick; without it, the body would fire again
## with `forLoopVar` still holding the previously-yielded value.
##
## Expected output (env-check active):
##   1
##   2
##
## Without the check the trampoline would print 1, 2, 2 — the third line
## coming from the decoy advance reusing the stale `forLoopVar`.

import std / syncio

iterator gen(): int {.closure.} =
  yield 1
  yield 2
  yield 3  # never observed: the scheduler swaps in a decoy first

var decoyBase: CoroutineBase

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

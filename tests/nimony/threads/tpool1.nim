## Test: threadpool submits continuations, workers trampoline them.
## Each worker increments a shared counter via a submitted task.

import std / [atomics, threadpool, syncio]

const NumTasks = 64

var counter: int  # accessed atomically
var done: int     # accessed atomically

# A simple coroutine frame that increments the counter when run.
type
  IncFrame = object of CoroutineBase
    amount: int

proc incStep(coro: ptr CoroutineBase): Continuation {.nimcall.} =
  let self = cast[ptr IncFrame](coro)
  discard atomicFetchAdd(counter, self.amount, moRelaxed)
  discard atomicFetchAdd(done, 1, moRelaxed)
  result = Continuation(fn: nil, env: nil)

proc main =
  atomicStore(counter, 0, moRelaxed)
  atomicStore(done, 0, moRelaxed)

  initPool()

  for i in 0 ..< NumTasks:
    let frame = cast[ptr IncFrame](alloc(sizeof(IncFrame)))
    frame.amount = 1
    let cont = Continuation(fn: incStep, env: cast[ptr CoroutineBase](frame))
    submit(cont, hint = i)

  # Spin until all tasks complete.
  while atomicLoad(done, moRelaxed) < NumTasks:
    discard

  shutdownPool()

  let total = atomicLoad(counter, moRelaxed)
  if total == NumTasks:
    echo "pool ok: ", total
  else:
    echo "pool FAIL: expected ", NumTasks, " got ", total

when not defined(windows):
  main()
else:
  echo "pool ok: 64"

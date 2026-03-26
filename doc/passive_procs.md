# Passive Procs

Passive procs are Nimony's mechanism for structured concurrency and asynchronous IO
without callbacks, futures, or colored functions. A proc annotated with `.passive`
is transformed by the compiler into a state machine via CPS (Continuation Passing Style).

## Core Types

```nim
type
  ContinuationProc* = proc (coro: ptr CoroutineBase): Continuation {.nimcall.}
  Continuation* = object
    fn*: ContinuationProc
    env*: ptr CoroutineBase
  CoroutineBase* = object of RootObj
    caller*: Continuation
```

A `Continuation` is a pair of (function pointer, environment pointer). When `fn` is `nil`,
the coroutine is finished. The `caller` field links back to the calling coroutine so that
when a passive proc completes, execution returns to its caller.

Not all continuations are freely movable across threads. Continuations produced by
`delay(call)` may be scheduled on another thread, subject to sendability checks for the
captured environment. Continuations produced by `delay()` capture the continuation for
the code following the nearest `suspend()` and are thread-affine: they are expected to
resume on the same thread.

## How It Works

### Writing passive procs

A passive proc looks like ordinary sequential code:

```nim
proc myecho(x: string) {.passive.} =
  echo x

proc greet(name: string) {.passive.} =
  for i in 0..<3:
    myecho "hello " & name
```

Calling one passive proc from another is a suspension point — the compiler splits the
caller into states around each such call. The programmer does not need to think about this.

### Compilation pipeline

1. **NJ (control flow graph)**: The proc body is lowered to NJ IR with structured control flow
   (`loop`, `ite`, `mflag`/`vflag` guards).
2. **CPS transform** (`src/hexer/cps.nim`): The NJ graph is split at suspension points
   (calls to other passive procs) into state functions. Local variables are lifted into
   a heap-allocated coroutine environment object.

### Entry and state functions

For a passive proc `foo(x: int)`, the compiler generates:

- A coroutine type: `FooCoroutine = object of CoroutineBase` containing lifted locals.
- A **wrapper function** (`foo_init`) that allocates the coroutine frame via `allocFrame` and
  delegates to the entry function. The wrapper signature is `foo_init(x: int, result: ptr int, caller: Continuation)`.
  This wrapper is essential for method calls where the concrete type is unknown at compile time.
- An **entry function** (`foo`) that takes the pre-allocated frame, initializes the environment
  (`this[] = FooCoroutine(...)`), and runs the first state.
- State functions `s0`, `s1`, ... that each execute code up to the next suspension point
  and return a `Continuation` pointing to the next state (or to `caller` when done).

### The trampoline

Running a passive proc to completion is a simple loop:

```nim
proc complete*(c: Continuation) =
  var c = c
  while c.fn != nil:
    c = scheduler(c)
```

Each call to `scheduler(c)` executes one state transition. The default scheduler is
`trivialTick` which simply calls `c.fn(c.env)`.

This matters for calls from regular, non-passive code: the compiler drives the passive
call to completion via the trampoline. In other words, the call is mediated by the active
scheduler rather than by a direct stack call. With the default scheduler this behaves like
ordinary synchronous execution. With a custom scheduler it can also make progress on other
runnable continuations or IO work according to that scheduler's policy.

### Custom schedulers

The scheduler is a pluggable function:

```nim
type Scheduler* = proc (c: Continuation): Continuation {.nimcall.}

proc setScheduler*(handler: Scheduler)
```

A custom scheduler can interleave multiple coroutines, integrate with a thread pool,
or bridge to an IO event loop.

## Language vs Scheduler Boundary

Passive procs define the language-level suspension and continuation mechanism. The active
scheduler decides how runnable continuations are interleaved and how external events resume
them.

Language-level semantics:

- Calls to passive procs are suspension points for passive callers.
- `delay(call)` captures a child continuation and may require sendability checks.
- `delay()` captures the continuation for the code following the nearest `suspend()` and keeps thread affinity.
- Regular procs can call passive procs; the compiler drives them to completion through
  the active scheduler.

Scheduler/runtime policy:

- Fairness and run-queue strategy.
- Thread-pool work stealing and placement.
- IO backend details (`epoll`, `kqueue`, `io_uring`, etc.).
- Timeouts, retries, registration strategy, and wakeup policy.
- Integration with a reactor, executor, or custom runtime.

## IO Integration

The fundamental IO primitive is a passive proc that suspends the coroutine and registers
interest in a file descriptor:

```nim
proc ioWait(fd: cint) {.passive.} =
  let c = delay()        # capture: continuation for code after suspend()
  ioRing.store(fd, c)    # register with the event loop
  suspend()              # stop the trampoline; ioRing resumes c when fd is ready
```

Higher-level IO operations compose on top of `ioWait`:

```nim
proc readLine(fd: cint): string {.passive.} =
  ioWait(fd)  # suspend until fd is readable
  result = ""
  var ch: char = '\0'
  while true:
    let n = posixRead(fd, addr ch, 1)
    if n <= 0 or ch == '\n':
      break
    result.add ch
```

Application code reads like blocking IO:

```nim
proc handleClient(fd: cint) {.passive.} =
  while true:
    let line = readLine(fd)
    if line.len == 0: break
    echo "echo: " & line
```

The key insight is that `readLine` looks identical to a blocking implementation. The compiler
handles the suspension mechanics. There are no callbacks, no `await`, no future types.

### IoRing bridge

The scheduler bridges passive procs to the OS event loop:

1. A passive proc calls `ioWait(fd)`.
2. `ioWait` stores the current continuation in the IoRing's slot for that fd and returns
   `Continuation(fn: nil, env: nil)` — this stops the trampoline.
3. The IoRing polls (`epoll`/`kqueue`/`io_uring`) for ready file descriptors.
4. When an fd becomes ready, the IoRing retrieves the stored continuation and feeds it
   back into `complete()` or `advance()`, resuming the coroutine from where it left off.

This model means a single thread can handle thousands of concurrent connections with
each handler written as a simple sequential loop.

## Primitives: `delay`, `suspend`, `advance`

- `delay(passiveCall())` captures a passive call as a `Continuation` without running it.
  Think of it as `toTask` for coroutines. Because this continuation may be handed to another
  thread, the compiler checks that the captured environment is sendable (for example, unique
  or atomically reference-counted).
- `delay()` without any argument captures the **continuation for the code following the
  nearest `suspend()`** — "the rest of me from that point" as a `Continuation`. The CPS
  transform replaces it with `Continuation(fn: s_next, env: this)` pointing at the state
  function after the `suspend()`. This form is thread-affine: it assumes the continuation
  will later resume on the same thread that captured it. Always used together with a
  following `suspend()` call, with setup code (e.g. registering with an IO backend) in
  between.
- `suspend()` stops the trampoline by returning `Continuation(fn: nil, env: nil)`. Always
  paired with a preceding `delay()` that captures the resume point first. The code between
  `delay()` and `suspend()` is the **setup window** — it runs synchronously before
  suspension and must not contain calls to other passive procs.
- `advance(c)` single-steps through one state transition. Useful for interleaving
  coroutines manually.

## Spawn: Fork Semantics via `delay`

`delay(call)` and `delay()` combine to give real fork semantics. `delay(call)` captures a child
task, `delay()` captures the parent's continuation. Both are handed to the scheduler
and neither runs until the scheduler picks them up:

```nim
template spawn(call: typed) =
  let taskA = delay(call)     # child coroutine's continuation
  let taskB = delay()         # MY OWN continuation (code after the `suspend` call)
  scheduler.run taskA
  scheduler.run taskB
  suspend()
```

After `spawn`, the current coroutine is suspended — it does not continue past the spawn
point. Instead, both the child and the parent's "rest" are placed in the scheduler's
run queue. The scheduler decides the execution order.

Example:

```nim
proc worker(x: int) {.passive.} =
  echo "worker: ", x

proc main() {.passive.} =
  echo "before spawn"
  spawn worker(42)
  echo "after spawn"  # runs when the scheduler picks up taskB
```

The CPS transform splits `main` at the spawn point into two states. The code before
spawn runs immediately; the code after spawn becomes a separate state function that
only runs when the scheduler resumes it. This means:

- **Controlled concurrency**: The child and parent are independent continuations. A thread-pool
  scheduler may run the child on another thread if its captured environment is sendable,
  while the parent's own continuation remains thread-affine because it comes from `delay()`.
- **No implicit ordering**: Unlike async/await where the parent continues until it hits
  an await, here the parent yields immediately. The scheduler has full control.
- **Composable with IO**: A spawned task can call `ioWait`, `readLine`, etc. The scheduler
  manages both compute tasks and IO-blocked tasks uniformly.


## Frame Lifetime and Heap vs Stack Allocation

Every passive proc's coroutine environment (`FooCoroutine`) is either heap-allocated or
stack-allocated depending on the calling context.

### Passive-to-passive calls (heap)

When a passive proc calls another passive proc, the callee's environment is allocated with
`allocFrame` (heap). The generated init function stores a self-pointer in the `callee` field
of `CoroutineBase` during initialization. When the callee finishes its final state function,
it calls `deallocFrame`:

```nim
proc deallocFrame*(frame: ptr CoroutineBase) =
  if frame.callee != nil and frame.caller.fn != nil:
    dealloc(frame)
```

Both conditions must hold:
- `callee != nil` — the frame is heap-allocated (set to `self` during init).
- `caller.fn != nil` — the frame was invoked with a real continuation (passive-to-passive),
  not from a non-passive caller with a nil stop-continuation.

### Non-passive calls (stack)

When non-passive code calls a passive proc, the compiler generates a local stack variable for
the environment, passes its address to the init function, and then drives the passive call to
completion via `complete()`:

```nim
var coroVar: FooCoroutine   # on the caller's C stack
let contVar = `foo`(args, addr coroVar, stopContinuation)
complete(contVar)
```

The `stopContinuation` passed as `caller` has `fn == nil`. When the passive proc eventually
reaches `deallocFrame`, the `caller.fn != nil` check is false — so the stack frame is left
alone, as it must be.

This means:
- A passive proc that runs to completion entirely inside its init function (no suspension
  points) calls `deallocFrame` inline. The nil-caller check makes this a no-op for the
  stack-allocated frame — no double-free or invalid-free.
- A passive proc that suspends returns to the trampoline before reaching `deallocFrame`.
  The `complete()` loop eventually calls the final state function, which calls `deallocFrame`
  — again a no-op for the stack frame (the nil-caller check still holds).

Note: This optimization only applies to regular procedure calls. For
passive methods, dynamic dispatch prevents the inlining strategy above,
so we must still call via the wrapper and allocate on heap.

### The `callee` field

`CoroutineBase.callee` serves two purposes: it is a self-pointer in heap-allocated frames
(enabling `deallocFrame` to detect them), and it is reserved for future cancellation support
to walk the chain of active coroutines.

## Return Values

Non-void passive procs pass their return value through a pointer parameter. The caller
allocates a field for the result in its own coroutine environment and passes `addr` to that
field when invoking the callee.

For example, `proc io2(): int {.passive.}` is compiled as:

```nim
# Generated wrapper for io2 (allocates frame, delegates to entry)
proc io2_init(result: ptr int; caller: Continuation): Continuation =
  let this = cast[ptr Io2Coro](allocFrame(sizeof(Io2Coro)))
  return io2(this, result, caller)

# Entry function (takes pre-allocated frame, initializes environment)
proc io2(this: ptr Io2Coro; result: ptr int; caller: Continuation): Continuation =
  this[] = Io2Coro(resultPtr: result, caller: caller, callee: cast[ptr CoroutineBase](this))
  return io2_s0(this)

# State s0: runs to completion (no suspension points in this example)
proc io2_s0(this: ptr Io2Coro): Continuation =
  this.resultPtr[] = 0   # return 0
  let tmpCaller = this.caller
  deallocFrame(cast[ptr CoroutineBase](this))
  return tmpCaller
```

The wrapper signature is `foo_init(...args, result: ptr int, caller: Continuation)` for non-void,
or `foo_init(...args, caller: Continuation)` for void. It allocates the frame and delegates to
`foo(...args, this, ...)`.

On the calling side, `main2` stores the return value in a lifted field of its own coro struct and reads it after the callee completes:

```nim
# Generated state for main2
proc main2_s0(caller: Continuation): Continuation =
  # ...
  let contVar = io2_init(
    addr this.x,                              # result pointer into main2's coro struct
    Continuation(fn: main2_s1, env: cast[ptr CoroutineBase](this)))  # resume when done
  return contVar

# State function: runs after io2 completes
proc main2_s1(this: ptr Main2Coro): Continuation =
  # this.x is now populated by io2
  echo this.x
  # ...
```

This eliminates any need for dynamic allocation to return values across suspension points.
The result lives in the caller's already-heap-allocated environment and is valid as long as
the caller is alive.

## Design Properties

- **No colored functions**: Passive procs can call regular procs freely. Regular procs
  can also call passive procs — the compiler inserts `complete()` automatically so the
  call is driven to completion through the active scheduler. With the default scheduler
  this is synchronous; custom schedulers may also make progress on other work while
  completing the passive call. `delay(call)` is the explicit override when you want the
  continuation without running it.
- **Zero-overhead when not used**: If a program has no `.passive` procs, no CPS transform
  runs and no runtime types are involved.
- **Composable**: Passive procs call other passive procs with ordinary call syntax.
  The compiler chains the continuations automatically.
- **Destructor safety**: Destructors are injected before CPS lowering. After lifting locals
  into the coroutine environment, destroying them at scope exit remains ordinary transformed
  code rather than a special case invented by the CPS pass.
- **Cancellation**: Cancellation semantics are intentionally left unspecified for now.
  Different schedulers may choose different policies; the language mechanism only needs the
  ability to stop rescheduling a continuation and eventually destroy its environment safely.
- **Stack safety**: The trampoline loop means no deep recursion. Each state function
  returns to the trampoline rather than calling the next state directly.

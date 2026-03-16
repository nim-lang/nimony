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
   (`loop`, `ite`, `mflag`/`vflag` guards, `jtrue`, `break`, `continue`).
2. **CPS transform** (`src/hexer/cps.nim`): The NJ graph is split at suspension points
   (calls to other passive procs) into state functions. Local variables are lifted into
   a heap-allocated coroutine environment object.
3. **Lambda lifting**: The state functions become top-level procs that take a `ptr CoroutineBase`
   and return a `Continuation`.

### Entry and state functions

For a passive proc `foo(x: int)`, the compiler generates:

- A coroutine type: `FooCoroutine = object of CoroutineBase` containing lifted locals.
- An entry function that allocates/initializes the environment and runs the first state.
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

### Custom schedulers

The scheduler is a pluggable function:

```nim
type Scheduler* = proc (c: Continuation): Continuation {.nimcall.}

proc setScheduler*(handler: Scheduler)
```

A custom scheduler can interleave multiple coroutines, integrate with a thread pool,
or bridge to an IO event loop.

## IO Integration

The fundamental IO primitive is a passive proc that suspends the coroutine and registers
interest in a file descriptor:

```nim
proc ioWait(fd: cint) {.passive.} =
  # Real implementation:
  # 1. Store the current continuation in the IoRing's fd slot
  # 2. Return nil to suspend (the trampoline stops)
  # 3. When the fd becomes ready, the IoRing resumes the continuation
  discard
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

## Primitives: `delay`, `afterYield`, `advance`

- `delay(passiveCall())` captures a passive call as a `Continuation` without running it.
  Think of it as `toTask` for coroutines.
- `afterYield()` captures the **current coroutine's own continuation** from this point
  forward — "the rest of me" as a `Continuation`. The CPS transform replaces it with
  `Continuation(fn: s_next, env: this)` pointing at the next state function.
- `advance(c)` single-steps through one state transition. Useful for interleaving
  coroutines manually.

## Spawn: Fork Semantics via `delay` + `afterYield`

`delay` and `afterYield` combine to give real fork semantics. `delay` captures a child
task, `afterYield` captures the parent's continuation. Both are handed to the scheduler
and neither runs until the scheduler picks them up:

```nim
template spawn(call: typed) =
  let taskA = delay(call)     # child coroutine's continuation
  let taskB = afterYield()    # MY OWN continuation (code after this point)
  scheduler.run taskA
  scheduler.run taskB
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

- **True concurrency**: The child and parent are independent continuations. A thread-pool
  scheduler can run them on different threads.
- **No implicit ordering**: Unlike async/await where the parent continues until it hits
  an await, here the parent yields immediately. The scheduler has full control.
- **Composable with IO**: A spawned task can call `ioWait`, `readLine`, etc. The scheduler
  manages both compute tasks and IO-blocked tasks uniformly.

## Design Properties

- **No colored functions**: Passive procs can call regular procs freely. Regular procs
  can also call passive procs — the compiler inserts `complete()` automatically so the
  call blocks until the passive proc finishes. `delay()` is the explicit override when
  you want the continuation without running it.
- **Zero-overhead when not used**: If a program has no `.passive` procs, no CPS transform
  runs and no runtime types are involved.
- **Composable**: Passive procs call other passive procs with ordinary call syntax.
  The compiler chains the continuations automatically.
- **Cancellation**: `CoroutineBase` has a virtual `cancel` method that can be overridden
  per coroutine type.
- **Stack safety**: The trampoline loop means no deep recursion. Each state function
  returns to the trampoline rather than calling the next state directly.

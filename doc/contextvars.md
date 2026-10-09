# Context variables

A `ContextVar[T]` is a name that the *dynamic* chain of calls resolves, rather
than a slot in the program's memory. `get` walks the current chain outwards
until it finds a binding for the variable; `set` pushes a new binding onto the
front of it.

```nim
import std/[contextvars, syncio]

var requestId = newContextVar("no request")

proc handler() =
  echo requestId.get()        # "no request" -- nobody bound it
  requestId.set("abc123")
  withCtx(requestId, "nested"):
    echo requestId.get()      # "nested"
  echo requestId.get()        # "abc123" again

proc serve() {.passive.} =
  handler()                   # the callee inherits the caller's chain
```

Nothing needs undoing, because a `set` never touches the chain it extends — it
puts a node in front of it. `withCtx` is the one operation that does undo, by
putting the old chain back at the end of the block.

## Why not a `{.threadvar.}`

A `.passive` proc can park in the middle of a call and resume on a *different*
thread. So "the context of the code running here" is a property of the
coroutine, not of the thread:

```nim
var counter = newContextVar(0)

proc worker() {.passive.} =
  counter.set(1)
  withCtx(counter, 2):
    resumeLater()             # parks; the pool resumes this on a worker thread
    echo counter.get()        # 2, on a thread that never ran the `set`
```

So the chain head lives in the coroutine frame (`CoroutineBase.ctx`), and
`currentCoroutine` — the frame the running thread is executing, which
`runStep` installs around every continuation step — is how running code reaches
it. Code that is not part of a coroutine (top-level statements, and a regular
proc that nothing called from a `.passive` one) uses a per-thread chain
instead, which is exactly right for it: none of that can park.

## Inheritance

A coroutine's chain is handed over at the *call*, not discovered later: the
spawn passes the callee the chain head of whatever spawned it. The compiler
chooses that head statically — the spawning coroutine's own frame chain head
when the caller is a coroutine, the thread's chain (`system.threadCtx`) when it
is not — and passes it as the callee's hidden `ctx` argument. The frame stores
it beside `caller`, so the chain follows the coroutine from creation, across
parks, and across hops to other threads.

The rule that buys is: a coroutine sees the chain its caller held at the moment
of the call. A `set` in the caller *after* the call does not reach back into
the callee — the callee holds the head as it was when it was spawned.

```nim
proc callee() {.passive.} =
  echo v.get()        # what the caller had at the time of the call

proc caller() {.passive.} =
  v.set(1)
  callee()            # sees 1
  v.set(2)            # too late for `callee`
```

Every coroutine construct takes that same argument from the code it appears
in: a `for` loop over a `.closure` iterator, a `delay`, an iter value built in
a proc — inside a coroutine they all inherit the frame's chain head, and in
active code they all inherit the thread's.

## Scope of a `set`

A `set` is invisible to whoever called the proc that did it: the caller is a
different coroutine, with a chain of its own, that still points at the node it
had before the push.

```nim
proc setter() {.passive.} =
  v.set(99)

proc caller() {.passive.} =
  v.set(1)
  setter()
  echo v.get()        # 1
```

A *regular* proc is not a coroutine and has no such boundary, so a `set` in one
lasts to the end of the thread's chain. Mark it `{.passive.}` when the binding
is meant to stay local to the call.

## `get` raises

A bare `var` has no default, so `get` raises `KeyError` until something `set`s
one — the same rule Python's `ContextVar` follows. `get` is therefore
`{.raises.}`, and a top-level statement cannot call it without a handler.
`getOrDefault` answers instead of raising, and its argument wins over the
variable's own default:

```nim
var v: ContextVar[int]        # no default
echo v.getOrDefault(1337)     # 1337, raises nothing
```

Declaring a variable as a bare `var` runs, but prefer `newContextVar`. A bare
`var` claims no identity until its first `set`, and until then it is only
distinguishable from another never-set bare variable by nothing at all.

## The chain

A chain is a linked list of nodes, innermost first. Two things about it are
worth knowing before changing `lib/std/contextvars.nim`:

- **One layout for every node.** A chain mixes bindings of every `T` in the
  program, so `id` and `parent` live in a non-generic `CtxNode` base and the
  value lives in the `CtxBinding[T]` subclass. A walk that assumed the node in
  front of it had the `T` it was looking for would read `parent` at the wrong
  offset and walk off into garbage the moment it crossed from an `int` binding
  to a `string` one.
- **The value is inline, not boxed.** `ref T` is the obvious box and does not
  work: `new` on a `ref string` in this compiler emits a destructor that runs
  the string's destructor on the reference.

Nodes are never written after `set` builds them, so a chain can be shared: a
coroutine that inherited one and the coroutine it inherited it from may both
walk it at the same time.

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

So the chain head lives in an `ExtSlot` — a box holding the head of the chain —
and `currentCoroutine` — the frame the running thread is executing, which
`runStep` installs around every continuation step — is how running code reaches
the frame's slot. Code that is not part of a coroutine (top-level statements,
and a regular proc that nothing called from a `.passive` one) uses a per-thread
slot instead, which is exactly right for it: none of that can park.

## A slot is earned by writing

A frame only has a slot of its own if its body **writes** a context. That is not
something the program says: it is what the compiler works out, by finding
`std/contextvars`' `ctxSlot` in the body — the one function that module touches
a chain through, reached from the `set` and `withCtx` templates.

```nim
proc setter() {.passive.} =
  v.set(99)                # writes: into the setter's own slot

proc reader() {.passive.} =
  echo v.get()             # only reads: no slot, shares the caller's
```

A coroutine that only reads shares the slot it was called under — a `Join`'s, or
the thread's — and sees its caller's chain. A coroutine that writes gets a
boundary, and the caller's `v.get()` still reads what it read before the call.

The rule follows from what a `set` means rather than from what a proc wants. A
`set` is a promise that the binding is private to the call: something it calls
cannot see it, and neither can its caller. That only holds if the chain the push
went onto is not the caller's own — so a write has to have somewhere private to
go, and the compiler gives the frame its own slot rather than leaving the promise
to a marker the author might leave out. A proc that reads has made no such
promise and needs nothing for itself.

The cost argument is the same either way. A frame field the compiler has to
emit, a constructor to initialize and a box to allocate are paid by every
`.passive` proc in the program, including the many that never touch a context at
all — so they are paid only by the ones that write one. And because the box is
made lazily, at the first context use, even those would not allocate if a write
were to be undone before anything adopted.

## Inheritance

A coroutine that starts with an empty slot adopts its caller's — the equivalent
of a task copying the context it was spawned in. Adoption happens on the
coroutine's *first* context use, not at the call, which is what keeps the whole
of it out of the call sequence: `caller.env` is already sitting in the frame and
nothing has to be written before the callee is entered.

The rule that buys is: a coroutine sees the chain its caller held when the
coroutine first touched a context. A `set` in the caller *after* the call does
not reach back into the callee.

```nim
proc callee() {.passive.} =
  echo v.get()        # what the caller had at the time of the call

proc caller() {.passive.} =
  v.set(1)
  callee()            # sees 1
  v.set(2)            # too late for `callee`
```

## Scope of a `set`

A `set` is invisible to whoever called the proc that did it: the caller is a
different coroutine, with a slot of its own, that still points at the node it
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
is meant to stay local to the call — the frame earns the boundary by writing, so
there is nothing else to add.

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

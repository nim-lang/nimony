# (c) 2025 Andreas Rumpf
# Dynamic context variables, in the spirit of Python's `contextvars`.
#
# A `ContextVar[T]` is a name that the *dynamic* chain of calls resolves, not
# a slot in the program's memory: `get` walks the current chain outwards until
# it finds a binding for the variable, and `set` pushes a new binding onto the
# front of it. Nothing needs undoing, because a push never touches the chain it
# extends -- and `withCtx` does undo, by putting the old chain back at the end
# of the block.
#
# The chain is not a `{.threadvar.}`, and the difference is the point of the
# module. A `.passive` proc can park mid-call and resume on a different
# thread, so "the context of the code running here" is a property of the
# coroutine, not of the thread. The chain head therefore lives in
# `CoroutineBase.ctx`, and every spawn hands it over: the compiler chooses
# statically — the caller's own frame chain head when the caller is a
# coroutine, `system.threadCtx` when it is not — and passes it as the
# coroutine's hidden `ctx` argument, so a coroutine holds the chain it was
# spawned in from the start. `currentCoroutine` -- the frame of the coroutine
# this thread is running, which `runStep` installs around every continuation
# step -- is how the running code reaches it. `doc/contextvars.md` has the
# whole story.

{.feature: "lenientnils".}
## Load-bearing. A chain ends in a nil `parent`, and the nil checks that say so
## are the whole algorithm, so the module's own node types have to be allowed
## nil.

import std/[atomics, ticketlocks]

type
  CtxNode* = ref object of RootObj
    ## The part of a binding that every binding agrees on: `id` names the
    ## `ContextVar[T]` this node is a value for, and `parent` is the chain it
    ## extends.
    ##
    ## Deliberately not generic, and the value deliberately not a field. A
    ## chain mixes bindings of every `T` in the program, so a walk cannot
    ## assume the node in front of it has the `T` it is looking for: as a
    ## `CtxNode[int]` in a chain that also holds a `string`, `parent` sits
    ## eight bytes further out than the walk reads it and the chain ends in
    ## garbage. `id` and `parent` in the base, and the value in the subclass,
    ## is what keeps one offset right for every node whatever the value is.
    ##
    ## Nothing writes a node after `push` builds it, so a chain is safe to
    ## share: a coroutine that inherited one and the coroutine it inherited it
    ## from can both walk it at the same time.
    id*: int
    parent*: CtxNode

  CtxBinding*[T] = ref object of CtxNode
    ## One binding, with the value in the node itself rather than a box behind
    ## it. A `ref T` box is the obvious other choice and does not work: `new`
    ## on a `ref string` in this compiler emits a destructor that runs the
    ## string's destructor on the reference. Keeping `val` inline also means a
    ## binding is one allocation instead of two, and the node being a real
    ## object is what roots a `string` bound for the life of a coroutine.
    val*: T

  ContextVar*[T] = object
    ## A dynamically scoped variable of type `T`. Declare one at module scope
    ## and use it from anywhere; two `ContextVar`s are distinct variables even
    ## when their `T` agrees, told apart by `id`.
    ##
    ## `id` is claimed from a process-wide counter by `newContextVar`. A
    ## `ContextVar` written as a bare `var` claims none and keeps `id == 0`,
    ## which works as an identity of its own -- enough while exactly one such
    ## variable exists, which is why a bare `var` runs. The first `set` gives
    ## it a real one, under `claimLock`, so two threads racing to `set` the
    ## same bare `var` settle on one identity rather than claiming two and
    ## stranding one thread's binding under an id the variable no longer
    ## answers to. Prefer `newContextVar` unless the bare form reads better.
    id*: int
    hasDefault*: bool
      ## Whether `get` answers `defVal` instead of raising.
    defVal*: T

var ctxVarSeq: int
  ## Backs `claimId`. Process-wide and increasing, so two variables built on
  ## two threads at once can never claim the same `id`.

var claimLock: TicketLock
  ## Serialises the check-and-claim of a bare `var`'s `id` in `ensureId`.
  ## The critical section is a handful of instructions and is never held
  ## across a call, so a ticket lock -- no init, no deinit -- is the right
  ## shape for it.

proc claimId*(): int {.inline.} =
  ## Claim an identity for a `ContextVar`. Atomic because a `ContextVar` may be
  ## built at run time on any thread -- a per-request one, say -- and two
  ## claimants must not land on the same `id`.
  atomicFetchAdd(ctxVarSeq, 1, moRelaxed) + 1

proc newContextVar*[T](): ContextVar[T] =
  ## A variable with no value until something `set`s one: `get` raises
  ## `KeyError` until then.
  ContextVar[T](id: claimId())

proc newContextVar*[T](default: T): ContextVar[T] =
  ## A variable whose `get` answers `default` until something `set`s a value.
  ## Distinct from `getOrDefault`, whose answer belongs to the call rather than
  ## to the variable.
  ContextVar[T](id: claimId(), hasDefault: true, defVal: default)

proc ensureId[T](v: var ContextVar[T]): int {.inline.} =
  ## `v`'s identity, claiming one for a bare `var` on first write.
  ##
  ## The lock covers the whole check-and-claim, not just the `claimId` call:
  ## two threads that both find `id == 0` outside it would both claim, and the
  ## second write would strand the first thread's binding under an identity
  ## the variable no longer answers to. The id is read back inside the lock
  ## and returned by value, so the `push` that follows binds the identity the
  ## variable settled on rather than re-reading a field another thread may
  ## still be writing.
  if v.id == 0:
    acquire(claimLock)
    if v.id == 0:
      v.id = claimId()
    release(claimLock)
  result = v.id

# --- The chain ---

proc ctxSlot*(): ptr RootRef =
  ## The address of the chain head belonging to the running code.
  ##
  ## Outside a coroutine that is `system.threadCtx` — the thread's own chain,
  ## because active code cannot park. Inside one it is the frame's `ctx`
  ## slot: the spawn handed it the caller's chain head when the frame was
  ## built, so the chain follows the coroutine from creation, across parks,
  ## and across hops to other threads. Nothing is discovered here; the
  ## compiler picked the answer at the call site, which is also why a `set`
  ## in the caller after the call does not reach back into the callee.
  ##
  ## A frame with no chain of its own — a `Join`, which is a trampoline
  ## marker rather than a coroutine — adopts the thread's, which is what a
  ## caller-walk used to find for it: both read as "the thread's chain" at
  ## run time. Adopting into the frame rather than reading through to
  ## `threadCtx` each time is what keeps a later `set` inside that frame off
  ## the thread's chain — two coroutines sharing a slot must not be able to
  ## see each other's bindings.
  ##
  ## An address rather than a value, so that a `withCtx` can hold on to the one
  ## slot its block pushed onto and put the old head back into exactly that
  ## slot, without resolving the owner a second time. `RootRef` for both the
  ## thread's chain and the frame field, so the two addresses have one type:
  ## `addr` of a `CtxNode` and `addr` of a `RootRef` are not the same type and
  ## neither converts to the other.
  let coro = currentCoroutine()
  if coro == nil:
    result = addr threadCtx
  elif coro.ctx != nil:
    result = addr coro.ctx
  else:
    coro.ctx = threadCtx
    result = addr coro.ctx

proc currentCtx*(): CtxNode {.inline.} =
  ## The head of the chain the running code reads and extends.
  cast[CtxNode](ctxSlot()[])

proc setCurrentCtx*(head: CtxNode) {.inline.} =
  ## Puts `head` at the front of the running code's chain.
  ctxSlot()[] = head

# --- Reading and writing ---

proc findNode(head: CtxNode; id: int): CtxNode =
  ## The node for `id` in `head`'s chain, innermost first, or nil when the
  ## chain has none. No type involved: `id` alone decides which node it is,
  ## and a node whose `id` matched is a `CtxBinding` of the `T` that asked.
  var n = head
  while n != nil:
    if n.id == id: break
    n = n.parent
  result = n

proc push[T](id: int; val: T) =
  ## Extends the running code's chain with a binding for `id`.
  ##
  ## The new node is the only thing that changes. The chain it extends is left
  ## exactly as it was, so the coroutine that set the value and every coroutine
  ## sharing that chain still read what they read before -- which is why a
  ## `set` in a `.passive` proc is invisible to whoever called it, with no
  ## unwinding to arrange.
  let n = CtxBinding[T](id: id, val: val, parent: cast[CtxNode](ctxSlot()[]))
  ctxSlot()[] = cast[RootRef](n)

proc get*[T](v: ContextVar[T]): T {.raises.} =
  ## The value of `v` for the running code: the nearest binding outwards in its
  ## chain -- the one `set` most recently pushed on it -- or `v`'s own default.
  ## Raises `KeyError` when `v` is bound nowhere in the chain and has no
  ## default.
  let n = findNode(currentCtx(), v.id)
  if n != nil:
    result = cast[CtxBinding[T]](n).val
  elif v.hasDefault:
    result = v.defVal
  else:
    raise KeyError

proc getOrDefault*[T](v: ContextVar[T]; dflt: T): T =
  ## The value of `v` for the running code, or `dflt` when it is bound nowhere.
  ## `v`'s own default is *not* consulted: the argument is what the caller
  ## asked for.
  let n = findNode(currentCtx(), v.id)
  if n != nil:
    result = cast[CtxBinding[T]](n).val
  else:
    result = dflt

proc isSet*[T](v: ContextVar[T]): bool =
  ## Whether `v` is bound in the running code's chain. Says nothing about a
  ## default: a variable with one always has a value as far as `get` is
  ## concerned.
  findNode(currentCtx(), v.id) != nil

template set*[T](v: var ContextVar[T]; val: T) =
  ## Binds `v` to `val` for everything the running proc calls, directly or
  ## through a suspension.
  ##
  ## A `.passive` caller does not see the binding: it is a different coroutine
  ## with a chain of its own, so the value cannot leak back out the way it came
  ## in. A *regular* proc is not a coroutine and has no such boundary, so a
  ## `set` in one lasts to the end of the thread's chain -- mark it `{.passive.}`
  ## when the binding is meant to stay local.
  let slot = ctxSlot()
  let ctxBefore = cast[CtxNode](slot[])
  push(ensureId(v), val)
  defer: slot[] = ctxBefore

template withCtx*[T](v: var ContextVar[T]; val: T; body: untyped): untyped =
  ## Runs `body` with `v` bound to `val`, then puts the previous chain back.
  ##
  ## The save and the restore are ordinary locals of the enclosing proc, which
  ## for a `.passive` proc means they live in its coroutine frame: a park inside
  ## `body` may resume on another thread, and the restore still lands on the
  ## chain the push was made from.
  block:
    v.set(val)
    body

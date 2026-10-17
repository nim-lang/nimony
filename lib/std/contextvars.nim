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
# `CoroutineBase.ctx`, and `ctxSlot()` — the compiler primitive this module is
# built on — is the address of that slot: inside a `.passive` proc the
# compiler lowers it to `addr (*this).ctx`, the slot of the frame the running
# code owns. A context crosses from one `.passive` proc to another through the
# hidden `ctx` argument the compiler passes at every spawn, so a coroutine
# holds the chain it was spawned in from the start. A coroutine spawned from a
# regular proc starts with an empty slot.
#
# Using this module — or `ctxSlot()` directly — outside a `.passive` proc is a
# compile error. There is no thread-wide fallback: a regular proc owns no
# frame, so there is no chain for it to read or extend. `doc/contextvars.md`
# has the whole story.

{.feature: "lenientnils".}
## Load-bearing. A chain ends in a nil `parent`, and the nil checks that say so
## are the whole algorithm, so the module's own node types have to be allowed
## nil.

{.feature: "untyped".}
## Load-bearing too. The public entry points are templates whose first act is
## to call `ctxSlot()`, and that call has to be semchecked where it lands -- a
## `.passive` proc -- rather than here, where there is no frame to name. The
## feature defers a template's body to its instantiation sites, so the same
## `get` is fine in a `.passive` proc and rejected in a regular one.

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

proc ctxSlot*(): ptr RootRef {.magic: "CtxSlot".}
  ## The address of the chain head belonging to the running code.
  ##
  ## A compiler primitive, legal only inside a `.passive` proc: the compiler
  ## lowers it to `addr (*this).ctx`, the `ctx` slot of the frame the running
  ## code owns. The spawn that built the frame filled that slot from the
  ## caller's own chain head, so the chain follows the coroutine from creation,
  ## across parks, and across hops to other threads. Nothing is discovered at
  ## run time; the compiler picked the answer at the call site, which is also
  ## why a `set` in a `.passive` caller after the call does not reach back into
  ## the callee.
  ##
  ## An address rather than a value, so that a `withCtx` can hold on to the one
  ## slot its block pushed onto and put the old head back into exactly that
  ## slot, without resolving the owner a second time. `RootRef` because that is
  ## the slot's type: every owner is a `CoroutineBase`, whose `ctx` field is a
  ## `RootRef`.

proc currentCtx(slot: ptr RootRef): CtxNode {.inline.} =
  ## The head of the chain in `slot`'s frame.
  cast[CtxNode](slot[])

proc findNode(head: CtxNode; id: int): CtxNode =
  ## The node for `id` in `head`'s chain, innermost first, or nil when the
  ## chain has none. No type involved: `id` alone decides which node it is,
  ## and a node whose `id` matched is a `CtxBinding` of the `T` that asked.
  var n = head
  while n != nil:
    if n.id == id: break
    n = n.parent
  result = n

proc push[T](head: CtxNode; ident: int; value: T): CtxNode {.inline.} =
  ## Extends `head`'s chain with a binding for `ident` and returns the new head.
  ##
  ## The new node is the only thing that changes. The chain it extends is left
  ## exactly as it was, so the coroutine that set the value and every coroutine
  ## sharing that chain still read what they read before -- which is why a
  ## `set` in a `.passive` proc is invisible to whoever called it, with no
  ## unwinding to arrange.
  ##
  ## The parameters deliberately do not reuse the field names `id`/`val`: a
  ## parameter in scope shadows the constructor's field key, and `id` and
  ## `parent` live in `CtxNode`, so a shadowed key would be placed against the
  ## wrong base and the C backend would not find it.
  result = cast[CtxNode](CtxBinding[T](id: ident, val: value, parent: head))

# --- Reading and writing ---
#
# The public entry points are templates, so that `ctxSlot()` is lowered in
# whoever called them: the primitive is legal only inside a `.passive` proc,
# and a `get` written in a regular proc has to be rejected at that proc, not
# hidden inside this module. The templates take the slot and hand it to the
# plain procs below, which carry the whole algorithm.

proc getImpl[T](slot: ptr RootRef; v: ContextVar[T]): T {.raises.} =
  let n = findNode(currentCtx(slot), v.id)
  if n != nil:
    result = cast[CtxBinding[T]](n).val
  elif v.hasDefault:
    result = v.defVal
  else:
    raise KeyError

template get*[T](v: ContextVar[T]): T =
  ## The value of `v` for the running code: the nearest binding outwards in its
  ## chain -- the one `set` most recently pushed on it -- or `v`'s own default.
  ## Raises `KeyError` when `v` is bound nowhere in the chain and has no
  ## default.
  getImpl(ctxSlot(), v)

proc getOrDefaultImpl[T](slot: ptr RootRef; v: ContextVar[T]; dflt: T): T =
  let n = findNode(currentCtx(slot), v.id)
  if n != nil:
    result = cast[CtxBinding[T]](n).val
  else:
    result = dflt

template getOrDefault*[T](v: ContextVar[T]; dflt: T): T =
  ## The value of `v` for the running code, or `dflt` when it is bound nowhere.
  ## `v`'s own default is *not* consulted: the argument is what the caller
  ## asked for.
  getOrDefaultImpl(ctxSlot(), v, dflt)

proc isSetImpl[T](slot: ptr RootRef; v: ContextVar[T]): bool {.inline.} =
  findNode(currentCtx(slot), v.id) != nil

template isSet*[T](v: ContextVar[T]): bool =
  ## Whether `v` is bound in the running code's chain. Says nothing about a
  ## default: a variable with one always has a value as far as `get` is
  ## concerned.
  isSetImpl(ctxSlot(), v)

template set*[T](v: var ContextVar[T]; val: T) =
  ## Binds `v` to `val` for everything the running proc calls, directly or
  ## through a suspension.
  ##
  ## A `.passive` caller does not see the binding: it is a different coroutine
  ## with a frame and a chain of its own, so the value cannot leak back out the
  ## way it came in. There is no thread-wide chain to leak into.
  let slot = ctxSlot()
  let ctxBefore = cast[CtxNode](slot[])
  slot[] = cast[RootRef](push(ctxBefore, ensureId(v), val))
  defer: slot[] = cast[RootRef](ctxBefore)

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

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
# coroutine, not of the thread. The chain head therefore lives in an
# `ExtSlot`, and `currentCoroutine` -- the frame of the coroutine this thread is
# running, which `runStep` installs around every continuation step -- is how
# the running code reaches the slot. Which slot is another matter, and nothing
# in a program says: a `.passive` proc that WRITES a context gets a slot of its
# own, which the compiler works out from this module's `ctxSlot` appearing in
# the body, and one that only reads shares the nearest enclosing slot.
# `doc/contextvars.md` has the whole story.

{.feature: "lenientnils".}
## Load-bearing. A chain ends in a nil `parent`, and the nil checks that say so
## are the whole algorithm, so the module's own node types have to be allowed
## nil.

import std/atomics

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
    ## it a real one. Prefer `newContextVar` unless the bare form reads better.
    id*: int
    hasDefault*: bool
      ## Whether `get` answers `defVal` instead of raising.
    defVal*: T

var ctxVarSeq: int
  ## Backs `claimId`. Process-wide and increasing, so two variables built on
  ## two threads at once can never claim the same `id`.

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

# --- The chain ---

proc ctxSlot*(): ptr ExtSlot =
  ## The address of the slot holding the chain the running code reads and
  ## extends. The seam between the module and the runtime: everything that
  ## touches a chain goes through here, which is what lets the compiler decide
  ## on its own that a `.passive` proc writes one (see `bodyWritesContext` in
  ## `src/hexer/coro_transform.nim`).
  ##
  ## Three answers, in order. Inside a coroutine that writes a context -- whose
  ## frame the compiler gave an `ext` field for exactly that reason -- its own
  ## slot, adopted on first use from whatever its caller was holding. Else the
  ## nearest enclosing slot, walking `caller`: a coroutine that only READS has
  ## no slot of its own and shares the context around it, the way a helper that
  ## does not care about its own state sees its caller's. Else the thread's own
  ## slot, for the top-level statements and a regular proc no `.passive` one
  ## called.
  ##
  ## Asking the frame rather than keeping a table means no bookkeeping: the
  ## frame is reachable from the continuation that resumes it, so a chain
  ## follows its coroutine across a park and across a hop to another thread
  ## with nothing installed around the step but `runningCoro` itself.
  ##
  ## An address rather than a value, so that a `withCtx` can hold on to the one
  ## slot its block pushed onto and put the old head back into exactly that
  ## slot, without resolving the owner a second time.
  let coro = currentCoroutine()
  if coro != nil:
    let own = coro.extSlot()
    if own != nil:
      # A slot of its own, but empty until asked for something. What to start
      # from is the caller's context AT THIS MOMENT, which is why adoption
      # happens here rather than at the call.
      adoptExtSlot(own, coro.caller.env)
      return own
    # No slot of its own, so the nearest enclosing one: `caller` names a `Join`
    # for a coroutine started by `complete` or an iterator, and a `Join` does
    # have a slot, so the walk lands on the waiting side's own context rather
    # than skipping past it to whatever called *that*. A holder that never
    # adopted a context has no box to hand out, so the walk passes it by, and
    # the one it lands on may still be empty -- `withCtx` puts an empty head
    # back when it pops a block that had none -- so it adopts there too.
    var up = coro.caller.env
    while up != nil:
      let slot = up.extSlot()
      if slot != nil and slot[] != nil:
        adoptExtSlot(slot, up.caller.env)
        return slot
      up = up.caller.env
  currentSlot()

proc currentCtx*(): CtxNode {.inline.} =
  ## The head of the chain the running code reads and extends.
  cast[CtxNode](ctxSlot()[].ctx)

template setCurrentCtx*(head: CtxNode) =
  ## Puts `head` at the front of the running code's chain.
  ##
  ## A template, not a proc, and that is the whole reason: this is a WRITE, and
  ## the compiler grants a `.passive` frame a context slot of its own when its
  ## body reaches `ctxSlot` (the only way this module touches a chain). Written
  ## as a proc, a `setCurrentCtx` in a `.passive` proc would put a chain in
  ## whatever slot happened to be around it -- the caller's -- and a `set` that
  ## the caller can see is exactly what this module exists to prevent.
  ctxSlot()[].ctx = head

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
  var n: CtxBinding[T]
  new(n)
  n.id = id
  n.val = val
  n.parent = cast[CtxNode](ctxSlot()[].ctx)
  ctxSlot()[].ctx = cast[RootRef](n)

proc get*[T](v: ContextVar[T]): T {.raises.} =
  ## The value of `v` for the running code: the nearest binding outwards in its
  ## chain -- the one `set` most recently pushed on it -- or `v`'s own default.
  ## Raises `KeyError` when `v` is bound nowhere in the chain and has no
  ## default.
  let n = findNode(currentCtx(), v.id)
  if n != nil:
    return cast[CtxBinding[T]](n).val
  if v.hasDefault:
    return v.defVal
  raise KeyError

proc getOrDefault*[T](v: ContextVar[T]; dflt: T): T =
  ## The value of `v` for the running code, or `dflt` when it is bound nowhere.
  ## `v`'s own default is *not* consulted: the argument is what the caller
  ## asked for.
  let n = findNode(currentCtx(), v.id)
  if n != nil:
    return cast[CtxBinding[T]](n).val
  dflt

proc isSet*[T](v: ContextVar[T]): bool =
  ## Whether `v` is bound in the running code's chain. Says nothing about a
  ## default: a variable with one always has a value as far as `get` is
  ## concerned.
  findNode(currentCtx(), v.id) != nil

template set*[T](v: var ContextVar[T]; val: T) =
  ## Binds `v` to `val` for everything the running proc calls, directly or
  ## through a suspension.
  ##
  ## A `.passive` caller does not see the binding: a `.passive` proc whose body
  ## reaches this template is one that writes a context, so the compiler gave
  ## its frame a slot of its own, and the value cannot leak back out the way it
  ## came in. A *regular* proc is not a coroutine and has no such boundary, so a
  ## `set` in one lasts to the end of the thread's chain -- mark the caller
  ## `{.passive.}` when the binding is meant to stay local.
  let slot = ctxSlot()
  let ctxBefore = cast[CtxNode](slot[].ctx)
  if v.id == 0:
    v.id = claimId() # a bare `var`, first write: give it an identity of its own
  push(v.id, val)
  defer: slot[].ctx = ctxBefore

template withCtx*[T](v: var ContextVar[T]; val: T; body: untyped): untyped =
  ## Runs `body` with `v` bound to `val`, then puts the previous chain back.
  ##
  ## The save and the restore are ordinary locals of the enclosing proc, which
  ## for a `.passive` proc means they live in its coroutine frame: a park inside
  ## `body` may resume on another thread, and the restore still lands on the
  ## chain the push was made from.
  ##
  ## A write, so it reaches `ctxSlot` through `set` above and the frame gets a
  ## slot of its own for the same reason.
  block:
    v.set(val)
    body

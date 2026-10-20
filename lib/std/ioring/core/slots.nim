# Slot allocator for OpContext slots.
#
# Every in-flight op lives in its own heap block, allocated when it is
# submitted and freed when it completes, so a ring's footprint follows its
# load. A slot is named by a small integer index (what the backends hand the
# kernel as user data and what the timer heap records); the index table and
# the freelist of indices are the only per-lane state that persists. Plus an
# O(1) index from `fd` to the (intrusive, doubly-linked) list of in-flight
# slots for that fd.

import ./types
import std/[tables, assertions]

type
  Slot* = object
    op*: OpContext
    gen*: uint32
      ## The generation this slot was allocated in; see `SlotArena.gens`.
    cancelRes*: int
      ## Non-zero once the ring has stopped waiting for an op the kernel still
      ## owns (its deadline passed, or its fd was closed): the result to
      ## report, `IoTimedOut` or `ECancelled`, when the kernel gives it back.
      ## Until then the slot, and the caller's buffer, remain the kernel's.
    # doubly-linked list of every in-flight slot that shares `fd`,
    # so a readiness event can find "all ops for this fd" in O(k) (k = ops on
    # this fd) instead of scanning every slot. -1 means "no neighbour".
    nextInFd*: int
    prevInFd*: int
  SlotArena* = object
    slots: seq[nil ptr Slot]
      ## `nil` while the index is free. The blocks themselves never move, so
      ## the addresses a backend hands the kernel (an accept's `sockAddr`, an
      ## io_uring msghdr) stay valid however many ops are in flight.
    gens: seq[uint32]
      ## Bumped every time an index is freed, and kept while the block is
      ## gone. A timer entry naming this index also names the generation it
      ## was armed for, so an entry left behind by an op that completed
      ## normally is recognised as stale instead of expiring whatever op
      ## happens to hold the index now.
    freelist: seq[int]
    fdHeads: Table[cint, int]     ## fd -> head slot index of its op list

proc init*(a: var SlotArena) =
  a.fdHeads = initTable[cint, int]()

func slot*(a: var SlotArena; idx: int): ptr Slot {.inline.} =
  ## The slot at `idx`, which must be in use.
  let p = a.slots[idx]
  if p != nil:
    result = p
  else:
    {.cast(noSideEffect).}:
      panic "ioring/slots: access to a free slot"

func op*(a: var SlotArena; idx: int): ptr OpContext {.inline.} =
  ## The op held by the slot at `idx`, which must be in use.
  let p = a.slot(idx)
  result = addr p.op

func isLive*(a: var SlotArena; idx: int; gen: uint32): bool {.inline.} =
  ## Does `idx` still hold the op of generation `gen`?
  result = a.slots[idx] != nil and a.gens[idx] == gen

proc allocSlot*(a: var SlotArena, op: OpContext): int =
  ## The index of a fresh slot holding `op`, or -1 when memory is exhausted.
  let p = cast[nil ptr Slot](alloc(sizeof(Slot)))
  if p == nil: return -1
  var idx: int
  if a.freelist.len > 0:
    idx = a.freelist.pop()
  else:
    idx = a.slots.len
    a.slots.add nil
    a.gens.add 0'u32
  p[] = Slot(op: op, gen: a.gens[idx], prevInFd: -1, nextInFd: -1)
  a.slots[idx] = p
  let head = a.fdHeads.getOrDefault(op.fd, -1)
  if head >= 0:
    a.slot(head).prevInFd = idx
  p.nextInFd = head
  a.fdHeads[op.fd] = idx
  result = idx

proc freeSlot*(a: var SlotArena; idx: int) =
  # Freeing a slot twice is the one corruption this arena cannot survive: the
  # index lands on the freelist twice, two later ops are handed the SAME slot,
  # and from there `fdHeads`/`nextInFd` describe a list that does not exist —
  # which surfaces far away as a bound check inside `slotsForFd`. Cheap to
  # check, and it names the bug where it happens instead of where it lands.
  assert a.slots[idx] != nil, "ioring/slots: freeSlot on a slot that is already free"
  let p = a.slot(idx)
  let fd = p.op.fd
  let prev = p.prevInFd
  let next = p.nextInFd
  if prev >= 0:
    a.slot(prev).nextInFd = next
  elif next >= 0:
    a.fdHeads[fd] = next
  else:
    a.fdHeads.del(fd)
  if next >= 0:
    a.slot(next).prevInFd = prev
  inc a.gens[idx]
  a.slots[idx] = nil
  dealloc(p)
  a.freelist.add(idx)

proc hasPendingForFd*(a: var SlotArena; fd: cint): bool =
  ## `var`, like `slotsForFd`, purely to avoid copying the arena — see there.
  result = a.fdHeads.getOrDefault(fd, -1) >= 0

iterator slotsForFd*(a: var SlotArena; fd: cint): int =
  ## Yield every in-use slot index for `fd`, O(k) in the number of ops on
  ## this fd rather than O(ops in flight).
  ##
  ## `a` is `var` even though nothing here writes to it: the callers all pass a
  ## seq element (`gSlots[lane].slotsForFd(...)`), and for a non-`var` parameter
  ## that materialises a *copy* of the arena's tables on every call. `var`
  ## passes the arena itself.
  ##
  ## The body may free the slot it was handed (`complete`/`closeFd` both do),
  ## so the successor is read *before* yielding — reading `a.slot(cur)` after
  ## the body ran would inspect a slot that is back on the freelist.
  var cur = a.fdHeads.getOrDefault(fd, -1)
  while cur >= 0:
    let nxt = a.slot(cur).nextInFd
    yield cur
    cur = nxt

iterator pendingFds*(a: var SlotArena): cint =
  ## Every fd that has at least one in-flight slot. The table is mutated by
  ## `freeSlot`, so a caller that completes ops must collect the fds first and
  ## dispatch afterwards — never `complete` from inside this loop.
  for fd in a.fdHeads.keys:
    yield fd

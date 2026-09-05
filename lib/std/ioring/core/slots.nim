# Slot allocator for OpContext slots.
#
# Pre-allocated array with a LIFO freelist, plus an O(1) index from `fd` to
# the (intrusive, doubly-linked) list of in-flight slots for that fd.

import ./types
import std/[tables, assertions]

type
  Slot* = object
    op*: OpContext
    gen*: uint32
      ## Bumped every time the slot is freed. A timer entry naming this slot
      ## also names the generation it was armed for, so an entry left behind
      ## by an op that completed normally is recognised as stale instead of
      ## expiring whatever op happens to be in the slot now.
    inUse*: bool
    # doubly-linked list of every in-flight slot that shares `fd`,
    # so a readiness event can find "all ops for this fd" in O(k) (k = ops on
    # this fd) instead of scanning the whole arena. -1 means "no neighbour".
    nextInFd*: int
    prevInFd*: int
  SlotArena* = object
    slots*: seq[Slot]
    freelist: seq[int]
    fdHeads: Table[cint, int]     ## fd -> head slot index of its op list

proc init*(a: var SlotArena, capacity: int) =
  a.slots = newSeq[Slot](capacity)
  a.freelist = newSeqOfCap[int](capacity)
  for i in countdown(capacity - 1, 0):
    a.freelist.add(i)
  a.fdHeads = initTable[cint, int]()

proc allocSlot*(a: var SlotArena, op: OpContext): int =
  var idx: int
  if a.freelist.len > 0:
    idx = a.freelist.pop()
  else:
    # NOTE: growing `slots` reallocates it, invalidating every pointer into the
    # arena. The io_uring backend hands `addr slots[idx].op.sockAddr` to the
    # kernel, so this must stay a cold path: `MaxOps` is sized to cover the
    # in-flight ceiling and the freelist normally satisfies every request.
    idx = a.slots.len
    a.slots.add(Slot())
  let gen = a.slots[idx].gen
  a.slots[idx] = Slot(op: op, gen: gen, inUse: true, prevInFd: -1, nextInFd: -1)
  let head = a.fdHeads.getOrDefault(op.fd, -1)
  if head >= 0:
    a.slots[head].prevInFd = idx
  a.slots[idx].nextInFd = head
  a.fdHeads[op.fd] = idx
  return idx

proc freeSlot*(a: var SlotArena; idx: int) =
  # Freeing a slot twice is the one corruption this arena cannot survive: the
  # index lands on the freelist twice, two later ops are handed the SAME slot,
  # and from there `fdHeads`/`nextInFd` describe a list that does not exist —
  # which surfaces far away as a bound check inside `slotsForFd`. Cheap to
  # check, and it names the bug where it happens instead of where it lands.
  assert a.slots[idx].inUse, "ioring/slots: freeSlot on a slot that is already free"
  let fd = a.slots[idx].op.fd
  let prev = a.slots[idx].prevInFd
  let next = a.slots[idx].nextInFd
  if prev >= 0:
    a.slots[prev].nextInFd = next
  elif next >= 0:
    a.fdHeads[fd] = next
  else:
    a.fdHeads.del(fd)
  if next >= 0:
    a.slots[next].prevInFd = prev
  # Reset to the *unlinked* state, not to `Slot()`: the default `int` is 0,
  # which is a valid slot index, so a zeroed `nextInFd` would make the freed
  # slot look like it still points at slot 0. `slotsForFd` walks these links
  # while its body frees slots, so "no neighbour" must stay -1.
  a.slots[idx] = Slot(gen: a.slots[idx].gen + 1'u32, inUse: false,
                      prevInFd: -1, nextInFd: -1)
  a.freelist.add(idx)

proc hasPendingForFd*(a: var SlotArena; fd: cint): bool =
  ## `var`, like `slotsForFd`, purely to avoid copying the arena — see there.
  result = a.fdHeads.getOrDefault(fd, -1) >= 0

iterator slotsForFd*(a: var SlotArena; fd: cint): int =
  ## Yield every in-use slot index for `fd`, O(k) in the number of ops on
  ## this fd rather than O(MaxOps).
  ##
  ## `a` is `var` even though nothing here writes to it: the callers all pass a
  ## seq element (`gSlots[lane].slotsForFd(...)`), and for a non-`var` parameter
  ## that materialises a *copy* of the whole arena — `MaxOps` slots, each with a
  ## 128-byte `Sockaddr_storage` inside, plus the `fdHeads` table — on every
  ## call, i.e. megabytes memcpy'd per I/O event. `var` passes the arena itself.
  ##
  ## The body may free the slot it was handed (`complete`/`closeFd` both do),
  ## so the successor is read *before* yielding — reading `a.slots[cur]` after
  ## the body ran would inspect a slot that is back on the freelist.
  var cur = a.fdHeads.getOrDefault(fd, -1)
  while cur >= 0:
    let nxt = a.slots[cur].nextInFd
    yield cur
    cur = nxt

iterator pendingFds*(a: var SlotArena): cint =
  ## Every fd that has at least one in-flight slot. The table is mutated by
  ## `freeSlot`, so a caller that completes ops must collect the fds first and
  ## dispatch afterwards — never `complete` from inside this loop.
  for fd in a.fdHeads.keys:
    yield fd

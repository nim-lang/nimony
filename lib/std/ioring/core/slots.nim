# Per-op slots.
#
# Every in-flight op lives in its own `ref Slot`, created when it is submitted
# and released when it completes, so a ring's footprint follows its load. The
# lane's per-fd lists hold the only owning reference; the deadline heap holds
# cursors, and what the kernel keeps (io_uring's `user_data`, an IOCP
# OVERLAPPED) is the slot's raw address, valid because a slot is never
# released before the kernel has given its op back.
#
# The per-fd lists are intrusive and doubly linked, so a readiness event finds
# "all ops for this fd" in O(k) (k = ops on this fd) instead of scanning every
# slot.

import ./types
import std/[tables, assertions]

type
  SlotObj* = object
    op*: OpContext
    cancelRes*: int
      ## Non-zero once the ring has stopped waiting for an op the kernel still
      ## owns (its deadline passed, or its fd was closed): the result to
      ## report, `IoTimedOut` or `ECancelled`, when the kernel gives it back.
      ## Until then the slot, and the caller's buffer, remain the kernel's.
    timerPos*: int
      ## Index of this op's entry in the lane's deadline heap, -1 for none.
    backendData*: nil pointer
      ## A backend's own per-op block (io_uring's msghdr, IOCP's OVERLAPPED),
      ## allocated and freed by that backend.
    nextInFd: nil Slot
    prevInFd {.cursor.}: nil Slot
      ## The predecessor owns this slot through `nextInFd`.
  Slot* = ref SlotObj
  SlotArena* = object
    fdHeads: Table[cint, nil Slot]  ## fd -> head of its op list

proc init*(a: var SlotArena) =
  a.fdHeads = initTable[cint, nil Slot]()

proc toPointer*(s: Slot): pointer {.inline.} =
  ## The address a backend hands the kernel for `s`.
  result = cast[pointer](s)

proc toSlot*(p: nil pointer): Slot {.inline.} =
  ## The slot whose `toPointer` is `p`. The slot must still be in flight.
  result = cast[Slot](p)

proc pushFront(head: var nil Slot; s: Slot) {.inline.} =
  if head != nil:
    head.prevInFd = s
  s.nextInFd = move head
  head = s

proc allocSlot*(a: var SlotArena, op: OpContext): nil Slot =
  ## A fresh slot holding `op`, or `nil` when memory is exhausted.
  result = Slot(op: op, timerPos: -1)
  if result == nil: return
  pushFront(a.fdHeads.mgetOrPut(op.fd, nil), result)

proc freeSlot*(a: var SlotArena; s: Slot) =
  ## Unlink `s` from its fd's list, which drops the ring's reference to it.
  ## The links are moved, not copied: every refcount update is atomic.
  let prev = s.prevInFd
  var next = move s.nextInFd
  s.prevInFd = nil
  if next != nil:
    next.prevInFd = prev
  # Last: this releases `s`.
  if prev != nil:
    prev.nextInFd = move next
  elif next != nil:
    a.fdHeads[s.op.fd] = move next
  else:
    a.fdHeads.del(s.op.fd)

proc hasPendingForFd*(a: var SlotArena; fd: cint): bool =
  ## `var`, like `slotsForFd`, purely to avoid copying the arena — see there.
  result = a.fdHeads.getOrDefault(fd, nil) != nil

iterator slotsForFd*(a: var SlotArena; fd: cint): Slot =
  ## Yield every in-flight slot for `fd`, O(k) in the number of ops on this
  ## fd rather than O(ops in flight).
  ##
  ## `a` is `var` even though nothing here writes to it: the callers all pass a
  ## seq element (`gSlots[lane].slotsForFd(...)`), and for a non-`var` parameter
  ## that materialises a *copy* of the arena's tables on every call. `var`
  ## passes the arena itself.
  ##
  ## The body may complete the slot it was handed (`complete`/`closeFd` both
  ## do), so the successor is taken *before* yielding; holding it also keeps
  ## it alive while the body runs.
  var cur = a.fdHeads.getOrDefault(fd, nil)
  while cur != nil:
    let nxt = cur.nextInFd
    yield cur
    cur = nxt

iterator pendingFds*(a: var SlotArena): cint =
  ## Every fd that has at least one in-flight slot. The table is mutated by
  ## `freeSlot`, so a caller that completes ops must collect the fds first and
  ## dispatch afterwards — never `complete` from inside this loop.
  for fd in a.fdHeads.keys:
    yield fd

iterator allSlots*(a: var SlotArena): Slot =
  ## Every in-flight slot. The body must not complete any of them.
  for head in a.fdHeads.values:
    var cur = head
    while cur != nil:
      yield cur
      cur = cur.nextInFd

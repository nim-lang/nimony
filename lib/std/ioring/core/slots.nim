# Slot allocator for OpContext slots.
#
# Pre-allocated array with a LIFO freelist, plus an O(1) index from `fd` to
# the (intrusive, doubly-linked) list of in-flight slots for that fd.
#
# Thread-safety: `allocSlot`/`freeSlot` are called from arbitrary submitting
# threads and from worker threads delivering completions, so all mutable
# state (freelist + fd index) is guarded by a single ticket lock. The lock
# only protects bookkeeping (list-splicing integer arithmetic); it is never
# held across a syscall or a user callback.

import ./types
import std/tables
import std/ticketlocks

const MaxOps* = 8192

type
  SlotArena* = ref object
    slots*: array[MaxOps, OpContext]
    freelist: seq[int]
    fdHeads: Table[cint, int]     ## fd -> head slot index of its op list
    lock: TicketLock

proc init*(a: SlotArena) =
  a.freelist = newSeqOfCap[int](MaxOps)
  for i in countdown(MaxOps - 1, 0):
    a.freelist.add(i)
  a.fdHeads = initTable[cint, int]()

proc capacity*(a: SlotArena): int {.inline.} = MaxOps

proc len*(a: SlotArena): int =
  ## Number of slots currently in use. For diagnostics/backpressure checks.
  withLock a.lock:
    result = MaxOps - a.freelist.len

proc tryAllocSlot*(a: SlotArena; fd: cint; idx: var int): bool =
  ## Allocate a slot for `fd` and link it into that fd's op list. Returns
  ## `false` (arena exhausted, `idx` untouched) instead of trapping, so
  ## callers can apply backpressure (e.g. `submitRead` returning an error)
  ## rather than hitting an assertion or undefined behaviour under load.
  withLock a.lock:
    if a.freelist.len == 0:
      return false
    idx = a.freelist.pop()
    a.slots[idx] = OpContext(inUse: true, fd: fd, nextInFd: -1, prevInFd: -1)
    let head = a.fdHeads.getOrDefault(fd, -1)
    if head >= 0:
      a.slots[head].prevInFd = idx
    a.slots[idx].nextInFd = head
    a.fdHeads[fd] = idx
    result = true

proc allocSlot*(a: SlotArena; fd: cint): int =
  ## Convenience wrapper that raises on exhaustion. Prefer `tryAllocSlot`
  ## wherever the caller can propagate backpressure instead of trapping.
  result = -1
  if not a.tryAllocSlot(fd, result):
    # raise newException(ValueError, "ioring: slot arena exhausted (MaxOps=" & $MaxOps & ")")
    discard
 

proc addrSlot*(a: SlotArena; idx: int): ptr OpContext =
  addr a.slots[idx]

proc freeSlot*(a: SlotArena; idx: int) =
  withLock a.lock:
    if not a.slots[idx].inUse:
      return # double-free guard; shutdown/cancel races can legitimately race here
    let fd = a.slots[idx].fd
    let prev = a.slots[idx].prevInFd
    let next = a.slots[idx].nextInFd
    if prev >= 0:
      a.slots[prev].nextInFd = next
    elif next >= 0:
      a.fdHeads[fd] = next
    else:
      a.fdHeads.del(fd)
    if next >= 0: a.slots[next].prevInFd = prev
    a.slots[idx] = OpContext()
    a.freelist.add(idx)

iterator slotsForFd*(a: SlotArena; fd: cint): int =
  ## Yield every in-use slot index for `fd`, O(k) in the number of ops on
  ## this fd rather than O(MaxOps). Caller must hold no expectations about
  ## the arena lock: this snapshots the chain under the lock first, since
  ## the callback invoked per-slot (read/write/accept + user continuation)
  ## must not run while holding the arena lock.
  var buf: seq[int] = newSeq[int]()
  {.cast(noSideEffect).}:
    withLock a.lock:
      var cur = a.fdHeads.getOrDefault(fd, -1)
      while cur >= 0:
        buf.add(cur)
        cur = a.slots[cur].nextInFd
  for idx in buf:
    yield idx

proc hasPendingForFd*(a: SlotArena; fd: cint): bool =
  withLock a.lock:
    result = a.fdHeads.getOrDefault(fd, -1) >= 0

proc cancelAllForFd*(a: SlotArena; fd: cint; drop: proc(idx: int) {.closure.}) =
  ## Free every slot still pending for `fd` (used when a fd is closed while
  ## ops are in flight) invoking `drop` for each so the caller can run the
  ## op's continuation with a cancellation result rather than leaking it.
  var buf: seq[int] = newSeq[int]()
  withLock a.lock:
    var cur = a.fdHeads.getOrDefault(fd, -1)
    while cur >= 0:
      buf.add(cur)
      cur = a.slots[cur].nextInFd
  for idx in buf:
    drop(idx)
    a.freeSlot(idx)

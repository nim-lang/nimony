# Slot allocator for OpContext slots.
#
# Pre-allocated array with a LIFO freelist, plus an O(1) index from `fd` to
# the (intrusive, doubly-linked) list of in-flight slots for that fd.

import ./types
import std/tables

type
  Slot* = object
    op*: OpContext
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
    idx = a.slots.len
    a.slots.add(Slot())
  a.slots[idx] = Slot(op: op, prevInFd: -1, nextInFd: -1)
  let head = a.fdHeads.getOrDefault(op.fd, -1)
  if head >= 0:
    a.slots[head].prevInFd = idx
  a.slots[idx].nextInFd = head
  a.fdHeads[op.fd] = idx
  return idx

proc freeSlot*(a: var SlotArena; idx: int) =
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
  a.slots[idx] = Slot()
  a.freelist.add(idx)

proc hasPendingForFd*(a: SlotArena; fd: cint): bool =
  result = a.fdHeads.getOrDefault(fd, -1) >= 0

iterator slotsForFd*(a: SlotArena; fd: cint): int =
  ## Yield every in-use slot index for `fd`, O(k) in the number of ops on
  ## this fd rather than O(MaxOps).
  var cur = a.fdHeads.getOrDefault(fd, -1)
  while cur >= 0:
    yield cur
    cur = a.slots[cur].nextInFd

# Slot allocator for OpContext slots.
# Pre-allocated array with LIFO freelist.

import ./types

const MaxOps* = 8192

type SlotArena* = ref object
  slots*: array[MaxOps, OpContext]
  freelist*: seq[int]

proc init*(a: SlotArena) =
  a.freelist = newSeq[int]()
  for i in 0..<MaxOps:
    a.freelist.add(i)

proc allocSlot*(a: SlotArena): int =
  result = a.freelist.pop()
  a.slots[result].inUse = true

proc addrSlot*(a: SlotArena; idx: int): ptr OpContext =
  addr a.slots[idx]

proc freeSlot*(a: SlotArena; idx: int) =
  a.slots[idx] = OpContext()
  a.freelist.add(idx)

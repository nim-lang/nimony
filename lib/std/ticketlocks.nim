## Ticketlocks for Nimony.
## A ticket lock is a fair lock, ideal for guaranteed worst case execution times (WCET).
## This implementation has the advantage that no `deinit` nor a destructor has to be
## run making it especially convenient to use.

import std / atomics

type
  TicketLock* = object
    nextTicket, nowServing: int

proc acquire*(L: var TicketLock) {.inline.} =
  let myTicket = atomicFetchAdd(L.nextTicket, 1, moRelaxed)
  while true:
    let currentlyServing = atomicLoad(L.nowServing, moAcquire)
    if currentlyServing == myTicket: break
    let previousTicket = myTicket - currentlyServing
    var delaySlots = 30 * previousTicket
    while delaySlots > 0:
      cpuRelax()
      dec delaySlots

proc release*(L: var TicketLock) {.inline.} =
  let myTicket = atomicLoad(L.nowServing, moRelaxed)
  atomicStore(L.nowServing, myTicket + 1, moRelease)

template withLock*(a: TicketLock; body: untyped) =
  ## Acquires the given lock, executes the statements in body and
  ## releases the lock after the statements finished executing.
  acquire(a)
  try:
    body
  finally:
    release(a)
